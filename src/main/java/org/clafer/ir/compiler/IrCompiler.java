package org.clafer.ir.compiler;

import org.clafer.collection.CacheMap;
import org.clafer.ir.IrAllDifferent;
import org.clafer.ir.IrArithm;
import org.clafer.ir.IrElement;
import org.clafer.ir.IrHalfReification;
import org.clafer.ir.IrIfOnlyIf;
import org.clafer.ir.IrIntExpr;
import org.clafer.ir.IrMember;
import org.clafer.ir.IrNotMember;
import org.clafer.ir.IrSelectN;
import org.clafer.ir.IrSetExpr;
import gnu.trove.set.hash.TIntHashSet;
import org.clafer.ir.IrNot;
import org.clafer.ir.IrSetEquality;
import org.clafer.ir.IrSingleton;
import org.clafer.ir.IrSortInts;
import org.clafer.ir.IrSortStrings;
import org.clafer.ir.IrSum;
import org.clafer.ir.IrUnion;
import solver.constraints.nary.cnf.ConjunctiveNormalForm;
import org.clafer.ir.IrAnd;
import org.clafer.ir.IrConstraint;
import org.clafer.Check;
import org.clafer.Util;
import org.clafer.constraint.propagator.PropagatorUtil;
import org.clafer.constraint.Constraints;
import org.clafer.ir.IrBoolChannel;
import org.clafer.ir.IrBoolConstraint;
import org.clafer.ir.IrIntChannel;
import org.clafer.ir.IrJoin;
import org.clafer.ir.IrJoinRef;
import org.clafer.ir.IrCard;
import solver.variables.SetVar;
import solver.constraints.Constraint;
import org.clafer.ir.IrBoolExpr;
import org.clafer.ir.IrBoolExprVisitor;
import org.clafer.ir.IrBoolVar;
import org.clafer.ir.IrConstraintVisitor;
import org.clafer.ir.IrDomain;
import org.clafer.ir.IrException;
import org.clafer.ir.IrImplies;
import org.clafer.ir.IrCompare;
import org.clafer.ir.IrIntExprVisitor;
import org.clafer.ir.IrIntVar;
import org.clafer.ir.IrModule;
import org.clafer.ir.IrSetExprVisitor;
import org.clafer.ir.IrSetVar;
import org.clafer.ir.IrUtil;
import solver.Solver;
import solver.constraints.IntConstraintFactory;
import solver.constraints.nary.Sum;
import solver.constraints.nary.cnf.ALogicTree;
import solver.constraints.nary.cnf.Literal;
import solver.constraints.nary.cnf.Node;
import solver.constraints.nary.cnf.Singleton;
import solver.constraints.set.SetConstraintsFactory;
import solver.variables.BoolVar;
import solver.variables.IntVar;
import solver.variables.VariableFactory;

/**
 * Compile from IR to Choco.
 * 
 * @author jimmy
 */
public class IrCompiler {

    private final Solver solver;
    private int varNum = 0;

    private IrCompiler(Solver solver) {
        this.solver = Check.notNull(solver);
    }

    public static IrSolutionMap compile(IrModule in, Solver out) {
        IrCompiler compiler = new IrCompiler(out);
        return compiler.compile(in);
    }

    private IrSolutionMap compile(IrModule module) {
        for (IrSetVar var : module.getSetVars()) {
            var.accept(setExprCompiler, null);
        }
        for (IrIntVar var : module.getIntVars()) {
            var.accept(intExprCompiler, null);
        }
        for (IrBoolVar var : module.getBoolVars()) {
            var.accept(boolExprCompiler, null);
        }
        for (IrConstraint constraint : module.getConstraints()) {
            solver.post(constraint.accept(constraintCompiler, null));
        }
        return new IrSolutionMap(boolVar, intVar, setVar);
    }

    private BoolVar numBoolVar(String name) {
        return VariableFactory.bool(name + "#" + varNum++, solver);
    }

    private IntVar numIntVar(String name, int low, int high) {
        return VariableFactory.enumerated(name, low, high, solver);
    }

    private IntVar numIntVar(String name, int[] dom) {
        return VariableFactory.enumerated(name, dom, solver);
    }

    private SetVar numSetVar(String name, int low, int high) {
        return VariableFactory.set(name, Util.range(low, high + 1), solver);
    }

    private SetVar numSetVar(String name, int[] env) {
        return VariableFactory.set(name, env, solver);
    }
    private final CacheMap<IrBoolVar, BoolVar> boolVar = new CacheMap<IrBoolVar, BoolVar>() {

        @Override
        protected BoolVar cache(IrBoolVar ir) {
            if (ir.isConstant()) {
                return (BoolVar) VariableFactory.fixed(ir.isTrue() ? 1 : 0, solver);
            }
            return VariableFactory.bool(ir.getName(), solver);
        }
    };
    private final CacheMap<IrIntVar, IntVar> intVar = new CacheMap<IrIntVar, IntVar>() {

        @Override
        protected IntVar cache(IrIntVar ir) {
            IrDomain domain = ir.getDomain();
            Integer constant = domain.getConstant();
            if (constant != null) {
                return VariableFactory.fixed(constant, solver);
            }
//            if (domain instanceof IrEnumDomain && domain.getUpperBound() - domain.getLowerBound() < 100) {
//                return VariableFactory.enumerated(ir.getName(), domain.getValues(), solver);
//            }
            return VariableFactory.bounded(ir.getName(), domain.getLowerBound(), domain.getUpperBound(), solver);
        }
    };
    private final CacheMap<IrSetVar, SetVar> setVar = new CacheMap<IrSetVar, SetVar>() {

        @Override
        protected SetVar cache(IrSetVar a) {
            IrDomain env = a.getEnv();
            IrDomain ker = a.getKer();
            return VariableFactory.set(a.getName(), env.getValues(), ker.getValues(), solver);
        }
    };
    private final CacheMap<SetVar, IntVar> setCardVar = new CacheMap<SetVar, IntVar>() {

        @Override
        protected IntVar cache(SetVar a) {
            IntVar card = VariableFactory.enumerated("|" + a.getName() + "|", a.getKernelSize(), a.getEnvelopeSize(), solver);
            solver.post(SetConstraintsFactory.cardinality(a, card));
            return card;
        }
    };
    private final IrConstraintVisitor<Void, Constraint> constraintCompiler = new IrConstraintVisitor<Void, Constraint>() {

        @Override
        public Constraint visit(IrBoolConstraint ir, Void a) {
            return ir.getExpr().accept(boolExprConstraintCompiler, a);
        }

        @Override
        public Constraint visit(IrHalfReification ir, Void a) {
            BoolVar $antecedent = ir.getAntecedent().accept(boolExprCompiler, a);
            Constraint $consequent = ir.getConsequent().accept(this, a);
            return _implies($antecedent, $consequent);
        }

        @Override
        public Constraint visit(IrBoolChannel ir, Void a) {
            IrBoolExpr[] bools = ir.getBools();
            IrSetExpr set = ir.getSet();

            BoolVar[] $bools = new BoolVar[bools.length];
            for (int i = 0; i < $bools.length; i++) {
                $bools[i] = bools[i].accept(boolExprCompiler, a);
            }
            SetVar $set = set.accept(setExprCompiler, a);
            return SetConstraintsFactory.bool_channel($bools, $set, 0);
        }

        @Override
        public Constraint visit(IrIntChannel ir, Void a) {
            IrIntExpr[] ints = ir.getInts();
            IrSetExpr[] sets = ir.getSets();

            IntVar[] $ints = new IntVar[ints.length];
            for (int i = 0; i < $ints.length; i++) {
                $ints[i] = ints[i].accept(intExprCompiler, a);
            }
            SetVar[] $sets = new SetVar[sets.length];
            for (int i = 0; i < $sets.length; i++) {
                $sets[i] = sets[i].accept(setExprCompiler, a);
            }
            return SetConstraintsFactory.int_channel($sets, $ints, 0, 0);
        }

        @Override
        public Constraint visit(IrSortInts ir, Void a) {
            IrIntExpr[] array = ir.getArray();

            IntVar[] $array = new IntVar[array.length];
            for (int i = 0; i < $array.length; i++) {
                $array[i] = array[i].accept(intExprCompiler, a);
            }
            return Constraints.increasing($array);
        }

        @Override
        public Constraint visit(IrSortStrings ir, Void a) {
            IrIntExpr[][] strings = ir.getStrings();

            IntVar[][] $strings = new IntVar[strings.length][];
            for (int i = 0; i < $strings.length; i++) {
                $strings[i] = new IntVar[strings[i].length];
                for (int j = 0; j < $strings[i].length; j++) {
                    $strings[i][j] = strings[i][j].accept(intExprCompiler, a);
                }
            }
            return _lex_chain_less_eq($strings);
        }

        @Override
        public Constraint visit(IrAllDifferent ir, Void a) {
            IrIntExpr[] operands = ir.getOperands();

            IntVar[] $operands = new IntVar[operands.length];
            for (int i = 0; i < $operands.length; i++) {
                $operands[i] = operands[i].accept(intExprCompiler, a);
            }
            return _all_different($operands);
        }

        @Override
        public Constraint visit(IrSelectN ir, Void a) {
            IrBoolExpr[] bools = ir.getBools();
            IrIntExpr n = ir.getN();

            BoolVar[] $bools = new BoolVar[bools.length];
            for (int i = 0; i < $bools.length; i++) {
                $bools[i] = bools[i].accept(boolExprCompiler, a);
            }
            IntVar $n = n.accept(intExprCompiler, a);

            return Constraints.selectN($bools, $n);
        }
    };
    private final IrBoolExprVisitor<Void, BoolVar> boolExprCompiler = new IrBoolExprVisitor<Void, BoolVar>() {

        @Override
        public BoolVar visit(IrBoolVar ir, Void a) {
            return boolVar.get(ir);
        }

        @Override
        public BoolVar visit(IrNot ir, Void a) {
            return VariableFactory.not(ir.getProposition().accept(this, a));
        }

        @Override
        public BoolVar visit(IrAnd ir, Void a) {
            BoolVar reified = numBoolVar("And");
            solver.post(_clauses(_reified(reified, ir.accept(boolExprTreeCompiler, a))));
            return reified;
        }

        @Override
        public BoolVar visit(IrImplies ir, Void a) {
            BoolVar $antecedent = ir.getAntecedent().accept(this, a);
            BoolVar $consequent = ir.getConsequent().accept(this, a);
            BoolVar reified = numBoolVar("Implies");
            solver.post(_implies(reified, _arithm($antecedent, "<=", $consequent)));
            solver.post(_implies(_not(reified), _arithm($antecedent, ">", $consequent)));
            return reified;
        }

        @Override
        public BoolVar visit(IrIfOnlyIf ir, Void a) {
            BoolVar $left = ir.getLeft().accept(this, a);
            BoolVar $right = ir.getRight().accept(this, a);
            BoolVar reified = numBoolVar("IfOnlyIf");
            solver.post(_implies(reified, _arithm($left, "=", $right)));
            solver.post(_implies(_not(reified), _arithm($left, "!=", $right)));
            return reified;
        }

        @Override
        public BoolVar visit(IrMember ir, Void a) {
            IntVar $var = ir.getVar().accept(intExprCompiler, a);
            BoolVar reified = numBoolVar("Member");
            solver.post(_implies(reified, _member($var, ir.getLow(), ir.getHigh())));
            solver.post(_implies(_not(reified), _not_member($var, ir.getLow(), ir.getHigh())));
            return reified;
        }

        @Override
        public BoolVar visit(IrNotMember ir, Void a) {
            IntVar $var = ir.getVar().accept(intExprCompiler, a);
            BoolVar reified = numBoolVar("NotMember");
            solver.post(_implies(reified, _not_member($var, ir.getLow(), ir.getHigh())));
            solver.post(_implies(_not(reified), _member($var, ir.getLow(), ir.getHigh())));
            return reified;
        }

        @Override
        public BoolVar visit(IrCompare ir, Void a) {
            IntVar $left = ir.getLeft().accept(intExprCompiler, a);
            IntVar $right = ir.getRight().accept(intExprCompiler, a);
            BoolVar reified = numBoolVar("IntCompare");
            solver.post(_implies(reified, _arithm($left, ir.getOp().getSyntax(), $right)));
            solver.post(_implies(_not(reified), _arithm($left, ir.getOp().getOpposite().getSyntax(), $right)));
            return reified;
        }

        @Override
        public BoolVar visit(IrSetEquality ir, Void a) {
            SetVar $left = ir.getLeft().accept(setExprCompiler, a);
            SetVar $right = ir.getRight().accept(setExprCompiler, a);
            BoolVar reified = numBoolVar("SetCompare");
            switch (ir.getOp()) {
                case Equal:
                    solver.post(_implies(reified, _all_equal($left, $right)));
                    solver.post(_implies(_not(reified), _all_different($left, $right)));
                    return reified;
                case NotEqual:
                    solver.post(_implies(reified, _all_different($left, $right)));
                    solver.post(_implies(_not(reified), _all_equal($left, $right)));
                    return reified;
                default:
                    throw new IrException();
            }
        }
    };
    private final IrBoolExprVisitor<Void, Constraint> boolExprConstraintCompiler = new IrBoolExprVisitor<Void, Constraint>() {

        @Override
        public Constraint visit(IrBoolVar ir, Void a) {
            return _arithm(boolVar.get(ir), "=", VariableFactory.fixed(1, solver));
        }

        @Override
        public Constraint visit(IrNot ir, Void a) {
            return _arithm(ir.getProposition().accept(boolExprCompiler, a), "=", VariableFactory.fixed(0, solver));
        }

        @Override
        public Constraint visit(IrAnd ir, Void a) {
            IrBoolExpr[] operands = ir.getOperands();

            BoolVar[] $operands = new BoolVar[operands.length];
            for (int i = 0; i < $operands.length; i++) {
                $operands[i] = operands[i].accept(boolExprCompiler, a);
            }
            if ($operands.length == 2) {
                return _arithm($operands[0], "+", $operands[1], "=", 2);
            }
            return _sum(VariableFactory.fixed($operands.length, solver), $operands);
        }

        @Override
        public Constraint visit(IrImplies ir, Void a) {
            BoolVar $antecedent = ir.getAntecedent().accept(boolExprCompiler, a);
            Constraint $consequent = ir.getConsequent().accept(this, a);
            return _implies($antecedent, $consequent);
        }

        @Override
        public Constraint visit(IrIfOnlyIf ir, Void a) {
            BoolVar $left = ir.getLeft().accept(boolExprCompiler, a);
            BoolVar $right = ir.getRight().accept(boolExprCompiler, a);
            return _arithm($left, "=", $right);
        }

        @Override
        public Constraint visit(IrMember ir, Void a) {
            IntVar $var = ir.getVar().accept(intExprCompiler, a);
            return _member($var, ir.getLow(), ir.getHigh());
        }

        @Override
        public Constraint visit(IrNotMember ir, Void a) {
            IntVar $var = ir.getVar().accept(intExprCompiler, a);
            return _not_member($var, ir.getLow(), ir.getHigh());
        }

        @Override
        public Constraint visit(IrCompare ir, Void a) {
            IntVar $left = ir.getLeft().accept(intExprCompiler, a);
            IntVar $right = ir.getRight().accept(intExprCompiler, a);
            return _arithm($left, ir.getOp().getSyntax(), $right);
        }

        @Override
        public Constraint visit(IrSetEquality ir, Void a) {
            SetVar $left = ir.getLeft().accept(setExprCompiler, a);
            SetVar $right = ir.getRight().accept(setExprCompiler, a);
            switch (ir.getOp()) {
                case Equal:
                    return _all_equal($left, $right);
                case NotEqual:
                    return _all_different($left, $right);
                default:
                    throw new IrException();
            }
        }
    };
    private final IrBoolExprVisitor<Void, ALogicTree> boolExprTreeCompiler = new IrBoolExprVisitor<Void, ALogicTree>() {

        @Override
        public ALogicTree visit(IrBoolVar ir, Void a) {
            if (ir.isConstant()) {
                return ir.isTrue() ? Singleton.TRUE : Singleton.FALSE;
            }
            return Literal.pos(boolVar.get(ir));
        }

        @Override
        public ALogicTree visit(IrNot ir, Void a) {
            return Node.nor(ir.getProposition().accept(this, a));
        }

        @Override
        public ALogicTree visit(IrAnd ir, Void a) {
            IrBoolExpr[] operands = ir.getOperands();

            ALogicTree[] $operands = new ALogicTree[operands.length];
            for (int i = 0; i < operands.length; i++) {
                $operands[i] = operands[i].accept(this, a);
            }
            return _and($operands);
        }

        @Override
        public ALogicTree visit(IrImplies ir, Void a) {
            ALogicTree $antecedent = ir.getAntecedent().accept(this, a);
            ALogicTree $consequent = ir.getConsequent().accept(this, a);
            return Node.implies($antecedent, $consequent);
        }

        @Override
        public ALogicTree visit(IrIfOnlyIf ir, Void a) {
            ALogicTree $left = ir.getLeft().accept(this, a);
            ALogicTree $right = ir.getRight().accept(this, a);
            return Node.ifOnlyIf($left, $right);
        }

        @Override
        public ALogicTree visit(IrMember ir, Void a) {
            return Literal.pos(ir.accept(boolExprCompiler, a));
        }

        @Override
        public ALogicTree visit(IrNotMember ir, Void a) {
            return Literal.pos(ir.accept(boolExprCompiler, a));
        }

        @Override
        public ALogicTree visit(IrCompare ir, Void a) {
            return Literal.pos(ir.accept(boolExprCompiler, a));
        }

        @Override
        public ALogicTree visit(IrSetEquality ir, Void a) {
            return Literal.pos(ir.accept(boolExprCompiler, a));
        }
    };
    private final IrIntExprVisitor<Void, IntVar> intExprCompiler = new IrIntExprVisitor<Void, IntVar>() {

        /**
         * TODO: optimize
         * 
         * 5 = x + y
         * 
         * sum([x,y], newVar)
         * newVar = 5
         * 
         * Instead pass "5" in the Void param so
         * sum([x,y], 5)
         */
        @Override
        public IntVar visit(IrIntVar ir, Void a) {
            return intVar.get(ir);
        }

        @Override
        public IntVar visit(IrCard ir, Void a) {
            return setCardVar.get(ir.getSet().accept(setExprCompiler, a));
        }

        @Override
        public IntVar visit(IrArithm ir, Void a) {
            IrIntExpr left = ir.getLeft();
            IrIntExpr right = ir.getRight();

            switch (ir.getOp()) {
                case Add: {
                    Integer leftConstant = IrUtil.getConstant(left);
                    Integer rightConstant = IrUtil.getConstant(right);
                    if (leftConstant != null && rightConstant != null) {
                        return VariableFactory.fixed(leftConstant.intValue() + rightConstant.intValue(), solver);
                    }
                    if (leftConstant != null) {
                        IntVar $right = right.accept(this, a);
                        return VariableFactory.offset($right, leftConstant.intValue());
                    }
                    if (rightConstant != null) {
                        IntVar $left = left.accept(this, a);
                        return VariableFactory.offset($left, rightConstant.intValue());
                    }
                    IntVar $left = left.accept(this, a);
                    IntVar $right = right.accept(this, a);
                    return _sum($left, $right);
                }
                case Sub: {
                    Integer leftConstant = IrUtil.getConstant(left);
                    Integer rightConstant = IrUtil.getConstant(right);
                    if (leftConstant != null && rightConstant != null) {
                        return VariableFactory.fixed(leftConstant.intValue() - rightConstant.intValue(), solver);
                    }
                    if (leftConstant != null && leftConstant.intValue() == 0) {
                        IntVar $right = right.accept(this, a);
                        return VariableFactory.minus($right);
                    }
                    if (rightConstant != null) {
                        IntVar $left = left.accept(this, a);
                        return VariableFactory.offset($left, -rightConstant.intValue());
                    }
                    IntVar $left = left.accept(this, a);
                    IntVar $right = right.accept(this, a);
                    return _sum($left, VariableFactory.minus($right));
                }
                case Mul: {
                    Integer leftConstant = IrUtil.getConstant(left);
                    Integer rightConstant = IrUtil.getConstant(right);
                    if (leftConstant != null && rightConstant != null) {
                        return VariableFactory.fixed(leftConstant.intValue() * rightConstant.intValue(), solver);
                    }
                    if (leftConstant != null && leftConstant.intValue() > -2) {
                        IntVar $right = right.accept(this, a);
                        return VariableFactory.scale($right, leftConstant.intValue());
                    }
                    if (rightConstant != null && rightConstant.intValue() > -2) {
                        IntVar $left = left.accept(this, a);
                        return VariableFactory.scale($left, rightConstant.intValue());
                    }
                    IntVar $left = left.accept(this, a);
                    IntVar $right = right.accept(this, a);
                    IntVar product = numIntVar("Mul",
                            $left.getLB() * $right.getLB(),
                            $left.getUB() * $right.getUB());
                    solver.post(_times(product, $left, $right));
                    return product;
                }
                case Div: {
                    IntVar $left = left.accept(this, a);
                    IntVar $right = right.accept(this, a);
                    IntVar quotient = numIntVar("Div",
                            $left.getLB() / $right.getUB(),
                            $left.getUB() / $right.getLB());
                    solver.post(IntConstraintFactory.eucl_div($left, $right, quotient));
                    return quotient;
                }
            }
            throw new IrException();
        }

        @Override
        public IntVar visit(IrSum ir, Void a) {
            IrIntExpr[] addends = ir.getAddends();

            IntVar[] $addends = new IntVar[addends.length];
            for (int i = 0; i < $addends.length; i++) {
                $addends[i] = addends[i].accept(this, a);
            }
            int[] bounds = Sum.getSumBounds($addends);
            IntVar sum = numIntVar("Sum", bounds[0], bounds[1]);
            solver.post(_sum(sum, $addends));
            return sum;
        }

        @Override
        public IntVar visit(IrElement ir, Void a) {
            IrIntExpr index = ir.getIndex();
            IrIntExpr[] array = ir.getArray();

            IntVar $index = index.accept(intExprCompiler, a);
            IntVar[] $array = new IntVar[array.length];
            for (int i = 0; i < $array.length; i++) {
                $array[i] = array[i].accept(intExprCompiler, a);
            }
            IntVar element = numIntVar("Element", getLB($array), getUB($array));
            solver.post(_element($index, $array, element));
            return element;
        }
    };
    private final IrSetExprVisitor<Void, SetVar> setExprCompiler = new IrSetExprVisitor<Void, SetVar>() {

        @Override
        public SetVar visit(IrSetVar ir, Void a) {
            return setVar.get(ir);
        }

        @Override
        public SetVar visit(IrSingleton ir, Void a) {
            IrIntExpr value = ir.getValue();

            IntVar $value = value.accept(intExprCompiler, a);
            SetVar singleton = numSetVar("Singleton", $value.getLB(), $value.getUB());
            solver.post(Constraints.singleton($value, singleton));
            return singleton;
        }

        @Override
        public SetVar visit(IrJoin ir, Void a) {
            IrSetExpr take = ir.getTake();
            IrSetExpr[] children = ir.getChildren();

            TIntHashSet env = new TIntHashSet();
            SetVar $take = take.accept(this, a);
            SetVar[] $children = new SetVar[children.length];
            for (int i = 0; i < $children.length; i++) {
                $children[i] = children[i].accept(this, a);
                PropagatorUtil.iterateEnv($children[i], env);
            }
            SetVar join = numSetVar("Join", env.toArray());
            solver.post(Constraints.join($take, $children, join));
            return join;
        }

        @Override
        public SetVar visit(IrJoinRef ir, Void a) {
            throw new UnsupportedOperationException("Not supported yet.");
        }

        @Override
        public SetVar visit(IrUnion ir, Void a) {
            IrSetExpr[] operands = ir.getOperands();

            TIntHashSet env = new TIntHashSet();
            SetVar[] $operands = new SetVar[operands.length];
            for (int i = 0; i < operands.length; i++) {
                $operands[i] = operands[i].accept(setExprCompiler, a);
                PropagatorUtil.iterateEnv($operands[i], env);
            }
            SetVar union = numSetVar("Union", env.toArray());
            solver.post(_union($operands, union));
            return union;
        }
    };
    public static void main(String[] args) {
        
    }
    private ConjunctiveNormalForm _clauses(ALogicTree tree) {
        return IntConstraintFactory.clauses(tree, solver);
    }

    private static BoolVar _not(BoolVar b) {
        return VariableFactory.not(b);
    }

    private static Node _reified(Literal var, ALogicTree tree) {
        return Node.reified(var, tree);
    }

    private static Node _reified(BoolVar var, ALogicTree tree) {
        return _reified(Literal.pos(var), tree);
    }

    private static Node _and(ALogicTree... children) {
        return Node.and(children);
    }

    private static Constraint _implies(BoolVar b, Constraint c) {
        return IntConstraintFactory.implies(b, c);
    }

    private static IntVar _sum(IntVar var1, IntVar var2) {
        return Sum.var(var1, var2);
    }

    private static Constraint _sum(IntVar sum, IntVar... vars) {
        return IntConstraintFactory.sum(vars, sum);
    }

    private static Constraint _sum(IntVar sum, BoolVar... vars) {
        return IntConstraintFactory.sum(vars, sum);
    }

    private static Constraint _times(IntVar product, IntVar var1, IntVar var2) {
        return IntConstraintFactory.times(product, var1, var2);
    }

    private static Constraint _arithm(IntVar var1, String op1, IntVar var2, String op2, int cste) {
        return IntConstraintFactory.arithm(var1, op1, var2, op2, cste);
    }

    private static Constraint _arithm(IntVar var1, String op, IntVar var2) {
        if (var2.instantiated()) {
            return IntConstraintFactory.arithm(var1, op, var2.getValue());
        }
        return IntConstraintFactory.arithm(var1, op, var2);
    }

    private static Constraint _element(IntVar index, IntVar[] array, IntVar value) {
        return IntConstraintFactory.element(value, array, index, 0);
    }

    private static Constraint _all_equal(SetVar... vars) {
        return SetConstraintsFactory.all_equal(vars);
    }

    private static Constraint _all_different(SetVar... vars) {
        return SetConstraintsFactory.all_different(vars);
    }

    private static Constraint _all_different(IntVar... vars) {
        return IntConstraintFactory.alldifferent(vars, "AC");
    }

    private static Constraint _member(IntVar var, int low, int high) {
        return IntConstraintFactory.member(var, low, high);
    }

    private static Constraint _not_member(IntVar var, int low, int high) {
        return IntConstraintFactory.not_member(var, low, high);
    }

    private static Constraint _lex_chain_less_eq(IntVar[]... vars) {
        if (vars.length == 2) {
            return IntConstraintFactory.lex_less_eq(vars[0], vars[1]);
        }
        return IntConstraintFactory.lex_chain_less_eq(vars);
    }

    private static Constraint _union(SetVar[] operands, SetVar union) {
        return SetConstraintsFactory.union(operands, union);
    }

    private int getLB(IntVar... vars) {
        if (vars.length < 0) {
            throw new IllegalArgumentException();
        }
        int lb = vars[0].getLB();
        for (int i = 1; i < vars.length; i++) {
            lb = Math.min(lb, vars[i].getLB());
        }
        return lb;
    }

    private int getUB(IntVar... vars) {
        if (vars.length < 0) {
            throw new IllegalArgumentException();
        }
        int ub = vars[0].getUB();
        for (int i = 1; i < vars.length; i++) {
            ub = Math.max(ub, vars[i].getUB());
        }
        return ub;
    }
}
