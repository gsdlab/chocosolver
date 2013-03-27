package org.clafer.ir.compiler;

import org.clafer.collection.CacheMap;
import org.clafer.ir.IrAllDifferent;
import org.clafer.ir.IrIntExpr;
import org.clafer.ir.IrSetExpr;
import gnu.trove.set.hash.TIntHashSet;
import org.clafer.ir.IrNot;
import org.clafer.ir.IrSetCompare;
import org.clafer.ir.IrSort;
import org.clafer.ir.IrUnion;
import solver.constraints.nary.cnf.ConjunctiveNormalForm;
import org.clafer.ir.IrAnd;
import org.clafer.ir.IrConstraint;
import org.clafer.Check;
import org.clafer.constraint.ConstraintUtil;
import org.clafer.constraint.Increasing;
import org.clafer.ir.IrBoolChannel;
import org.clafer.ir.IrBoolConstraint;
import org.clafer.ir.IrIntChannel;
import org.clafer.ir.IrJoin;
import org.clafer.ir.IrJoinRef;
import org.clafer.ir.IrSetCard;
import solver.variables.SetVar;
import solver.constraints.Constraint;
import solver.variables.Variable;
import org.clafer.ir.IrBoolExpr;
import org.clafer.ir.IrBoolExprVisitor;
import org.clafer.ir.IrBoolVar;
import org.clafer.ir.IrConstraintVisitor;
import org.clafer.ir.IrDomain;
import org.clafer.ir.IrDomain.IrBoundDomain;
import org.clafer.ir.IrDomain.IrEnumDomain;
import org.clafer.ir.IrException;
import org.clafer.ir.IrImplies;
import org.clafer.ir.IrIntCompare;
import org.clafer.ir.IrIntExprVisitor;
import org.clafer.ir.IrIntVar;
import org.clafer.ir.IrModule;
import org.clafer.ir.IrSetExprVisitor;
import org.clafer.ir.IrSetVar;
import solver.Solver;
import solver.constraints.IntConstraintFactory;
import solver.constraints.nary.cnf.ALogicTree;
import solver.constraints.nary.cnf.Literal;
import solver.constraints.nary.cnf.Node;
import solver.constraints.nary.cnf.Singleton;
import solver.constraints.set.SetConstraintsFactory;
import solver.variables.BoolVar;
import solver.variables.IntVar;
import solver.variables.VariableFactory;
import static org.clafer.ir.Irs.*;

/**
 *
 * @author jimmy
 */
public class IrCompiler {

    public static void main(String[] args) {
        IrBoolExpr ir = implies(bool("feature"), equal(boundInt("footprint", -4, 4), constant(4)));

        IrModule irModule = new IrModule();
        irModule.addConstraint(ir);

        Solver solver = new Solver();
        compile(irModule, solver);

        if (solver.findSolution()) {
            do {
                System.out.println("----");
                for (Variable var : solver.getVars()) {
                    System.out.println(var + " : " + var.getClass());
                }
                System.out.println("****");
            } while (solver.nextSolution());
        }
        System.out.println(solver);
    }
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
        for (IrConstraint constraint : module.getConstraints()) {
            solver.post(constraint.accept(constraintCompiler, null));
        }
        return new IrSolutionMap(boolVar, intVar, setVar);
    }

    private BoolVar numBoolVar(String name) {
        return VariableFactory.bool(name + "#" + varNum++, solver);
    }

    private IntVar numIntVar(String name, int[] dom) {
        return VariableFactory.enumerated(name, dom, solver);
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
            if (domain instanceof IrEnumDomain) {
                IrEnumDomain enumDomain = (IrEnumDomain) domain;
                return VariableFactory.enumerated(ir.getName(), enumDomain.getValues(), solver);
            } else if (domain instanceof IrBoundDomain) {
                IrBoundDomain boundDomain = (IrBoundDomain) domain;
                return VariableFactory.enumerated(ir.getName(), boundDomain.getLow(), boundDomain.getHigh(), solver);
            } else {
                throw new IrException();
            }
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
        public Constraint visit(IrSort ir, Void a) {
            IrIntExpr[] array = ir.getArray();

            IntVar[] $array = new IntVar[array.length];
            for (int i = 0; i < $array.length; i++) {
                $array[i] = array[i].accept(intExprCompiler, a);
            }
            return _sort($array);
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
            BoolVar reified = numBoolVar("Implies");
            solver.post(_implies(reified, _arithm(reified, ">=", reified)));
            solver.post(_implies(_not(reified), _arithm(reified, "<", reified)));
            return reified;
        }

        @Override
        public BoolVar visit(IrIntCompare ir, Void a) {
            IntVar $left = ir.getLeft().accept(intExprCompiler, a);
            IntVar $right = ir.getRight().accept(intExprCompiler, a);
            BoolVar reified = numBoolVar("IntCompare");
            solver.post(_implies(reified, _arithm($left, ir.getOp().getSyntax(), $right)));
            solver.post(_implies(_not(reified), _arithm($left, ir.getOp().getOpposite().getSyntax(), $right)));
            return reified;
        }

        @Override
        public BoolVar visit(IrSetCompare ir, Void a) {
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
            return _clauses(ir.accept(boolExprTreeCompiler, a));
        }

        @Override
        public Constraint visit(IrImplies ir, Void a) {
            BoolVar $antecedent = ir.getAntecedent().accept(boolExprCompiler, a);
            Constraint $consequent = ir.getConsequent().accept(this, a);
            return _implies($antecedent, $consequent);
        }

        @Override
        public Constraint visit(IrIntCompare ir, Void a) {
            IntVar $left = ir.getLeft().accept(intExprCompiler, a);
            IntVar $right = ir.getRight().accept(intExprCompiler, a);
            return _arithm($left, ir.getOp().getSyntax(), $right);
        }

        @Override
        public Constraint visit(IrSetCompare ir, Void a) {
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
            return Literal.neg(ir.getProposition().accept(boolExprCompiler, a));
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
        public ALogicTree visit(IrIntCompare ir, Void a) {
            return Literal.pos(ir.accept(boolExprCompiler, a));
        }

        @Override
        public ALogicTree visit(IrSetCompare ir, Void a) {
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
        public IntVar visit(IrSetCard ir, Void a) {
            return setCardVar.get(ir.getSet().accept(setExprCompiler, a));
        }
    };
    private final IrSetExprVisitor<Void, SetVar> setExprCompiler = new IrSetExprVisitor<Void, SetVar>() {

        @Override
        public SetVar visit(IrSetVar ir, Void a) {
            return setVar.get(ir);
        }

        @Override
        public SetVar visit(IrJoin ir, Void a) {
            throw new UnsupportedOperationException("Not supported yet.");
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
                ConstraintUtil.iterateEnv($operands[i], env);
            }
            SetVar union = numSetVar("Union", env.toArray());
            solver.post(_union($operands, union));
            return union;
        }
    };

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

    private static Constraint _arithm(IntVar var1, String op, IntVar var2) {
        if (var2.instantiated()) {
            return IntConstraintFactory.arithm(var1, op, var2.getValue());
        }
        return IntConstraintFactory.arithm(var1, op, var2);
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

    private static Constraint _union(SetVar[] operands, SetVar union) {
        return SetConstraintsFactory.union(operands, union);
    }

    private Constraint _sort(IntVar[] vars) {
        return new Increasing(vars, solver);
    }
}
