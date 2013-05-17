package org.clafer.ir.compiler;

import org.clafer.collection.CacheMap;
import org.clafer.ir.IrAllDifferent;
import org.clafer.ir.IrArithm;
import org.clafer.ir.IrArrayToSet;
import org.clafer.ir.IrBoolCast;
import org.clafer.ir.IrElement;
import org.clafer.ir.IrHalfReification;
import org.clafer.ir.IrIfOnlyIf;
import org.clafer.ir.IrIfThenElse;
import org.clafer.ir.IrIntExpr;
import org.clafer.ir.IrBetween;
import org.clafer.ir.IrMember;
import org.clafer.ir.IrNotBetween;
import org.clafer.ir.IrNotImplies;
import org.clafer.ir.IrNotMember;
import org.clafer.ir.IrOr;
import org.clafer.ir.IrSelectN;
import org.clafer.ir.IrSetExpr;
import gnu.trove.set.hash.TIntHashSet;
import java.util.Deque;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;
import org.clafer.ir.IrNot;
import org.clafer.ir.IrSetEquality;
import org.clafer.ir.IrSingleton;
import org.clafer.ir.IrSortInts;
import org.clafer.ir.IrSortStrings;
import org.clafer.ir.IrUnion;
import solver.constraints.nary.cnf.ConjunctiveNormalForm;
import org.clafer.ir.IrAnd;
import org.clafer.ir.IrConstraint;
import org.clafer.Check;
import org.clafer.Util;
import org.clafer.constraint.propagator.PropUtil;
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
import org.clafer.ir.IrBoolLiteral;
import org.clafer.ir.IrBoolVar;
import org.clafer.ir.IrConstraintVisitor;
import org.clafer.ir.IrDomain;
import org.clafer.ir.IrException;
import org.clafer.ir.IrImplies;
import org.clafer.ir.IrCompare;
import org.clafer.ir.IrIntExprVisitor;
import org.clafer.ir.IrIntLiteral;
import org.clafer.ir.IrIntVar;
import org.clafer.ir.IrModule;
import org.clafer.ir.IrSetExprVisitor;
import org.clafer.ir.IrSetLiteral;
import org.clafer.ir.IrSetVar;
import org.clafer.ir.IrUtil;
import org.clafer.ir.analysis.ExpressionAnalysis;
import solver.Solver;
import solver.constraints.IntConstraintFactory;
import solver.constraints.LogicalConstraintFactory;
import solver.constraints.nary.Sum;
import solver.constraints.nary.cnf.LogOp;
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
        module = ExpressionAnalysis.analyze(module);
        for (IrBoolVar var : module.getBoolVars()) {
            boolVar.get(var);
        }
        for (IrIntVar var : module.getIntVars()) {
            intVar.get(var);
        }
        for (IrSetVar var : module.getSetVars()) {
            setVar.get(var);
        }
        for (IrConstraint constraint : module.getConstraints()) {
            solver.post(constraint.accept(constraintCompiler, null));
        }
        return new IrSolutionMap(boolVar, intVar, setVar);
    }

    private BoolVar numBoolVar(String name) {
        return VariableFactory.bool(name + "#" + varNum++, solver);
    }

    private IntVar intVar(String name, IrDomain domain) {
        if (domain.getLowerBound() == 0 && domain.getUpperBound() == 1) {
            return VariableFactory.bool(name, solver);
        }
        if (domain.isBounded()) {
            return VariableFactory.enumerated(name, domain.getLowerBound(), domain.getUpperBound(), solver);
        }
        return VariableFactory.enumerated(name, domain.getValues(), solver);
    }

    private IntVar numIntVar(String name, IrDomain domain) {
        return intVar(name + "#" + varNum++, domain);
    }

    private IntVar numIntVar(String name, int low, int high) {
        return VariableFactory.enumerated(name + "#" + varNum++, low, high, solver);
    }

    private IntVar numIntVar(String name, int[] dom) {
        return VariableFactory.enumerated(name + "#" + varNum++, dom, solver);
    }

    private SetVar numSetVar(String name, IrSetExpr expr) {
        IrDomain env = expr.getEnv();
        return VariableFactory.set(name + "#" + varNum++, env.getValues(), solver);
    }

    private SetVar numSetVar(String name, int low, int high) {
        return VariableFactory.set(name + "#" + varNum++, Util.range(low, high), solver);
    }

    private SetVar numSetVar(String name, int[] env) {
        return VariableFactory.set(name + "#" + varNum++, env, solver);
    }
    private final CacheMap<IrBoolVar, BoolVar> boolVar = new CacheMap<IrBoolVar, BoolVar>() {

        @Override
        protected BoolVar cache(IrBoolVar ir) {
            Boolean constant = IrUtil.getConstant(ir);
            if (constant != null) {
                return constant.booleanValue() ? VariableFactory.one(solver) : VariableFactory.zero(solver);
            }
            return VariableFactory.bool(ir.getName(), solver);
        }
    };
    private final CacheMap<IrIntVar, IntVar> intVar = new CacheMap<IrIntVar, IntVar>() {

        @Override
        protected IntVar cache(IrIntVar ir) {
            Integer constant = IrUtil.getConstant(ir);
            if (constant != null) {
                switch(constant.intValue()) {
                    case 0:
                        VariableFactory.zero(solver);
                    case 1:
                        VariableFactory.one(solver);
                    default:
                        return VariableFactory.fixed(constant, solver);
                }
            }
            return intVar(ir.getName(), ir.getDomain());
        }
    };
    private final CacheMap<IrSetVar, SetVar> setVar = new CacheMap<IrSetVar, SetVar>() {

        @Override
        protected SetVar cache(IrSetVar a) {
            int[] constant = IrUtil.getConstant(a);
            if (constant != null) {
                return VariableFactory.set(a.toString(), constant, constant, solver);
            }
            IrDomain env = a.getEnv();
            IrDomain ker = a.getKer();
            IrDomain card = a.getCard();
            SetVar set = VariableFactory.set(a.getName(), env.getValues(), ker.getValues(), solver);

            if (card.getUpperBound() < env.size()) {
                IntVar setCard = setCardVar.get(set);
                solver.post(_arithm(setCard, "<=", card.getUpperBound()));
            }
            if (card.getLowerBound() > ker.size()) {
                IntVar setCard = setCardVar.get(set);
                solver.post(_arithm(setCard, ">=", card.getLowerBound()));
            }

            return set;
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
            return Constraints.intChannel($sets, $ints);
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

        private final Map<BoolVar, BoolVar> notCache = new HashMap<BoolVar, BoolVar>();

        private BoolVar not(BoolVar var) {
            BoolVar not = notCache.get(var);
            if (not == null) {
                not = VariableFactory.not(var);
                notCache.put(var, not);
            }
            return not;
        }

        @Override
        public BoolVar visit(IrBoolLiteral ir, Void a) {
            return boolVar.get(ir.getVar());
        }

        @Override
        public BoolVar visit(IrNot ir, Void a) {
            return not(boolVar.get(ir.getVar()));
        }

        @Override
        public BoolVar visit(IrAnd ir, Void a) {
            IrBoolExpr[] operands = ir.getOperands();

            BoolVar[] $operands = new BoolVar[operands.length];
            for (int i = 0; i < $operands.length; i++) {
                $operands[i] = operands[i].accept(boolExprCompiler, a);
            }
            switch ($operands.length) {
                case 1:
                    return $operands[0];
                case 2:
                    BoolVar reified = numBoolVar("And");
                    solver.post(_implies(reified, _arithm($operands[0], "+", $operands[1], "=", 2)));
                    solver.post(_implies(reified.not(), _arithm($operands[0], "+", $operands[1], "!=", 2)));
                    return reified;
                default:
                    reified = numBoolVar("And");
                    solver.post(_clauses(LogOp.reified(reified, LogOp.and($operands))));
                    return reified;
            }
        }

        @Override
        public BoolVar visit(IrOr ir, Void a) {
            throw new UnsupportedOperationException("Not supported yet.");
        }

        @Override
        public BoolVar visit(IrImplies ir, Void a) {
            BoolVar $antecedent = ir.getAntecedent().accept(this, a);
            BoolVar $consequent = ir.getConsequent().accept(this, a);
            BoolVar reified = numBoolVar("Implies");
            solver.post(_clauses(LogOp.reified(reified, LogOp.implies($antecedent, $consequent))));
            return reified;
        }

        @Override
        public BoolVar visit(IrNotImplies ir, Void a) {
            BoolVar $antecedent = ir.getAntecedent().accept(this, a);
            BoolVar $consequent = ir.getConsequent().accept(this, a);
            BoolVar reified = numBoolVar("NotImplies");
            solver.post(_clauses(LogOp.reified(reified.not(), LogOp.implies($antecedent, $consequent))));
            return reified;
        }

        @Override
        public BoolVar visit(IrIfThenElse ir, Void a) {
            BoolVar $antecedent = ir.getAntecedent().accept(this, a);
            BoolVar $consequent = ir.getConsequent().accept(this, a);
            BoolVar $alternative = ir.getAlternative().accept(this, a);
            BoolVar reified = numBoolVar("IfThenElse");
            solver.post(_clauses(LogOp.reified(reified, LogOp.ifThenElse(
                    $antecedent, $consequent, $alternative))));
            return reified;
        }

        @Override
        public BoolVar visit(IrIfOnlyIf ir, Void a) {
            BoolVar $left = ir.getLeft().accept(this, a);
            BoolVar $right = ir.getRight().accept(this, a);
            BoolVar reified = numBoolVar("IfOnlyIf");
            solver.post(_implies(reified, _arithm($left, "=", $right)));
            solver.post(_implies(reified.not(), _arithm($left, "!=", $right)));
            return reified;
        }

        @Override
        public BoolVar visit(IrBetween ir, Void a) {
            IntVar $var = ir.getVar().accept(intExprCompiler, a);
            BoolVar reified = numBoolVar("Between");
            solver.post(_implies(reified, _between($var, ir.getLow(), ir.getHigh())));
            solver.post(_implies(reified.not(), _not_between($var, ir.getLow(), ir.getHigh())));
            return reified;
        }

        @Override
        public BoolVar visit(IrNotBetween ir, Void a) {
            IntVar $var = ir.getVar().accept(intExprCompiler, a);
            BoolVar reified = numBoolVar("NotBetween");
            solver.post(_implies(reified, _not_between($var, ir.getLow(), ir.getHigh())));
            solver.post(_implies(reified.not(), _between($var, ir.getLow(), ir.getHigh())));
            return reified;
        }

        @Override
        public BoolVar visit(IrCompare ir, Void a) {
            IntVar $left = ir.getLeft().accept(intExprCompiler, a);
            IntVar $right = ir.getRight().accept(intExprCompiler, a);
            BoolVar reified = numBoolVar("IntCompare");
            solver.post(_implies(reified, _arithm($left, ir.getOp().getSyntax(), $right)));
            solver.post(_implies(reified.not(), _arithm($left, ir.getOp().getOpposite().getSyntax(), $right)));
            return reified;
        }

        @Override
        public BoolVar visit(IrSetEquality ir, Void a) {
            SetVar $left = ir.getLeft().accept(setExprCompiler, a);
            SetVar $right = ir.getRight().accept(setExprCompiler, a);
            BoolVar reified = numBoolVar("SetCompare");
            switch (ir.getOp()) {
                case Equal:
                    solver.post(_implies(reified, _equal($left, $right)));
                    solver.post(_implies(reified.not(), _all_different($left, $right)));
                    return reified;
                case NotEqual:
                    solver.post(_implies(reified, _all_different($left, $right)));
                    solver.post(_implies(reified.not(), _equal($left, $right)));
                    return reified;
                default:
                    throw new IrException();
            }
        }

        @Override
        public BoolVar visit(IrMember ir, Void a) {
            IntVar $element = ir.getElement().accept(intExprCompiler, a);
            SetVar $set = ir.getSet().accept(setExprCompiler, a);
            BoolVar reified = numBoolVar("Member");
            solver.post(_implies(reified, _member($element, $set)));
            solver.post(_implies(reified.not(), _not_member($element, $set)));
            return reified;
        }

        @Override
        public BoolVar visit(IrNotMember ir, Void a) {
            IntVar $element = ir.getElement().accept(intExprCompiler, a);
            SetVar $set = ir.getSet().accept(setExprCompiler, a);
            BoolVar reified = numBoolVar("NotMember");
            solver.post(_implies(reified, _not_member($element, $set)));
            solver.post(_implies(reified.not(), _member($element, $set)));
            return reified;
        }

        @Override
        public BoolVar visit(IrBoolCast ir, Void a) {
            BoolVar $expr = (BoolVar) ir.getExpr().accept(intExprCompiler, a);
            return ir.isFlipped() ? not($expr) : $expr;
        }
    };
    private final IrBoolExprVisitor<Void, Constraint> boolExprConstraintCompiler = new IrBoolExprVisitor<Void, Constraint>() {

        @Override
        public Constraint visit(IrBoolLiteral ir, Void a) {
            return _arithm(boolVar.get(ir.getVar()), "=", VariableFactory.fixed(1, solver));
        }

        @Override
        public Constraint visit(IrNot ir, Void a) {
            return _arithm(boolVar.get(ir.getVar()), "=", VariableFactory.fixed(0, solver));
        }

        @Override
        public Constraint visit(IrAnd ir, Void a) {
            IrBoolExpr[] operands = ir.getOperands();

            BoolVar[] $operands = new BoolVar[operands.length];
            for (int i = 0; i < $operands.length; i++) {
                $operands[i] = operands[i].accept(boolExprCompiler, a);
            }
            switch ($operands.length) {
                case 1:
                    return _arithm($operands[0], "=", 1);
                case 2:
                    return _arithm($operands[0], "+", $operands[1], "=", 2);
                default:
                    return _sum(VariableFactory.fixed($operands.length, solver), $operands);
            }
        }

        @Override
        public Constraint visit(IrOr ir, Void a) {
            throw new UnsupportedOperationException("Not supported yet.");
        }

        @Override
        public Constraint visit(IrImplies ir, Void a) {
            BoolVar $antecedent = ir.getAntecedent().accept(boolExprCompiler, a);
            Constraint $consequent = ir.getConsequent().accept(this, a);
            return _implies($antecedent, $consequent);
        }

        @Override
        public Constraint visit(IrNotImplies ir, Void a) {
            throw new UnsupportedOperationException("Not supported yet.");
        }

        @Override
        public Constraint visit(IrIfThenElse ir, Void a) {
            BoolVar $antecedent = ir.getAntecedent().accept(boolExprCompiler, a);
            Constraint $consequent = ir.getConsequent().accept(this, a);
            Constraint $alternative = ir.getAlternative().accept(this, a);
            return _ifThenElse($antecedent, $consequent, $alternative);
        }

        @Override
        public Constraint visit(IrIfOnlyIf ir, Void a) {
            BoolVar $left = ir.getLeft().accept(boolExprCompiler, a);
            BoolVar $right = ir.getRight().accept(boolExprCompiler, a);
            return _arithm($left, "=", $right);
        }

        @Override
        public Constraint visit(IrBetween ir, Void a) {
            IntVar $var = ir.getVar().accept(intExprCompiler, a);
            return _between($var, ir.getLow(), ir.getHigh());
        }

        @Override
        public Constraint visit(IrNotBetween ir, Void a) {
            IntVar $var = ir.getVar().accept(intExprCompiler, a);
            return _not_between($var, ir.getLow(), ir.getHigh());
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
                    return _equal($left, $right);
                case NotEqual:
                    return _all_different($left, $right);
                default:
                    throw new IrException();
            }
        }

        @Override
        public Constraint visit(IrMember ir, Void a) {
            IntVar $element = ir.getElement().accept(intExprCompiler, a);
            SetVar $set = ir.getSet().accept(setExprCompiler, a);
            return _member($element, $set);
        }

        @Override
        public Constraint visit(IrNotMember ir, Void a) {
            IntVar $element = ir.getElement().accept(intExprCompiler, a);
            SetVar $set = ir.getSet().accept(setExprCompiler, a);
            return _not_member($element, $set);
        }

        @Override
        public Constraint visit(IrBoolCast ir, Void a) {
            IntVar $expr = ir.getExpr().accept(intExprCompiler, a);
            return _arithm($expr, "=", ir.isFlipped() ? 0 : 1);
        }
    };
//    private final IrBoolExprVisitor<Void, ALogicTree> boolExprTreeCompiler = new IrBoolExprVisitor<Void, ALogicTree>() {
//
//        @Override
//        public ALogicTree visit(IrBoolVar ir, Void a) {
//            if (ir.isConstant()) {
//                return ir.isTrue() ? Singleton.TRUE : Singleton.FALSE;
//            }
//            return Literal.pos(boolVar.get(ir));
//        }
//
//        @Override
//        public ALogicTree visit(IrNot ir, Void a) {
//            return Node.nor(ir.getProposition().accept(this, a));
//        }
//
//        @Override
//        public ALogicTree visit(IrAnd ir, Void a) {
//            IrBoolExpr[] operands = ir.getOperands();
//
//            ALogicTree[] $operands = new ALogicTree[operands.length];
//            for (int i = 0; i < operands.length; i++) {
//                $operands[i] = operands[i].accept(this, a);
//            }
//            return _and($operands);
//        }
//
//        @Override
//        public ALogicTree visit(IrImplies ir, Void a) {
//            ALogicTree $antecedent = ir.getAntecedent().accept(this, a);
//            ALogicTree $consequent = ir.getConsequent().accept(this, a);
//            return Node.implies($antecedent, $consequent);
//        }
//
//        @Override
//        public ALogicTree visit(IrIfOnlyIf ir, Void a) {
//            ALogicTree $left = ir.getLeft().accept(this, a);
//            ALogicTree $right = ir.getRight().accept(this, a);
//            return Node.ifOnlyIf($left, $right);
//        }
//
//        @Override
//        public ALogicTree visit(IrBetween ir, Void a) {
//            return Literal.pos(ir.accept(boolExprCompiler, a));
//        }
//
//        @Override
//        public ALogicTree visit(IrNotBetween ir, Void a) {
//            return Literal.pos(ir.accept(boolExprCompiler, a));
//        }
//
//        @Override
//        public ALogicTree visit(IrCompare ir, Void a) {
//            return Literal.pos(ir.accept(boolExprCompiler, a));
//        }
//
//        @Override
//        public ALogicTree visit(IrSetEquality ir, Void a) {
//            return Literal.pos(ir.accept(boolExprCompiler, a));
//        }
//    };
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
        public IntVar visit(IrIntLiteral ir, Void a) {
            return intVar.get(ir.getVar());
        }

        @Override
        public IntVar visit(IrCard ir, Void a) {
            return setCardVar.get(ir.getSet().accept(setExprCompiler, a));
        }

        @Override
        public IntVar visit(IrArithm ir, Void a) {
            int constants;
            IrIntExpr[] operands = ir.getOperands();
            Deque<IntVar> filter = new LinkedList<IntVar>();
            switch (ir.getOp()) {
                case Add: {
                    constants = 0;
                    for (IrIntExpr operand : operands) {
                        Integer constant = IrUtil.getConstant(operand);
                        if (constant != null) {
                            constants += constant.intValue();
                        } else {
                            filter.add(operand.accept(this, a));
                        }
                    }
                    IntVar[] addends = filter.toArray(new IntVar[filter.size()]);
                    switch (addends.length) {
                        case 0:
                            // This case should have already been optimized earlier.
                            return VariableFactory.fixed(constants, solver);
                        case 1:
                            return VariableFactory.offset(addends[0], constants);
                        case 2:
                            return VariableFactory.offset(_sum(addends[0], addends[1]), constants);
                        default:
                            IntVar sum = numIntVar("Sum", ir.getDomain());
                            solver.post(_sum(sum, addends));
                            return VariableFactory.offset(sum, constants);
                    }
                }
                case Sub: {
                    constants = 0;
                    for (int i = 1; i < operands.length; i++) {
                        Integer constant = IrUtil.getConstant(operands[i]);
                        if (constant != null) {
                            constants += constant.intValue();
                        } else {
                            filter.add(operands[i].accept(this, a));
                        }
                    }
                    Integer constant = IrUtil.getConstant(operands[0]);
                    if (constant != null) {
                        IntVar[] subtractends = filter.toArray(new IntVar[filter.size()]);
                        switch (subtractends.length) {
                            case 0:
                                return VariableFactory.fixed(constant - constants, solver);
                            case 1:
                                return VariableFactory.offset(VariableFactory.minus(subtractends[0]), constant - constants);
                            case 2:
                                return VariableFactory.offset(VariableFactory.minus(_sum(subtractends[0], subtractends[1])), constant - constants);
                            default:
                                IntVar diff = numIntVar("Diff", ir.getDomain());
                                solver.post(_difference(diff, Util.cons(VariableFactory.fixed(constant - constants, solver), subtractends)));
                                return VariableFactory.offset(diff, -constants);
                        }
                    }
                    filter.add(operands[0].accept(this, a));
                    IntVar[] subtractends = filter.toArray(new IntVar[filter.size()]);
                    switch (subtractends.length) {
                        case 1:
                            return VariableFactory.offset(subtractends[0], -constants);
                        case 2:
                            return VariableFactory.offset(_sum(subtractends[0],
                                    VariableFactory.minus(subtractends[1])), -constants);
                        default:
                            IntVar diff = numIntVar("Diff", ir.getDomain());
                            solver.post(_difference(diff, subtractends));
                            return VariableFactory.offset(diff, -constants);
                    }
                }
                case Mul: {
                    // TODO: assert operands.length == 2
                    constants = 1;
                    for (IrIntExpr operand : operands) {
                        Integer constant = IrUtil.getConstant(operand);
                        if (constant != null) {
                            constants *= constant.intValue();
                        } else {
                            filter.add(operand.accept(this, a));
                        }
                    }
                    if (filter.isEmpty()) {
                        // This case should have already been optimized earlier.
                        return VariableFactory.fixed(constants, solver);
                    }
                    if (constants < -1) {
                        filter.add(VariableFactory.fixed(constants, solver));
                    }
                    IntVar[] multiplicands = filter.toArray(new IntVar[filter.size()]);
                    switch (multiplicands.length) {
                        case 1:
                            return constants < -1 ? multiplicands[0]
                                    : VariableFactory.scale(multiplicands[0], constants);
                        default:
                            IntVar multiplicand = multiplicands[0];
                            for (int i = 1; i < multiplicands.length; i++) {
                                IntVar product = numIntVar("Mul", ir.getDomain());
                                solver.post(_times(multiplicand, multiplicands[i], product));
                                multiplicand = product;
                            }
                            return constants < -1 ? multiplicand : VariableFactory.scale(multiplicand, constants);
                    }
                }
                case Div: {
                    // TODO: assert operands.length == 2
                    IntVar divisor = operands[0].accept(this, a);
                    for (int i = 1; i < operands.length; i++) {
                        IntVar quotient = numIntVar("Div", ir.getDomain());
                        solver.post(_times(divisor, operands[i].accept(this, a), quotient));
                        divisor = quotient;
                    }
                    return divisor;
                }
            }
            throw new IrException();
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
        public SetVar visit(IrSetLiteral ir, Void a) {
            return setVar.get(ir.getVar());
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
        public SetVar visit(IrArrayToSet ir, Void a) {
            IrIntExpr[] array = ir.getArray();

            IntVar[] $array = new IntVar[array.length];
            for (int i = 0; i < $array.length; i++) {
                $array[i] = array[i].accept(intExprCompiler, a);
            }
            SetVar set = numSetVar("Singleton", ir);
            solver.post(Constraints.arrayToSet($array, set));
            return set;
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
                PropUtil.iterateEnv($children[i], env);
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
                PropUtil.iterateEnv($operands[i], env);
            }
            SetVar union = numSetVar("Union", env.toArray());
            solver.post(_union($operands, union));
            return union;
        }
    };

    private ConjunctiveNormalForm _clauses(LogOp tree) {
        return IntConstraintFactory.clauses(tree, solver);
    }

    private static Constraint _implies(BoolVar b, Constraint c) {
        return LogicalConstraintFactory.ifThen(b, c);
    }

    private static Constraint _ifThenElse(BoolVar b, Constraint c, Constraint d) {
        return LogicalConstraintFactory.ifThenElse(b, c, d);
    }

    private static IntVar _sum(IntVar var1, IntVar var2) {
        return Sum.var(var1, var2);
    }

    private static Constraint _difference(IntVar difference, IntVar... vars) {
        int[] coeffiecients = new int[vars.length];
        coeffiecients[0] = 1;
        for (int i = 1; i < coeffiecients.length; i++) {
            coeffiecients[i] = -1;
        }
        return IntConstraintFactory.scalar(vars, coeffiecients, difference);
    }

    private static Constraint _sum(IntVar sum, IntVar... vars) {
        return IntConstraintFactory.sum(vars, sum);
    }

    private static Constraint _sum(IntVar sum, BoolVar... vars) {
        return IntConstraintFactory.sum(vars, sum);
    }

    private static Constraint _times(IntVar var1, IntVar var2, IntVar product) {
        return IntConstraintFactory.times(var1, var2, product);
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

    private static Constraint _arithm(IntVar var1, String op, int c) {
        return IntConstraintFactory.arithm(var1, op, c);
    }

    private static Constraint _element(IntVar index, IntVar[] array, IntVar value) {
        return IntConstraintFactory.element(value, array, index, 0);
    }

    private static Constraint _equal(SetVar var1, SetVar var2) {
        return Constraints.equal(var1, var2);
    }

    private static Constraint _all_different(SetVar... vars) {
        return SetConstraintsFactory.all_different(vars);
    }

    private static Constraint _all_different(IntVar... vars) {
        return IntConstraintFactory.alldifferent(vars, "AC");
    }

    private static Constraint _between(IntVar var, int low, int high) {
        return IntConstraintFactory.member(var, low, high);
    }

    private static Constraint _not_between(IntVar var, int low, int high) {
        return IntConstraintFactory.not_member(var, low, high);
    }

    private static Constraint _member(IntVar element, SetVar set) {
        return SetConstraintsFactory.member(element, set);
    }

    private static Constraint _not_member(IntVar element, SetVar set) {
        return Constraints.notMember(element, set);
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

    public static void main(String[] args) {
    }
}
