package org.clafer.ir.compiler;

import gnu.trove.set.hash.TIntHashSet;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import org.chocosolver.solver.Model;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.constraints.Operator;
import org.chocosolver.solver.variables.BoolVar;
import org.chocosolver.solver.variables.CStringVar;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.solver.variables.SetVar;
import static org.chocosolver.solver.variables.Var.mapCard;
import static org.chocosolver.solver.variables.Var.mapChars;
import static org.chocosolver.solver.variables.Var.mapLength;
import org.chocosolver.util.ESat;
import org.clafer.choco.constraint.Constraints;
import org.clafer.collection.Pair;
import org.clafer.common.Check;
import org.clafer.common.Util;
import org.clafer.domain.BoolDomain;
import org.clafer.domain.Domain;
import org.clafer.ir.IrAcyclic;
import org.clafer.ir.IrAdd;
import org.clafer.ir.IrAllDifferent;
import org.clafer.ir.IrAnd;
import org.clafer.ir.IrArrayEquality;
import org.clafer.ir.IrArrayToSet;
import org.clafer.ir.IrBoolChannel;
import org.clafer.ir.IrBoolExpr;
import org.clafer.ir.IrBoolExprVisitor;
import org.clafer.ir.IrBoolVar;
import org.clafer.ir.IrCard;
import org.clafer.ir.IrCompare;
import org.clafer.ir.IrConcat;
import org.clafer.ir.IrConnected;
import org.clafer.ir.IrContainsSetTernary;
import org.clafer.ir.IrCount;
import org.clafer.ir.IrDiv;
import org.clafer.ir.IrElement;
import org.clafer.ir.IrExpr;
import org.clafer.ir.IrIfOnlyIf;
import org.clafer.ir.IrIfThenElse;
import org.clafer.ir.IrIntArrayExpr;
import org.clafer.ir.IrIntArrayExprVisitor;
import org.clafer.ir.IrIntArrayVar;
import org.clafer.ir.IrIntChannel;
import org.clafer.ir.IrIntExpr;
import org.clafer.ir.IrIntExprVisitor;
import org.clafer.ir.IrIntVar;
import org.clafer.ir.IrInverse;
import org.clafer.ir.IrJoinFunction;
import org.clafer.ir.IrJoinRelation;
import org.clafer.ir.IrLength;
import org.clafer.ir.IrLone;
import org.clafer.ir.IrMask;
import org.clafer.ir.IrMember;
import org.clafer.ir.IrMinus;
import org.clafer.ir.IrMod;
import org.clafer.ir.IrModule;
import org.clafer.ir.IrMul;
import org.clafer.ir.IrNot;
import org.clafer.ir.IrNotMember;
import org.clafer.ir.IrOffset;
import org.clafer.ir.IrOne;
import org.clafer.ir.IrOr;
import org.clafer.ir.IrPrefix;
import org.clafer.ir.IrRegister;
import org.clafer.ir.IrSelectN;
import org.clafer.ir.IrSetArrayExpr;
import org.clafer.ir.IrSetArrayExprVisitor;
import org.clafer.ir.IrSetArrayVar;
import org.clafer.ir.IrSetDifference;
import org.clafer.ir.IrSetElement;
import org.clafer.ir.IrSetEquality;
import org.clafer.ir.IrSetExpr;
import org.clafer.ir.IrSetExprVisitor;
import org.clafer.ir.IrSetIntersection;
import org.clafer.ir.IrSetMax;
import org.clafer.ir.IrSetMin;
import org.clafer.ir.IrSetSum;
import org.clafer.ir.IrSetTernary;
import org.clafer.ir.IrSetUnion;
import org.clafer.ir.IrSetVar;
import org.clafer.ir.IrSingleton;
import org.clafer.ir.IrSingletonFilter;
import org.clafer.ir.IrSortSets;
import org.clafer.ir.IrSortStrings;
import org.clafer.ir.IrSortStringsChannel;
import org.clafer.ir.IrStringCompare;
import org.clafer.ir.IrStringElement;
import org.clafer.ir.IrStringExpr;
import org.clafer.ir.IrStringExprVisitor;
import org.clafer.ir.IrStringVar;
import org.clafer.ir.IrSubarray;
import org.clafer.ir.IrSubsetEq;
import org.clafer.ir.IrSuffix;
import org.clafer.ir.IrTernary;
import org.clafer.ir.IrTransitiveClosure;
import org.clafer.ir.IrUnreachable;
import org.clafer.ir.IrVar;
import org.clafer.ir.Irs;
import static org.clafer.ir.Irs.not;
import org.clafer.ir.analysis.CommonSubexpression;
import org.clafer.ir.analysis.Optimizer;
import org.clafer.ir.analysis.deduction.Coalesce;

/**
 * Compile from IR to Choco.
 *
 * @author jimmy
 */
public class IrCompiler {

    private final Model model;
    private final boolean coalesceVariables;
    private int varNum = 0;

    private IrCompiler(Model model, boolean coalesceVariables) {
        this.model = Check.notNull(model);
        this.coalesceVariables = coalesceVariables;
    }

    public static IrSolutionMap compile(IrModule in, Model out) {
        return compile(in, out, true);
    }

    public static IrSolutionMap compile(IrModule in, Model out, boolean coalesceVariables) {
        IrCompiler compiler = new IrCompiler(out, coalesceVariables);
        return compiler.compile(in);
    }

    private IrSolutionMap compile(IrModule module) {
        Pair<Coalesce, IrModule> optimizedPair = Optimizer.optimize(module, coalesceVariables);
        Coalesce coalesce = optimizedPair.getFst();
        IrModule optModule = optimizedPair.getSnd();
        commonSubexpressions.addAll(CommonSubexpression.findCommonSubexpressions(optModule));

        for (IrBoolExpr constraint : optModule.getConstraints()) {
            Constraint c = compileAsConstraint(constraint);
            if (ESat.TRUE.equals(c.isSatisfied())) {
                // TODO
//                assert constraint instanceof IrRegister;
            } else {
                post(c);
            }
        }

        return new IrSolutionMap(coalesce, intVarMap, setVarMap);
    }

    private final Map<IrIntVar, IntVar> intVarMap = new HashMap<>();
    private final Map<IrSetVar, SetVar> setVarMap = new HashMap<>();
    private final Map<IrStringVar, CStringVar> stringVarMap = new HashMap<>();
    private final Map<TIntHashSet, SetVar> cachedSetConstants = new HashMap<>();
    private final Map<IntVar, IntVar> cachedMinus = new HashMap<>();
    private final Map<Pair<IntVar, Integer>, IntVar> cachedOffset = new HashMap<>();
    private final Map<Pair<IntVar, Integer>, IntVar> cachedScale = new HashMap<>();
    private final Set<IrExpr> commonSubexpressions = new HashSet<>();
    private final Map<IrIntExpr, IntVar> cachedCommonIntSubexpressions = new HashMap<>();
    private final Map<IrSetExpr, SetVar> cachedCommonSetSubexpressions = new HashMap<>();
    private final Map<IrStringExpr, CStringVar> cachedCommonStringSubexpressions = new HashMap<>();
    private final Map<IrIntArrayExpr, IntVar[]> cachedCommonIntArraySubexpressions = new HashMap<>();
    private final Map<IrSetArrayExpr, SetVar[]> cachedCommonSetArraySubexpressions = new HashMap<>();

    private void post(Constraint constraint) {
        model.post(constraint);
    }

    private BoolVar boolVar(String name, BoolDomain domain) {
        if (domain.isTrue()) {
            return model.boolVar(true);
        }
        if (domain.isFalse()) {
            return model.boolVar(false);
        }
        return model.boolVar(name);
    }

    private IntVar intVar(String name, Domain domain) {
        if (domain.isConstant()) {
            int constant = domain.getLowBound();
            switch (domain.getLowBound()) {
                case 0:
                    return model.boolVar(false);
                case 1:
                    return model.boolVar(true);
                default:
                    return model.intVar(constant);
            }
        }
        if (domain.getLowBound() == 0 && domain.getHighBound() == 1) {
            return model.boolVar(name);
        }
        if (domain.isBounded()) {
            return model.intVar(name, domain.getLowBound(), domain.getHighBound(), false);
        }
        return model.intVar(name, domain.getValues());
    }

    private SetVar setVar(String name, Domain env, Domain ker) {
        if (env.size() == ker.size()) {
            int[] values = ker.getValues();
            TIntHashSet valueSet = new TIntHashSet(values);
            SetVar var = cachedSetConstants.get(valueSet);
            if (var == null) {
                var = model.setVar(values);
                cachedSetConstants.put(valueSet, var);
            }
            return var;
        }
        return model.setVar(name, ker.getValues(), env.getValues());
    }

    private SetVar setVar(String name, Domain env, Domain ker, Domain card) {
        if (card.getLowBound() == env.size()) {
            return model.setVar(env.getValues());
        }
        if (card.getHighBound() == ker.size()) {
            return model.setVar(ker.getValues());
        }
        SetVar set = setVar(name, env, ker);
        // TODO: not always needed
        set.setCard(model.intVar("|" + name + "|", card.getValues()));
        return set;
    }

    private CStringVar cstring(String name, Domain[] chars, Domain length) {
        IntVar[] $chars = new IntVar[chars.length];
        for (int i = 0; i < $chars.length; i++) {
            $chars[i] = intVar(name + "[" + i + "]", chars[i]);
        }
        IntVar $length = intVar("|" + name + "|", length);
        return new CStringVar($chars, $length);
    }

    private BoolVar numBoolVar(String name) {
        return boolVar(name + "#" + varNum++, BoolDomain.TrueFalseDomain);
    }

    private IntVar numIntVar(String name, Domain domain) {
        return intVar(name + "#" + varNum++, domain);
    }

    private SetVar numSetVar(String name, Domain env, Domain ker) {
        return setVar(name + "#" + varNum++, env, ker);
    }

    private SetVar numSetVar(String name, Domain env, Domain ker, Domain card) {
        return setVar(name + "#" + varNum++, env, ker, card);
    }

    private SetVar[] numSetVars(String name, Domain[] envs, Domain[] kers, Domain[] cards) {
        SetVar[] sets = new SetVar[envs.length];
        for (int i = 0; i < sets.length; i++) {
            sets[i] = setVar(name + "[" + i + "]#" + varNum++, envs[i], kers[i], cards[i]);
        }
        return sets;
    }

    private CStringVar numCstring(String name, Domain[] chars, Domain length) {
        return cstring(name + "#" + varNum++, chars, length);
    }

    private BoolVar getBoolVar(IrBoolVar var) {
        BoolVar boolVar = (BoolVar) intVarMap.get(var);
        if (boolVar == null) {
            boolVar = boolVar(var.getName(), var.getDomain());
            intVarMap.put(var, boolVar);
        }
        return boolVar;
    }

    private IntVar getIntVar(IrIntVar var) {
        IntVar intVar = intVarMap.get(var);
        if (intVar == null) {
            intVar = intVar(var.getName(), var.getDomain());
            intVarMap.put(var, intVar);
        }
        return intVar;
    }

    private SetVar getSetVar(IrSetVar var) {
        SetVar setVar = setVarMap.get(var);
        if (setVar == null) {
            if (var.isConstant()) {
                setVar = setVar(var.getName(), var.getKer(), var.getKer());
            } else {
                IntVar setCardVar = getIntVar(var.getCardVar());
                setVar = setVar(var.getName(), var.getEnv(), var.getKer());
                setVar.setCard(setCardVar);
            }
            setVarMap.put(var, setVar);
        }
        return setVar;
    }

    private BoolVar asBoolVar(Object obj) {
        if (obj instanceof Constraint) {
            return asBoolVar((Constraint) obj);
        }
        return (BoolVar) obj;
    }

    private BoolVar asBoolVar(Constraint op) {
        return op.reify();
    }

    private BoolVar compileAsBoolVar(IrBoolExpr expr) {
        return asBoolVar(expr.accept(boolExprCompiler, BoolVarNoReify));
    }

    private BoolVar[] compileAsBoolVars(IrBoolExpr[] exprs) {
        BoolVar[] vars = new BoolVar[exprs.length];
        for (int i = 0; i < vars.length; i++) {
            vars[i] = compileAsBoolVar(exprs[i]);
        }
        return vars;
    }

    private IntVar compileAsIntVar(IrBoolExpr expr) {
        return asBoolVar(expr.accept(boolExprCompiler, BoolVarNoReify));
    }

    private IntVar[] compileAsIntVars(IrBoolExpr[] exprs) {
        IntVar[] vars = new IntVar[exprs.length];
        for (int i = 0; i < vars.length; i++) {
            vars[i] = compileAsIntVar(exprs[i]);
        }
        return vars;
    }

    private Constraint asConstraint(Object obj) {
        if (obj instanceof BoolVar) {
            return asConstraint((BoolVar) obj);
        }
        return (Constraint) obj;
    }

    private Constraint asConstraint(BoolVar var) {
        return _arithm(var, "=", 1);
    }

    private Constraint compileAsConstraint(IrBoolExpr expr) {
        return asConstraint(expr.accept(boolExprCompiler, ConstraintNoReify));
    }

    private Constraint compileAsConstraint(IrBoolExpr expr, BoolVar reify) {
        BoolArg arg = new BoolArg(reify, Preference.Constraint);
        Object result = expr.accept(boolExprCompiler, arg);
        if (result instanceof Constraint) {
            Constraint constraint = (Constraint) result;
            if (arg.hasReify()) {
                // The compliation failed to reify, explicitly reify now.
                return _arithm(arg.useReify(), "=", constraint.reify());
            }
            return constraint;
        }
        BoolVar var = (BoolVar) result;
        if (arg.hasReify()) {
            return _arithm(arg.useReify(), "=", var);
        }
        return asConstraint(var);
    }

    private IntVar compileAsEqualVar(IrIntExpr expr, IntVar value) {
        Object result = commonSubexpressions.contains(expr)
                ? compile(expr)
                : expr.accept(intExprCompiler, value);
        if (result instanceof IntVar) {
            return _arithm((IntVar) result, "=", value).reify();
        }
        return ((Constraint) result).reify();
    }

    private Constraint compileAsEqual(IrIntExpr expr, IntVar value, BoolVar reify) {
        Object result = commonSubexpressions.contains(expr)
                ? compile(expr)
                : expr.accept(intExprCompiler, value);
        if (result instanceof IntVar) {
            // The compliation failed to reify, explicitly reify now.
            Constraint eq = _arithm(value, "=", (IntVar) result);
            return _arithm(reify, "=", eq.reify());
        }
        return _arithm(((Constraint) result).reify(), "=", reify);
    }

    private IntVar compileAsNotEqualVar(IrIntExpr expr, IntVar value) {
        Object result = commonSubexpressions.contains(expr)
                ? compile(expr)
                : expr.accept(intExprCompiler, value);
        if (result instanceof IntVar) {
            return _arithm((IntVar) result, "!=", value).reify();
        }
        return ((Constraint) result).reify().not();
    }

    private Constraint compileAsNotEqual(IrIntExpr expr, IntVar value, BoolVar reify) {
        Object result = commonSubexpressions.contains(expr)
                ? compile(expr)
                : expr.accept(intExprCompiler, value);
        if (result instanceof IntVar) {
            // The compliation failed to reify, explicitly reify now.
            Constraint notEq = _arithm(value, "!=", (IntVar) result);
            return _arithm(reify, "=", notEq.reify());
        }
        return _arithm(((Constraint) result).reify(), "!=", reify);
    }

    private Constraint compileAsConstraint(IrIntExpr expr, IntVar reify) {
        Object result = commonSubexpressions.contains(expr)
                ? compile(expr)
                : expr.accept(intExprCompiler, reify);
        if (result instanceof IntVar) {
            // The compliation failed to reify, explicitly reify now.
            return _arithm(reify, "=", (IntVar) result);
        }
        return (Constraint) result;
    }

    private Constraint compileAsConstraint(IrSetExpr expr, SetVar reify) {
        Object result = commonSubexpressions.contains(expr)
                ? compile(expr)
                : expr.accept(setExprCompiler, reify);
        if (result instanceof SetVar) {
            SetVar set = (SetVar) result;
            // The compliation failed to reify, explicitly reify now.
            return _equal(reify, set);
        }
        return (Constraint) result;
    }

    private Constraint compileAsConstraint(IrIntArrayExpr expr, IntVar[] reify) {
        Object result = commonSubexpressions.contains(expr)
                ? compile(expr)
                : expr.accept(intArrayExprCompiler, reify);
        if (result instanceof IntVar[]) {
            IntVar[] array = (IntVar[]) result;
            // The compliation failed to reify, explicitly reify now.
            return Constraints.equal(reify, array);
        }
        return (Constraint) result;
    }

    private Constraint compileArithm(IrIntExpr a, Rel op, int b) {
        return compileArithm(a, op, Irs.constant(b));
    }

    private Constraint compileArithm(IrIntExpr a, Rel op, IrIntExpr b) {
        if (a instanceof IrMinus) {
            IrMinus minus = (IrMinus) a;
            // -a ◁ b <=> a + b ▷ 0
            return compileArithm(minus.getExpr(), Arithm.ADD, b, op.reverse(), 0);
        }
        if (b instanceof IrMinus) {
            IrMinus minus = (IrMinus) b;
            // a ◁ -b <=> a + b ◁ 0
            return compileArithm(a, Arithm.ADD, minus.getExpr(), op, 0);
        }
        if (a instanceof IrNot) {
            IrNot not = (IrNot) a;
            // !a ◁ b <=> 1 - a ◁ b <=> a + b ▷ 1
            return compileArithm(not.getExpr(), Arithm.ADD, b, op.reverse(), 1);
        }
        if (b instanceof IrNot) {
            IrNot not = (IrNot) b;
            // a ◁ !b <=> a ◁ 1 - b <=> a + b ◁ 1
            return compileArithm(a, Arithm.ADD, not.getExpr(), op, 1);
        }
        switch (op) {
            case EQ:
                if (b instanceof IrVar) {
                    return compileAsConstraint(a, compile(b));
                }
                return compileAsConstraint(b, compile(a));
            case NQ:
                if (a instanceof IrVar && b instanceof IrVar) {
                    return _arithm(compile(a), "!=", compile(b));
                }
                if (a instanceof IrBoolExpr && b instanceof IrBoolExpr) {
                    IrBoolExpr boolA = (IrBoolExpr) a;
                    IrBoolExpr boolB = (IrBoolExpr) b;
                    if (boolA instanceof IrCompare) {
                        return compileAsConstraint(boolA.negate(), compileAsIntVar(boolB));
                    }
                    if (boolB instanceof IrCompare) {
                        return compileAsConstraint(boolB.negate(), compileAsIntVar(boolA));
                    }
                    if (boolA instanceof IrBoolVar) {
                        return compileAsConstraint(boolB, compileAsIntVar(boolA.negate()));
                    }
                    return compileAsConstraint(boolA, compileAsIntVar(boolB.negate()));
                }
        }
        return _arithm(compile(a), op.getSyntax(), compile(b));
    }

    private Constraint compileArithm(IrIntExpr a, Rel op1, IrIntExpr b, Arithm op2, int c) {
        if (c == 0) {
            return compileArithm(a, op1, b);
        }
        if (a.isConstant()) {
            int constant = a.getLowBound();
            // a ◁ b + c <=> b ▷ a - c
            // a ◁ b - c <=> b ▷ a + c
            return compileArithm(b, op1.reverse(), op2.negate().compute(constant, c));
        }
        if (b.isConstant()) {
            int constant = b.getLowBound();
            return compileArithm(a, op1, op2.compute(constant, c));
        }
        if (a instanceof IrMinus) {
            IrMinus minus = (IrMinus) a;
            // -a ◁ b + c <=> a + b ▷ -c
            // -a ◁ b - c <=> a + b ▷ c
            return compileArithm(minus.getExpr(), Arithm.ADD, b, op1.reverse(),
                    Arithm.ADD.equals(op2) ? -c : c);
        }
        if (b instanceof IrMinus) {
            IrMinus minus = (IrMinus) b;
            // a ◁ -b + c <=> a + b ▷ c
            // a ◁ -b - c <=> a + b ▷ -c
            return compileArithm(a, Arithm.ADD, minus.getExpr(), op1.reverse(),
                    Arithm.ADD.equals(op2) ? c : -c);
        }
        if (a instanceof IrAdd) {
            IrAdd add = (IrAdd) a;
            IrIntExpr[] addends = add.getAddends();
            if (addends.length == 1) {
                // a + d ◁ b + c <=> a ◁ b + (c - d)
                // a + d ◁ b - c <=> a ◁ b - (c + d)
                return compileArithm(addends[0], op1, b, op2,
                        Arithm.ADD.equals(op2) ? c - add.getOffset() : c + add.getOffset());
            }
        }
        if (b instanceof IrAdd) {
            IrAdd add = (IrAdd) b;
            IrIntExpr[] addends = add.getAddends();
            if (addends.length == 1) {
                // a ◁ b + d + c <=> a ◁ b + (c + d)
                // a ◁ b + d - c <=> a ◁ b - (c - d)
                return compileArithm(a, op1, addends[0], op2,
                        Arithm.ADD.equals(op2) ? c + add.getOffset() : c - add.getOffset());
            }
        }
        if (a instanceof IrNot) {
            IrNot not = (IrNot) a;
            // !a ◁ b + c <=> 1 - a ◁ b + c <=> a + b ▷ 1 - c
            // !a ◁ b - c <=> 1 - a ◁ b - c <=> a + b ▷ 1 + c
            return compileArithm(not.getExpr(), Arithm.ADD, b, op1.reverse(),
                    Arithm.ADD.equals(op2) ? 1 - c : 1 + c);
        }
        if (b instanceof IrNot) {
            IrNot not = (IrNot) b;
            // a ◁ !b + c <=> a ◁ 1 - b + c <=> a + b ▷ 1 + c
            // a ◁ !b - c <=> a ◁ 1 - b - c <=> a + b ▷ 1 - c
            return compileArithm(a, Arithm.ADD, not.getExpr(), op1.reverse(),
                    Arithm.ADD.equals(op2) ? 1 + c : 1 - c);
        }
        return _arithm(compile(a), op1.getSyntax(), compile(b), op2.getSyntax(), c);
    }

    private Constraint compileArithm(IrIntExpr a, Arithm op1, IrIntExpr b, Rel op2, int c) {
        if (a.isConstant()) {
            int constant = a.getLowBound();
            // a + b ◁ c <=> b ◁ c - a
            // a - b ◁ c <=> b ▷ a - c
            return Arithm.ADD.equals(op1)
                    ? compileArithm(b, op2, c - constant)
                    : compileArithm(b, op2.reverse(), constant - c);
        }
        if (b.isConstant()) {
            // a + b ◁ c <=> a ◁ c - b
            // a - b ◁ c <=> a ◁ c + b
            int constant = b.getLowBound();
            return compileArithm(a, op2, op1.negate().compute(c, constant));
        }
        if (b instanceof IrMinus) {
            IrMinus minus = (IrMinus) b;
            // a + (-b) ◁ c <=> a - b ◁ c
            // a - (-b) ◁ c <=> a + b ◁ c
            return compileArithm(a, op1.negate(), minus.getExpr(), op2, c);
        }
        if (a instanceof IrAdd) {
            IrAdd add = (IrAdd) a;
            IrIntExpr[] addends = add.getAddends();
            if (addends.length == 1) {
                // a + d + b ◁ c <=> a + b ◁ (c - d)
                // a + d - b ◁ c <=> a - b ◁ (c - d)
                return compileArithm(addends[0], op1, b, op2, c - add.getOffset());
            }
        }
        if (b instanceof IrAdd) {
            IrAdd add = (IrAdd) b;
            IrIntExpr[] addends = add.getAddends();
            if (addends.length == 1) {
                // a + b + d ◁ c <=> a + b ◁ (c - d)
                // a - (b + d) ◁ c <=> a - b ◁ (c + d)
                return compileArithm(a, op1, addends[0], op2,
                        Arithm.ADD.equals(op1) ? c - add.getOffset() : c + add.getOffset());
            }
        }
        switch (op1) {
            case ADD:
                if (a instanceof IrNot) {
                    IrNot not = (IrNot) a;
                    // !a + b ◁ c <=> 1 - a + b ◁ c <=> b ◁ a + c - 1
                    return compileArithm(b, op2, not.getExpr(), Arithm.ADD, c - 1);
                }
                if (b instanceof IrNot) {
                    IrNot not = (IrNot) b;
                    // a + !b ◁ c <=> a + 1 - b ◁ c <=> a ◁ b + c - 1
                    return compileArithm(a, op2, not.getExpr(), Arithm.ADD, c - 1);
                }
                if (a instanceof IrMinus) {
                    IrMinus minus = (IrMinus) a;
                    // (-a) + b ◁ c <=> b - a ◁ c
                    return compileArithm(b, Arithm.MINUS, minus.getExpr(), op2, c);
                }
                break;
            case MINUS:
                if (a instanceof IrNot) {
                    IrNot not = (IrNot) a;
                    // !a - b ◁ c <=> 1 - a - b ◁ c <=> -a - b ◁ c - 1 <=> a + b ▷ 1 - c
                    return compileArithm(not.getExpr(), Arithm.ADD, b, op2.reverse(), 1 - c);
                }
                if (b instanceof IrNot) {
                    IrNot not = (IrNot) b;
                    // a - !b ◁ c <=> a - 1 + b ◁ c <=> a + b ◁ c + 1
                    return compileArithm(a, Arithm.ADD, not.getExpr(), op2, c + 1);
                }
                if (a instanceof IrMinus) {
                    IrMinus minus = (IrMinus) a;
                    // (-a) - b ◁ c <=> a + b ▷ -c
                    return compileArithm(minus.getExpr(), Arithm.ADD, b, op2.reverse(), -c);
                }
                break;
        }
        return _arithm(compile(a), op1.getSyntax(), compile(b), op2.getSyntax(), c);
    }

    private Constraint compileArithm(IrIntExpr a, IrCompare.Op op, IrIntExpr b) {
        return compileArithm(a, Rel.from(op), b);
    }

    private Constraint compileArithm(IrIntExpr a, IrCompare.Op op1, IrIntExpr b, Arithm op2, int c) {
        return compileArithm(a, Rel.from(op1), b, op2, c);
    }

    private Constraint compileArithm(IrIntExpr a, Arithm op1, IrIntExpr b, IrCompare.Op op2, int c) {
        return compileArithm(a, op1, b, Rel.from(op2), c);
    }

    private Constraint compileArithm(int c, IrCompare.Op op1, IrIntExpr a, Arithm op2, IrIntExpr b) {
        return compileArithm(b, op2, a, Rel.from(op1).reverse(), c);
    }

    private Constraint compileArithm(int c, Arithm op1, IrIntExpr a, IrCompare.Op op2, IrIntExpr b) {
        return compileArithm(b, Rel.from(op2).reverse(), a, op1, c);
    }

    private Object compile(IrBoolExpr expr) {
        return expr.accept(boolExprCompiler, ConstraintNoReify);
    }

    private IntVar compile(IrIntExpr expr) {
        IntVar var = cachedCommonIntSubexpressions.get(expr);
        if (var == null) {
            var = (IntVar) expr.accept(intExprCompiler, null);
            if (commonSubexpressions.contains(expr)) {
                cachedCommonIntSubexpressions.put(expr, var);
            }
        }
        return var;
    }

    private IntVar[] compile(IrIntExpr[] exprs) {
        IntVar[] vars = new IntVar[exprs.length];
        for (int i = 0; i < vars.length; i++) {
            vars[i] = compile(exprs[i]);
        }
        return vars;
    }

    private SetVar compile(IrSetExpr expr) {
        SetVar set = cachedCommonSetSubexpressions.get(expr);
        if (set == null) {
            set = (SetVar) expr.accept(setExprCompiler, null);
            if (commonSubexpressions.contains(expr)) {
                cachedCommonSetSubexpressions.put(expr, set);
            }
        }
        return set;
    }

    private SetVar[] compile(IrSetExpr[] exprs) {
        SetVar[] vars = new SetVar[exprs.length];
        for (int i = 0; i < vars.length; i++) {
            vars[i] = compile(exprs[i]);
        }
        return vars;
    }

    private CStringVar compile(IrStringExpr expr) {
        CStringVar string = cachedCommonStringSubexpressions.get(expr);
        if (string == null) {
            string = (CStringVar) expr.accept(stringExprCompiler, null);
            if (commonSubexpressions.contains(expr)) {
                cachedCommonStringSubexpressions.put(expr, string);
            }
        }
        return string;
    }

    private CStringVar[] compile(IrStringExpr[] exprs) {
        CStringVar[] vars = new CStringVar[exprs.length];
        for (int i = 0; i < vars.length; i++) {
            vars[i] = compile(exprs[i]);
        }
        return vars;
    }

    private IntVar[] compile(IrIntArrayExpr expr) {
        IntVar[] ints = cachedCommonIntArraySubexpressions.get(expr);
        if (ints == null) {
            ints = (IntVar[]) expr.accept(intArrayExprCompiler, null);
            if (commonSubexpressions.contains(expr)) {
                cachedCommonIntArraySubexpressions.put(expr, ints);
            }
        }
        return ints;
    }

    private SetVar[] compile(IrSetArrayExpr expr) {
        SetVar[] sets = cachedCommonSetArraySubexpressions.get(expr);
        if (sets == null) {
            sets = (SetVar[]) expr.accept(setArrayExprCompiler, null);
            if (commonSubexpressions.contains(expr)) {
                cachedCommonSetArraySubexpressions.put(expr, sets);
            }
        }
        return sets;
    }

    private final IrBoolExprVisitor<BoolArg, Object> boolExprCompiler = new IrBoolExprVisitor<BoolArg, Object>() {
        @Override
        public Object visit(IrRegister ir, BoolArg a) {
            IrVar variable = ir.getVariable();
            if (variable instanceof IrBoolVar) {
                compile((IrBoolVar) variable);
            } else if (variable instanceof IrIntVar) {
                compile((IrIntVar) variable);
            } else if (variable instanceof IrSetVar) {
                compile((IrSetVar) variable);
            } else {
                compile((IrStringVar) variable);
            }
            return model.trueConstraint();
        }

        @Override
        public Object visit(IrBoolVar ir, BoolArg a) {
            return getBoolVar(ir);
        }

        @Override
        public Object visit(IrNot ir, BoolArg a) {
            Object expr = compile(ir.getExpr());
            if (expr instanceof Constraint) {
                return ((Constraint) expr).getOpposite();
            }
            BoolVar var = (BoolVar) expr;
            if (a.hasReify()) {
                return _arithm(a.useReify(), "!=", var);
            }
            return Preference.Constraint.equals(a.getPreference())
                    ? _arithm(var, "=", 0)
                    : var.not();
        }

        @Override
        public Object visit(IrAnd ir, BoolArg a) {
            IrBoolExpr[] operands = ir.getOperands();
            switch (operands.length) {
                case 1:
                    // delegate
                    return operands[0].accept(this, a);
                case 2:
                    return compileArithm(operands[0], Arithm.ADD, operands[1], Rel.EQ, 2);
                default:
                    return Constraints.and(compileAsBoolVars(ir.getOperands()));
            }
        }

        @Override
        public Object visit(IrLone ir, BoolArg a) {
            IrBoolExpr[] operands = ir.getOperands();
            switch (operands.length) {
                case 1:
                    return model.trueConstraint();
                case 2:
                    return compileArithm(operands[0], Arithm.ADD, operands[1], Rel.LE, 1);
                default:
                    return Constraints.lone(compileAsBoolVars(ir.getOperands()));
            }
        }

        @Override
        public Object visit(IrOne ir, BoolArg a) {
            IrBoolExpr[] operands = ir.getOperands();
            switch (operands.length) {
                case 1:
                    // delegate
                    return operands[0].accept(this, a);
                case 2:
                    return compileArithm(operands[0], Arithm.ADD, operands[1], Rel.EQ, 1);
                default:
                    return Constraints.one(compileAsBoolVars(ir.getOperands()));
            }
        }

        @Override
        public Object visit(IrOr ir, BoolArg a) {
            IrBoolExpr[] operands = ir.getOperands();
            switch (operands.length) {
                case 1:
                    // delegate
                    return operands[0].accept(this, a);
                case 2:
                    return operands[1].isNegative()
                            ? compileArithm(not(operands[1]), Rel.LE, operands[0])
                            : compileArithm(not(operands[0]), Rel.LE, operands[1]);
                default:
                    return Constraints.or(compileAsBoolVars(ir.getOperands()));
            }
        }

        @Override
        public Object visit(IrIfThenElse ir, BoolArg a) {
            BoolVar antecedent = compileAsBoolVar(ir.getAntecedent());
            BoolVar consequent = compileAsBoolVar(ir.getConsequent());
            BoolVar alternative = compileAsBoolVar(ir.getAlternative());
            return _ifThenElse(antecedent, consequent, alternative);
        }

        @Override
        public Object visit(IrIfOnlyIf ir, BoolArg a) {
            IrBoolExpr left = ir.getLeft();
            IrBoolExpr right = ir.getRight();
            if (left.isNegative()) {
                return compileArithm(ir.getLeft().negate(), Rel.NQ, ir.getRight());
            }
            if (right.isNegative()) {
                return compileArithm(ir.getLeft(), Rel.NQ, ir.getRight().negate());
            }
            return compileArithm(ir.getLeft(), Rel.EQ, ir.getRight());
        }

        @Override
        public Object visit(IrCompare ir, BoolArg a) {
            IrCompare.Op op = ir.getOp();
            IrIntExpr left = ir.getLeft();
            IrIntExpr right = ir.getRight();
            if (left instanceof IrAdd) {
                IrAdd add = (IrAdd) left;
                IrIntExpr[] addends = add.getAddends();
                if (addends.length == 1) {
                    return compileArithm(addends[0], Arithm.MINUS, right, op, -add.getOffset());
                }
                if (addends.length == 2) {
                    if (right.isConstant()) {
                        return compileArithm(addends[0], Arithm.ADD, addends[1], op, right.getLowBound() - add.getOffset());
                    }
                }
            }
            if (right instanceof IrAdd) {
                IrAdd add = (IrAdd) right;
                IrIntExpr[] addends = add.getAddends();
                if (addends.length == 1) {
                    return compileArithm(left, Arithm.MINUS, addends[0], op, add.getOffset());
                }
                if (addends.length == 2) {
                    if (left.isConstant()) {
                        return compileArithm(left.getLowBound() - add.getOffset(), op, addends[0], Arithm.ADD, addends[1]);
                    }
                }
            }
            if (IrCompare.Op.Equal.equals(op)) {
                if (a.hasReify()) {
                    BoolVar reify = a.useReify();
                    return right instanceof IrIntVar
                            ? compileAsEqual(left, compile(right), reify)
                            : compileAsEqual(right, compile(left), reify);
                }
                if (a.getPreference().equals(Preference.BoolVar)) {
                    return right instanceof IrIntVar
                            ? compileAsEqualVar(left, compile(right))
                            : compileAsEqualVar(right, compile(left));
                }
            } else if (IrCompare.Op.NotEqual.equals(op)) {
                if (a.hasReify()) {
                    BoolVar reify = a.useReify();
                    return right instanceof IrIntVar
                            ? compileAsNotEqual(left, compile(right), reify)
                            : compileAsNotEqual(right, compile(left), reify);
                }
                if (a.getPreference().equals(Preference.BoolVar)) {
                    return right instanceof IrIntVar
                            ? compileAsNotEqualVar(left, compile(right))
                            : compileAsNotEqualVar(right, compile(left));
                }
            }
            return compileArithm(left, op, right);
        }

        @Override
        public Object visit(IrArrayEquality ir, BoolArg a) {
            switch (ir.getOp()) {
                case Equal:
                    if (ir.getRight() instanceof IrIntArrayVar) {
                        return compileAsConstraint(ir.getLeft(), compile(ir.getRight()));
                    }
                    return compileAsConstraint(ir.getRight(), compile(ir.getLeft()));
                case NotEqual:
                    return compileAsConstraint(ir.getRight(), compile(ir.getLeft())).getOpposite();
                default:
                    throw new IllegalArgumentException("Unexpected operator.");
            }
        }

        @Override
        public Object visit(IrSetEquality ir, BoolArg a) {
            switch (ir.getOp()) {
                case Equal:
                    if (ir.getRight() instanceof IrSetVar) {
                        return compileAsConstraint(ir.getLeft(), compile(ir.getRight()));
                    }
                    return compileAsConstraint(ir.getRight(), compile(ir.getLeft()));
                case NotEqual:
                    return _not_equal(compile(ir.getLeft()), compile(ir.getRight()));
                default:
                    throw new IllegalArgumentException("Unexpected operator.");
            }
        }

        @Override
        public Object visit(IrStringCompare ir, BoolArg a) {
            CStringVar string1 = compile(ir.getLeft());
            CStringVar string2 = compile(ir.getRight());
            switch (ir.getOp()) {
                case Equal:
                    return Constraints.equal(
                            string1.getChars(), string1.getLength(),
                            string2.getChars(), string2.getLength());
                case NotEqual:
                    return Constraints.notEqual(
                            string1.getChars(), string1.getLength(),
                            string2.getChars(), string2.getLength());
                case LessThan:
                    return Constraints.lessThan(string1.getChars(), string2.getChars());
                case LessThanEqual:
                    return Constraints.lessThanEqual(string1.getChars(), string2.getChars());
                default:
                    throw new IllegalArgumentException("Unexpected operator.");
            }
        }

        @Override
        public Object visit(IrMember ir, BoolArg a) {
            return _member(compile(ir.getElement()), compile(ir.getSet()));
        }

        @Override
        public Object visit(IrNotMember ir, BoolArg a) {
            return _not_member(compile(ir.getElement()), compile(ir.getSet()));
        }

        @Override
        public Object visit(IrSubsetEq ir, BoolArg a) {
            return _subset_eq(compile(ir.getSubset()), compile(ir.getSuperset()));
        }

        @Override
        public Constraint visit(IrBoolChannel ir, BoolArg a) {
            BoolVar[] bools = compileAsBoolVars(ir.getBools());
            SetVar set = compile(ir.getSet());
            return model.setBoolsChanneling(bools, set);
        }

        @Override
        public Constraint visit(IrIntChannel ir, BoolArg a) {
            IntVar[] ints = compile(ir.getInts());
            SetVar[] sets = compile(ir.getSets());
            return Constraints.intChannel(sets, ints);
        }

        @Override
        public Object visit(IrSortStrings ir, BoolArg a) {
            IntVar[][] strings = new IntVar[ir.getStrings().length][];
            for (int i = 0; i < strings.length; i++) {
                strings[i] = compile(ir.getStrings()[i]);
            }
            return ir.isStrict() ? _lex_chain_less(strings) : _lex_chain_less_eq(strings);
        }

        @Override
        public Object visit(IrSortSets ir, BoolArg a) {
            SetVar[] sets = compile(ir.getSets());
            IntVar[] bounds = compile(ir.getBounds());
            return Constraints.sortedSets(sets, mapCard(sets), bounds);
        }

        @Override
        public Object visit(IrSortStringsChannel ir, BoolArg a) {
            IntVar[][] strings = new IntVar[ir.getStrings().length][];
            for (int i = 0; i < strings.length; i++) {
                strings[i] = compile(ir.getStrings()[i]);
            }
            return _lex_chain_channel(strings, compile(ir.getInts()));
        }

        @Override
        public Constraint visit(IrAllDifferent ir, BoolArg a) {
            IrIntExpr[] operands = ir.getOperands();

            IntVar[] $operands = new IntVar[operands.length];
            for (int i = 0; i < $operands.length; i++) {
                $operands[i] = compile(operands[i]);
            }
            return _all_different($operands);
        }

        @Override
        public Constraint visit(IrSelectN ir, BoolArg a) {
            BoolVar[] bools = compileAsBoolVars(ir.getBools());
            IntVar n = compile(ir.getN());
            return Constraints.selectN(bools, n);
        }

        @Override
        public Object visit(IrAcyclic ir, BoolArg a) {
            IntVar[] edges = compile(ir.getEdges());
            return Constraints.acyclic(edges);
        }

        @Override
        public Object visit(IrConnected ir, BoolArg a) {
            SetVar[] relation = compile(ir.getRelation());
            SetVar nodes = compile(ir.getNodes());
            //TODO handle directed graph in the future
            return Constraints.connected(nodes, relation, ir.isDirected());
        }

        @Override
        public Object visit(IrUnreachable ir, BoolArg a) {
            IntVar[] edges = compile(ir.getEdges());
            return Constraints.unreachable(edges, ir.getFrom(), ir.getTo());
        }

        @Override
        public Object visit(IrPrefix ir, BoolArg a) {
            CStringVar prefix = compile(ir.getPrefix());
            CStringVar word = compile(ir.getWord());
            return Constraints.prefix(
                    prefix.getChars(), prefix.getLength(),
                    word.getChars(), word.getLength());
        }

        @Override
        public Object visit(IrSuffix ir, BoolArg a) {
            CStringVar suffix = compile(ir.getSuffix());
            CStringVar word = compile(ir.getWord());
            return Constraints.suffix(
                    suffix.getChars(), suffix.getLength(),
                    word.getChars(), word.getLength());
        }
    };
    private final IrIntExprVisitor<IntVar, Object> intExprCompiler = new IrIntExprVisitor<IntVar, Object>() {
        @Override
        public IntVar visit(IrIntVar ir, IntVar reify) {
            return getIntVar(ir);
        }

        @Override
        public IntVar visit(IrMinus ir, IntVar reify) {
            IntVar expr = compile(ir.getExpr());
            return _minus(expr);
        }

        @Override
        public Object visit(IrCard ir, IntVar reify) {
            SetVar set = compile(ir.getSet());
            return set.getCard();
        }

        private Pair<Integer, IrIntExpr> getCoefficient(IrIntExpr e) {
            if (e instanceof IrMinus) {
                return new Pair<>(-1, ((IrMinus) e).getExpr());
            } else if (e instanceof IrMul) {
                IrMul mul = (IrMul) e;
                if (mul.getMultiplicand().isConstant()) {
                    return new Pair<>(mul.getMultiplicand().getLowBound(), mul.getMultiplier());
                }
                if (mul.getMultiplier().isConstant()) {
                    return new Pair<>(mul.getMultiplier().getLowBound(), mul.getMultiplicand());
                }
            }
            return new Pair<>(1, e);
        }

        private Pair<int[], IrIntExpr[]> getCoefficients(IrIntExpr[] es) {
            int[] coefficients = new int[es.length];
            IrIntExpr[] exprs = new IrIntExpr[es.length];
            for (int i = 0; i < es.length; i++) {
                Pair<Integer, IrIntExpr> coef = getCoefficient(es[i]);
                coefficients[i] = coef.getFst();
                exprs[i] = coef.getSnd();
            }
            return new Pair<>(coefficients, exprs);
        }

        @Override
        public Object visit(IrAdd ir, IntVar reify) {
            int offset = ir.getOffset();
            IrIntExpr[] addends = ir.getAddends();
            switch (addends.length) {
                case 0:
                    // This case should have already been optimized earlier.
                    return model.intVar(offset);
                case 1:
                    if (reify == null) {
                        return _offset(compile(addends[0]), offset);
                    }
                    return _arithm(reify, "-", compile(addends[0]), "=", offset);
                default:
                    Pair<int[], IrIntExpr[]> coeffs = getCoefficients(addends);
                    int[] coefficients = coeffs.getFst();
                    IntVar[] operands = compile(coeffs.getSnd());
                    if (reify == null) {
                        IntVar sum = numIntVar("Sum", ir.getDomain().offset(-offset));
                        post(_scalar(sum, coefficients, operands));
                        return _offset(sum, offset);
                    }
                    if (offset != 0) {
                        coefficients = Util.cons(1, coefficients);
                        operands = Util.cons(model.intVar(offset), operands);
                    }
                    return _scalar(reify, coefficients, operands);
            }
        }

        @Override
        public Object visit(IrMul ir, IntVar reify) {
            IrIntExpr multiplicand = ir.getMultiplicand();
            IrIntExpr multiplier = ir.getMultiplier();
            if (multiplicand.isConstant()) {
                switch (multiplicand.getLowBound()) {
                    case 0:
                        return compileAsConstraint(multiplicand, reify);
                    case 1:
                        return compileAsConstraint(multiplier, reify);
                    default:
                        if (multiplicand.getLowBound() >= -1) {
                            return _scale(compile(multiplier), multiplicand.getLowBound());
                        }
                        return _minus(_scale(compile(multiplier), -multiplicand.getLowBound()));
                }
            }
            if (multiplier.isConstant()) {
                switch (multiplier.getLowBound()) {
                    case 0:
                        return compileAsConstraint(multiplier, reify);
                    case 1:
                        return compileAsConstraint(multiplicand, reify);
                    default:
                        if (multiplier.getLowBound() >= -1) {
                            return _scale(compile(multiplicand), multiplier.getLowBound());
                        }
                        return _minus(_scale(compile(multiplicand), -multiplier.getLowBound()));
                }
            }
            if (reify != null) {
                return _times(compile(multiplicand), compile(multiplier), reify);
            }
            IntVar product = numIntVar("Mul", ir.getDomain());
            post(_times(compile(multiplicand), compile(multiplier), product));
            return product;
        }

        @Override
        public Object visit(IrDiv ir, IntVar reify) {
            IrIntExpr dividend = ir.getDividend();
            IrIntExpr divisor = ir.getDivisor();
            if (reify == null) {
                IntVar quotient = numIntVar("Div", ir.getDomain());
                post(_div(compile(dividend), compile(divisor), quotient));
                return quotient;
            }
            return _div(compile(dividend), compile(divisor), reify);
        }

        @Override
        public Object visit(IrMod ir, IntVar reify) {
            IrIntExpr dividend = ir.getDividend();
            IrIntExpr divisor = ir.getDivisor();
            if (reify == null) {
                IntVar remainder = numIntVar("Mod", ir.getDomain());
                post(_mod(compile(dividend), compile(divisor), remainder));
                return remainder;
            }
            return _mod(compile(dividend), compile(divisor), reify);
        }

        @Override
        public Object visit(IrElement ir, IntVar reify) {
            if (reify == null) {
                IntVar element = numIntVar("Element", ir.getDomain());
                post(_element(compile(ir.getIndex()), compile(ir.getArray()), element));
                return element;
            }
            return _element(compile(ir.getIndex()), compile(ir.getArray()), reify);
        }

        @Override
        public Object visit(IrCount ir, IntVar reify) {
            IntVar[] array = compile(ir.getArray());
            if (reify == null) {
                IntVar count = numIntVar("Count", ir.getDomain());
                post(model.count(ir.getValue(), array, count));
                return count;
            }
            return model.count(ir.getValue(), array, reify);
        }

        @Override
        public Object visit(IrSetMax ir, IntVar reify) {
            SetVar set = compile(ir.getSet());
            if (reify == null) {
                IntVar max = numIntVar("SetMax", ir.getDomain());
                post(Constraints.max(set, set.getCard(), max, ir.getDefaultValue()));
                return max;
            }
            return Constraints.max(set, set.getCard(), reify, ir.getDefaultValue());
        }

        @Override
        public Object visit(IrSetMin ir, IntVar reify) {
            SetVar set = compile(ir.getSet());
            if (reify == null) {
                IntVar min = numIntVar("SetMin", ir.getDomain());
                post(Constraints.min(set, set.getCard(), min, ir.getDefaultValue()));
                return min;
            }
            return Constraints.min(set, set.getCard(), reify, ir.getDefaultValue());
        }

        @Override
        public Object visit(IrSetSum ir, IntVar reify) {
            SetVar set = compile(ir.getSet());
            if (reify == null) {
                IntVar sum = numIntVar("SetSum", ir.getDomain());
                post(Constraints.setSum(set, set.getCard(), sum));
                return sum;
            }
            return Constraints.setSum(set, set.getCard(), reify);
        }

        @Override
        public Object visit(IrTernary ir, IntVar reify) {
            BoolVar antecedent = compileAsBoolVar(ir.getAntecedent());
            IntVar consequent = compile(ir.getConsequent());
            IntVar alternative = compile(ir.getAlternative());
            if (reify == null) {
                IntVar ternary = numIntVar("Ternary", ir.getDomain());
                post(Constraints.ternary(antecedent, ternary, consequent, alternative));
                return ternary;
            }
            return Constraints.ternary(antecedent, reify, consequent, alternative);
        }

        @Override
        public Object visit(IrLength ir, IntVar a) {
            CStringVar string = compile(ir.getString());
            return string.getLength();
        }

        private Object compileBool(IrBoolExpr expr, IntVar a) {
            if (a instanceof BoolVar) {
                return compileAsConstraint(expr, (BoolVar) a);
            }
            IntVar var = compileAsIntVar(expr);
            return a == null ? var : _arithm(var, "=", a);
        }

        @Override
        public Object visit(IrRegister ir, IntVar a) {
            return compileBool(ir, a);
        }

        @Override
        public Object visit(IrBoolVar ir, IntVar a) {
            return compileBool(ir, a);
        }

        @Override
        public Object visit(IrNot ir, IntVar a) {
            return compileBool(ir, a);
        }

        @Override
        public Object visit(IrAnd ir, IntVar a) {
            return compileBool(ir, a);
        }

        @Override
        public Object visit(IrLone ir, IntVar a) {
            return compileBool(ir, a);
        }

        @Override
        public Object visit(IrOne ir, IntVar a) {
            return compileBool(ir, a);
        }

        @Override
        public Object visit(IrOr ir, IntVar a) {
            return compileBool(ir, a);
        }

        @Override
        public Object visit(IrIfThenElse ir, IntVar a) {
            return compileBool(ir, a);
        }

        @Override
        public Object visit(IrIfOnlyIf ir, IntVar a) {
            return compileBool(ir, a);
        }

        @Override
        public Object visit(IrCompare ir, IntVar a) {
            return compileBool(ir, a);
        }

        @Override
        public Object visit(IrArrayEquality ir, IntVar a) {
            return compileBool(ir, a);
        }

        @Override
        public Object visit(IrSetEquality ir, IntVar a) {
            return compileBool(ir, a);
        }

        @Override
        public Object visit(IrStringCompare ir, IntVar a) {
            return compileBool(ir, a);
        }

        @Override
        public Object visit(IrMember ir, IntVar a) {
            return compileBool(ir, a);
        }

        @Override
        public Object visit(IrNotMember ir, IntVar a) {
            return compileBool(ir, a);
        }

        @Override
        public Object visit(IrSubsetEq ir, IntVar a) {
            return compileBool(ir, a);
        }

        @Override
        public Object visit(IrBoolChannel ir, IntVar a) {
            return compileBool(ir, a);
        }

        @Override
        public Object visit(IrIntChannel ir, IntVar a) {
            return compileBool(ir, a);
        }

        @Override
        public Object visit(IrSortStrings ir, IntVar a) {
            return compileBool(ir, a);
        }

        @Override
        public Object visit(IrSortSets ir, IntVar a) {
            return compileBool(ir, a);
        }

        @Override
        public Object visit(IrSortStringsChannel ir, IntVar a) {
            return compileBool(ir, a);
        }

        @Override
        public Object visit(IrAllDifferent ir, IntVar a) {
            return compileBool(ir, a);
        }

        @Override
        public Object visit(IrSelectN ir, IntVar a) {
            return compileBool(ir, a);
        }

        @Override
        public Object visit(IrAcyclic ir, IntVar a) {
            return compileBool(ir, a);
        }

        @Override
        public Object visit(IrConnected ir, IntVar a) {
            return compileBool(ir, a);
        }

        @Override
        public Object visit(IrUnreachable ir, IntVar a) {
            return compileBool(ir, a);
        }

        @Override
        public Object visit(IrPrefix ir, IntVar a) {
            return compileBool(ir, a);
        }

        @Override
        public Object visit(IrSuffix ir, IntVar a) {
            return compileBool(ir, a);
        }
    };
    private final IrSetExprVisitor<SetVar, Object> setExprCompiler = new IrSetExprVisitor<SetVar, Object>() {
        @Override
        public Object visit(IrSetVar ir, SetVar reify) {
            return getSetVar(ir);
        }

        @Override
        public Object visit(IrSingleton ir, SetVar reify) {
            IntVar value = compile(ir.getValue());
            if (reify == null) {
                SetVar singleton = numSetVar("Singleton", ir.getEnv(), ir.getKer());
                post(Constraints.singleton(value, singleton, model.intVar(1)));
                return singleton;
            }
            return Constraints.singleton(value, reify, reify.getCard());
        }

        @Override
        public Object visit(IrSingletonFilter ir, SetVar reify) {
            IntVar value = compile(ir.getValue());
            int filter = ir.getFilter();
            if (reify == null) {
                SetVar singleton = numSetVar("SingletonFilter", ir.getEnv(), ir.getKer(), ir.getCard());
                post(Constraints.singletonFilter(value, singleton, singleton.getCard(), filter));
                return singleton;
            }
            return Constraints.singletonFilter(value, reify, reify.getCard(), filter);
        }

        @Override
        public Object visit(IrArrayToSet ir, SetVar reify) {
            IntVar[] array = compile(ir.getArray());
            if (reify == null) {
                SetVar set = numSetVar("ArrayToSet", ir.getEnv(), ir.getKer(), ir.getCard());
                post(Constraints.arrayToSet(array, set, set.getCard(), ir.getGlobalCardinality()));
                return set;
            }
            return Constraints.arrayToSet(array, reify, reify.getCard(), ir.getGlobalCardinality());
        }

        @Override
        public Object visit(IrSetElement ir, SetVar reify) {
            SetVar[] array = compile(ir.getArray());
            IntVar index = compile(ir.getIndex());
            if (reify == null) {
                SetVar set = numSetVar("Element", ir.getEnv(), ir.getKer(), ir.getCard());
                post(Constraints.element(index, array, mapCard(array), set, set.getCard()));
                return set;
            }
            return Constraints.element(index, array, mapCard(array), reify, reify.getCard());
        }

        @Override
        public Object visit(IrJoinRelation ir, SetVar reify) {
            SetVar take = compile(ir.getTake());
            SetVar[] children = compile(ir.getChildren());
            if (reify == null) {
                SetVar joinRelation = numSetVar("JoinRelation", ir.getEnv(), ir.getKer(), ir.getCard());
                if (ir.isInjective()) {
                    post(Constraints.joinInjectiveRelation(take, take.getCard(),
                            children, mapCard(children), joinRelation, joinRelation.getCard()));
                    return joinRelation;
                } else {
                    post(Constraints.joinRelation(take, children, joinRelation));
                    return joinRelation;
                }
            }
            if (ir.isInjective()) {
                return Constraints.joinInjectiveRelation(take, take.getCard(),
                        children, mapCard(children), reify, reify.getCard());
            }
            return Constraints.joinRelation(take, children, reify);
        }

        @Override
        public Object visit(IrJoinFunction ir, SetVar reify) {
            SetVar take = compile(ir.getTake());
            IntVar[] refs = compile(ir.getRefs());
            if (reify == null) {
                SetVar joinFunction = numSetVar("JoinFunction", ir.getEnv(), ir.getKer(), ir.getCard());
                post(Constraints.joinFunction(take, take.getCard(), refs, joinFunction, joinFunction.getCard(), ir.getGlobalCardinality()));
                return joinFunction;
            }
            return Constraints.joinFunction(take, take.getCard(), refs, reify, reify.getCard(), ir.getGlobalCardinality());
        }

        @Override
        public Object visit(IrSetDifference ir, SetVar reify) {
            SetVar minuend = compile(ir.getMinuend());
            SetVar subtrahend = compile(ir.getSubtrahend());
            if (reify == null) {
                SetVar difference = numSetVar("Difference", ir.getEnv(), ir.getKer(), ir.getCard());
                post(_difference(minuend, subtrahend, difference));
                return difference;
            }
            return _difference(minuend, subtrahend, reify);
        }

        @Override
        public Object visit(IrSetIntersection ir, SetVar reify) {
            SetVar[] operands = compile(ir.getOperands());
            if (reify == null) {
                SetVar intersection = numSetVar("Intersection", ir.getEnv(), ir.getKer(), ir.getCard());
                post(_intersection(operands, intersection));
                return intersection;
            }
            return _intersection(operands, reify);
        }

        @Override
        public Object visit(IrSetUnion ir, SetVar reify) {
            SetVar[] operands = compile(ir.getOperands());
            if (reify == null) {
                SetVar union = numSetVar("Union", ir.getEnv(), ir.getKer(), ir.getCard());
                post(_union(operands, union, ir.isDisjoint()));
                return union;
            }
            return _union(operands, reify, ir.isDisjoint());
        }

        @Override
        public Object visit(IrOffset ir, SetVar reify) {
            SetVar set = compile(ir.getSet());
            if (reify == null) {
                SetVar offset = numSetVar("Offset", ir.getEnv(), ir.getKer());
                offset.setCard(set.getCard());
                post(_offset(set, offset, ir.getOffset()));
                return offset;
            }
            return _offset(set, reify, ir.getOffset());
        }

        @Override
        public Object visit(IrMask ir, SetVar reify) {
            SetVar set = compile(ir.getSet());
            if (reify == null) {
                SetVar mask = numSetVar("Mask", ir.getEnv(), ir.getKer(), ir.getCard());
                post(_mask(set, mask, ir.getFrom(), ir.getTo()));
                return mask;
            }
            return _mask(set, reify, ir.getFrom(), ir.getTo());
        }

        @Override
        public Object visit(IrSetTernary ir, SetVar reify) {
            BoolVar antecedent = compileAsBoolVar(ir.getAntecedent());
            SetVar consequent = compile(ir.getConsequent());
            SetVar alternative = compile(ir.getAlternative());
            if (reify == null) {
                SetVar ternary = numSetVar("Ternary", ir.getEnv(), ir.getKer(), ir.getCard());
                post(Constraints.ternary(antecedent, ternary, consequent, alternative));
                return ternary;
            }
            return Constraints.ternary(antecedent, reify, consequent, alternative);
        }

        @Override
        public Object visit(IrContainsSetTernary ir, SetVar reify) {
            SetVar antecedent = compile(ir.getAntecedent());
            SetVar consequent = compile(ir.getConsequent());
            if (reify == null) {
                SetVar ternary = numSetVar("ContainsTernary", ir.getEnv(), ir.getKer(), ir.getCard());
                post(Constraints.containsImpliesEqualTest(antecedent, ir.getX(), ternary, ternary.getCard(), consequent, consequent.getCard()));
                return ternary;
            }
            return Constraints.containsImpliesEqualTest(antecedent, ir.getX(), reify, reify.getCard(), consequent, consequent.getCard());
        }
    };

    private final IrStringExprVisitor<CStringVar, Object> stringExprCompiler = new IrStringExprVisitor<CStringVar, Object>() {

        @Override
        public Object visit(IrStringVar ir, CStringVar reify) {
            CStringVar string = stringVarMap.get(ir);
            if (string == null) {
                IntVar length = compile(ir.getLengthVar());
                IntVar[] chars = compile(ir.getCharVars());
                string = new CStringVar(chars, length);
                stringVarMap.put(ir, string);
            }
            return string;
        }

        @Override
        public Object visit(IrConcat ir, CStringVar reify) {
            CStringVar left = compile(ir.getLeft());
            CStringVar right = compile(ir.getRight());
            if (reify == null) {
                CStringVar concat = numCstring("Concat", ir.getChars(), ir.getLength());
                post(Constraints.concat(
                        left.getChars(), left.getLength(),
                        right.getChars(), right.getLength(),
                        concat.getChars(), concat.getLength()));
                return concat;
            }
            return Constraints.concat(
                    left.getChars(), left.getLength(),
                    right.getChars(), right.getLength(),
                    reify.getChars(), reify.getLength());
        }

        @Override
        public Object visit(IrStringElement ir, CStringVar reify) {
            IntVar index = compile(ir.getIndex());
            CStringVar[] array = compile(ir.getArray());
            if (reify == null) {
                CStringVar element = numCstring("ElementString", ir.getChars(), ir.getLength());
                post(Constraints.element(index,
                        mapChars(array), mapLength(array),
                        element.getChars(), element.getLength()));
                return element;
            }
            return Constraints.element(index,
                    mapChars(array), mapLength(array),
                    reify.getChars(), reify.getLength());
        }
    };

    private final IrIntArrayExprVisitor<IntVar[], Object> intArrayExprCompiler = new IrIntArrayExprVisitor<IntVar[], Object>() {

        @Override
        public Object visit(IrIntArrayVar ir, IntVar[] a) {
            return compile(ir.getArray());
        }

        @Override
        public Object visit(IrSubarray ir, IntVar[] reify) {
            IntVar[] array = compile(ir.getArray());
            IntVar index = compile(ir.getIndex());
            IntVar sublength = compile(ir.getSublength());
            if (reify == null) {
                // Note that posting the substring constraint will constrain index and sublength.
                throw new UnsupportedOperationException();
            }
            return Constraints.subarray(reify, sublength, index, array);
        }
    };

    private final IrSetArrayExprVisitor<SetVar[], Object> setArrayExprCompiler = new IrSetArrayExprVisitor<SetVar[], Object>() {

        @Override
        public Object visit(IrSetArrayVar ir, SetVar[] reify) {
            return compile(ir.getArray());
        }

        @Override
        public Object visit(IrInverse ir, SetVar[] reify) {
            SetVar[] relation = compile(ir.getRelation());
            if (reify == null) {
                SetVar[] inverse = numSetVars("Inverse", ir.getEnvs(), ir.getKers(), ir.getCards());
                post(model.inverseSet(relation, inverse, 0, 0));
                return inverse;
            }
            return model.inverseSet(relation, reify, 0, 0);
        }

        @Override
        public Object visit(IrTransitiveClosure ir, SetVar[] reify) {
            SetVar[] relation = compile(ir.getRelation());
            if (reify == null) {
                SetVar[] closure = numSetVars("TransitiveClosure", ir.getEnvs(), ir.getKers(), ir.getCards());
                post(Constraints.transitiveClosure(relation, closure, ir.isReflexive()));
                return closure;
            }
            return Constraints.transitiveClosure(relation, reify, ir.isReflexive());
        }
    };

    private Constraint _implies(BoolVar antecedent, Constraint consequent) {
        return _implies(antecedent, consequent.reify());
    }

    private static Constraint _ifThenElse(BoolVar antecedent, BoolVar consequent, BoolVar alternative) {
        return Constraints.ifThenElse(antecedent, consequent, alternative);
    }

    private static Constraint _ifThenElse(BoolVar antecedent, Constraint consequent, Constraint alternative) {
        return _ifThenElse(antecedent, consequent.reify(), alternative.reify());
    }

    private Constraint _sum(IntVar sum, IntVar... vars) {
        for (IntVar var : vars) {
            if (!(var instanceof BoolVar)) {
                return model.sum(vars, "=", sum);
            }
        }
        return _sum(sum, Arrays.copyOf(vars, vars.length, BoolVar[].class));
    }

    private Constraint _sum(IntVar sum, BoolVar... vars) {
        return model.sum(vars, "=", sum);
    }

    private Constraint _scalar(IntVar sum, int[] coefficients, IntVar[] vars) {
        boolean allOnes = true;
        for (int coefficient : coefficients) {
            if (coefficient != 1) {
                allOnes = false;
                break;
            }
        }
        if (allOnes) {
            return _sum(sum, vars);
        }
        return model.scalar(vars, coefficients, "=", sum);
    }

    private Constraint _times(IntVar multiplicand, IntVar multiplier, IntVar product) {
        return model.times(multiplicand, multiplier, product);
    }

    private Constraint _div(IntVar dividend, IntVar divisor, IntVar quotient) {
        return model.div(dividend, divisor, quotient);
    }

    private Constraint _mod(IntVar dividend, IntVar divisor, IntVar remainder) {
        if (divisor.contains(0)) {
            model.post(_arithm(divisor, "!=", 0));
        }
        return model.mod(dividend, divisor, remainder);
    }

    private Constraint _arithm(IntVar var1, String op1, IntVar var2, String op2, int cste) {
        if (cste == 0) {
            switch (Operator.get(op2)) {
                case PL:
                case MN:
                    return model.arithm(var1, op1, var2);
            }
        }
        return model.arithm(var1, op1, var2, op2, cste);
    }

    private Constraint _implies(BoolVar antecedent, IntVar consequent) {
        return _arithm(antecedent, "<=", consequent);
    }

    private Constraint _arithm(IntVar var1, String op, IntVar var2) {
        if (var2.isInstantiated()) {
            return model.arithm(var1, op, var2.getValue());
        }
        return model.arithm(var1, op, var2);
    }

    private Constraint _arithm(IntVar var1, String op, int c) {
        return model.arithm(var1, op, c);
    }

    private static Constraint _element(IntVar index, IntVar[] array, IntVar value) {
        return Constraints.element(value, array, index, 0);
    }

    private static Constraint _equal(SetVar var1, SetVar var2) {
        return Constraints.equal(var1, var1.getCard(), var2, var2.getCard());
    }

    private static Constraint _not_equal(SetVar var1, SetVar var2) {
        if (var1.isInstantiated()) {
            return Constraints.notEqual(var2, var1.getLB().toArray());
        }
        if (var2.isInstantiated()) {
            return Constraints.notEqual(var1, var2.getLB().toArray());
        }
        return Constraints.notEqual(var1, var1.getCard(), var2, var2.getCard());
    }

    private Constraint _all_different(IntVar... vars) {
        return model.allDifferent(vars, "AC");
    }

    private Constraint _within(IntVar var, int low, int high) {
        return model.member(var, low, high);
    }

    private Constraint _within(IntVar var, int[] values) {
        return model.member(var, values);
    }

    private static Constraint _member(IntVar element, SetVar set) {
        return Constraints.member(element, set);
    }

    private static Constraint _not_member(IntVar element, SetVar set) {
        return Constraints.notMember(element, set);
    }

    private Constraint _lex_chain_less(IntVar[]... vars) {
        if (vars.length == 2) {
            return model.lexLess(vars[0], vars[1]);
        }
        return model.lexChainLess(vars);
    }

    private Constraint _lex_chain_less_eq(IntVar[]... vars) {
        if (vars.length == 2) {
            return model.lexLessEq(vars[0], vars[1]);
        }
        return model.lexChainLessEq(vars);
    }

    private static Constraint _lex_chain_channel(IntVar[][] strings, IntVar[] ints) {
        return Constraints.lexChainChannel(strings, ints);
    }

    private static Constraint _difference(SetVar minuend, SetVar subtrahend, SetVar difference) {
        return Constraints.difference(
                minuend, minuend.getCard(),
                subtrahend, subtrahend.getCard(),
                difference, difference.getCard());
    }

    private static Constraint _intersection(SetVar[] operands, SetVar intersection) {
        return Constraints.intersection(
                operands, mapCard(operands),
                intersection, intersection.getCard());
    }

    private static Constraint _union(SetVar[] operands, SetVar union, boolean disjoint) {
        return Constraints.union(
                operands, mapCard(operands),
                union, union.getCard(),
                disjoint);
    }

    private IntVar _offset(IntVar var, int offset) {
        Pair<IntVar, Integer> pair = new Pair<>(var, offset);
        IntVar cache = cachedOffset.get(pair);
        if (cache == null) {
            cache = model.intOffsetView(var, offset);
            cachedOffset.put(pair, cache);
        }
        return cache;
    }

    private Constraint _offset(SetVar set, SetVar offseted, int offset) {
        return model.offSet(set, offseted, offset);
    }

    private IntVar _minus(IntVar var) {
        IntVar minus = cachedMinus.get(var);
        if (minus == null) {
            minus = model.intMinusView(var);
            cachedMinus.put(var, minus);
        }
        return minus;
    }

    private IntVar _scale(IntVar var, int scale) {
        Pair<IntVar, Integer> pair = new Pair<>(var, scale);
        IntVar cache = cachedScale.get(pair);
        if (cache == null) {
            cache = model.intScaleView(var, scale);
            cachedScale.put(pair, cache);
        }
        return cache;
    }

    private static Constraint _mask(SetVar set, SetVar masked, int from, int to) {
        return Constraints.mask(set, set.getCard(), masked, masked.getCard(), from, to);
    }

    private static Constraint _subset_eq(SetVar sub, SetVar sup) {
        return Constraints.subsetEq(sub, sub.getCard(), sup, sup.getCard());
    }

    private static enum Rel {

        EQ("="),
        NQ("!="),
        LT("<"),
        LE("<="),
        GT(">"),
        GE(">=");
        private final String syntax;

        private Rel(String syntax) {
            this.syntax = syntax;
        }

        private String getSyntax() {
            return syntax;
        }

        private static Rel from(IrCompare.Op op) {
            switch (op) {
                case Equal:
                    return EQ;
                case NotEqual:
                    return NQ;
                case LessThanEqual:
                    return LE;
                default:
                    throw new IllegalArgumentException();
            }
        }

        private Rel reverse() {
            switch (this) {
                case LT:
                    return GT;
                case LE:
                    return GE;
                case GT:
                    return LT;
                case GE:
                    return LE;
                default:
                    return this;
            }
        }
    }

    private static enum Arithm {

        ADD("+"),
        MINUS("-");
        private final String syntax;

        private Arithm(String syntax) {
            this.syntax = syntax;
        }

        private String getSyntax() {
            return syntax;
        }

        private int compute(int a, int b) {
            switch (this) {
                case ADD:
                    return a + b;
                case MINUS:
                    return a - b;
                default:
                    throw new IllegalStateException();
            }
        }

        private Arithm negate() {
            switch (this) {
                case ADD:
                    return MINUS;
                case MINUS:
                    return ADD;
                default:
                    throw new IllegalStateException();
            }
        }
    }
    private static final BoolArg ConstraintNoReify = new BoolArg(null, Preference.Constraint);
    private static final BoolArg BoolVarNoReify = new BoolArg(null, Preference.BoolVar);

    private static class BoolArg {

        // The solution needs to be reified in this variable.
        // Set to null if no reification needed.
        private BoolVar reify;
        // The prefered type of solution.
        private final Preference preference;

        BoolArg(BoolVar reify, Preference preference) {
            this.reify = reify;
            this.preference = preference;
        }

        boolean hasReify() {
            return reify != null;
        }

        BoolVar useReify() {
            BoolVar tmp = reify;
            reify = null;
            return tmp;
        }

        Preference getPreference() {
            return preference;
        }
    }

    private static enum Preference {

        Constraint,
        BoolVar;
    }
}
