package org.clafer.ir.compiler;

import gnu.trove.set.hash.TIntHashSet;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import org.clafer.choco.constraint.Constraints;
import org.clafer.collection.Pair;
import org.clafer.collection.Triple;
import org.clafer.common.Check;
import org.clafer.common.Util;
import org.clafer.ir.IrAcyclic;
import org.clafer.ir.IrAdd;
import org.clafer.ir.IrAllDifferent;
import org.clafer.ir.IrAnd;
import org.clafer.ir.IrArrayToSet;
import org.clafer.ir.IrBoolChannel;
import org.clafer.domain.BoolDomain;
import org.clafer.ir.IrBoolExpr;
import org.clafer.ir.IrBoolExprVisitor;
import org.clafer.ir.IrBoolVar;
import org.clafer.ir.IrCard;
import org.clafer.ir.IrCompare;
import org.clafer.ir.IrConcat;
import org.clafer.ir.IrCount;
import org.clafer.ir.IrDiv;
import org.clafer.domain.Domain;
import org.clafer.ir.IrCountNotEqual;
import org.clafer.ir.IrElement;
import org.clafer.ir.IrStringElement;
import org.clafer.ir.IrExpr;
import org.clafer.ir.IrFilterString;
import org.clafer.ir.IrIfOnlyIf;
import org.clafer.ir.IrIfThenElse;
import org.clafer.ir.IrImplies;
import org.clafer.ir.IrIntArrayExpr;
import org.clafer.ir.IrIntArrayExprVisitor;
import org.clafer.ir.IrIntArrayVar;
import org.clafer.ir.IrIntChannel;
import org.clafer.ir.IrIntConstant;
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
import org.clafer.ir.IrModule;
import org.clafer.ir.IrMul;
import org.clafer.ir.IrNot;
import org.clafer.ir.IrNotImplies;
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
import org.clafer.ir.IrSetExpr;
import org.clafer.ir.IrSetExprVisitor;
import org.clafer.ir.IrSetIntersection;
import org.clafer.ir.IrSetSum;
import org.clafer.ir.IrSetTernary;
import org.clafer.ir.IrSetEquality;
import org.clafer.ir.IrSetUnion;
import org.clafer.ir.IrSetVar;
import org.clafer.ir.IrSingleton;
import org.clafer.ir.IrSortSets;
import org.clafer.ir.IrSortStrings;
import org.clafer.ir.IrSortStringsChannel;
import org.clafer.ir.IrStringCompare;
import org.clafer.ir.IrStringExpr;
import org.clafer.ir.IrStringExprVisitor;
import org.clafer.ir.IrStringVar;
import org.clafer.ir.IrSubsetEq;
import org.clafer.ir.IrSuffix;
import org.clafer.ir.IrTernary;
import org.clafer.ir.IrTransitiveClosure;
import org.clafer.ir.IrUnreachable;
import org.clafer.ir.IrUtil;
import org.clafer.ir.IrVar;
import org.clafer.ir.IrWithin;
import org.clafer.ir.IrXor;
import org.clafer.ir.Irs;
import org.clafer.ir.analysis.Coalescer;
import org.clafer.ir.analysis.CommonSubexpression;
import org.clafer.ir.analysis.DuplicateConstraints;
import org.clafer.ir.analysis.LinearEquationOptimizer;
import org.clafer.ir.analysis.Optimizer;
import org.chocosolver.solver.Solver;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.constraints.ICF;
import org.chocosolver.solver.constraints.Operator;
import org.chocosolver.solver.constraints.set.SCF;
import org.chocosolver.solver.variables.BoolVar;
import org.chocosolver.solver.variables.CSetVar;
import org.chocosolver.solver.variables.CStringVar;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.solver.variables.SetVar;
import static org.chocosolver.solver.variables.Var.*;
import org.clafer.ir.IrMod;
import org.clafer.ir.IrSetMax;

/**
 * Compile from IR to Choco.
 *
 * @author jimmy
 */
public class IrCompiler {

    private final Solver solver;
    private final boolean coalesceVariables;
    private int varNum = 0;

    private IrCompiler(Solver solver, boolean coalesceVariables) {
        this.solver = Check.notNull(solver);
        this.coalesceVariables = coalesceVariables;
    }

    public static IrSolutionMap compile(IrModule in, Solver out) {
        return compile(in, out, true);
    }

    public static IrSolutionMap compile(IrModule in, Solver out, boolean coalesceVariables) {
        IrCompiler compiler = new IrCompiler(out, coalesceVariables);
        return compiler.compile(in);
    }

    private IrSolutionMap compile(IrModule module) {
        IrModule optModule = Optimizer.optimize(module);

        Map<IrIntVar, IrIntVar> coalescedIntVars = Collections.emptyMap();
        Map<IrSetVar, IrSetVar> coalescedSetVars = Collections.emptyMap();
        if (coalesceVariables) {
            Triple<Map<IrIntVar, IrIntVar>, Map<IrSetVar, IrSetVar>, IrModule> coalesceTriple = Coalescer.coalesce(optModule);
            coalescedIntVars = coalesceTriple.getFst();
            coalescedSetVars = coalesceTriple.getSnd();
            optModule = coalesceTriple.getThd();
            while (!coalesceTriple.getFst().isEmpty()
                    || !coalesceTriple.getSnd().isEmpty()) {
                coalesceTriple = Coalescer.coalesce(optModule);
                coalescedIntVars = compose(coalescedIntVars, coalesceTriple.getFst());
                coalescedSetVars = compose(coalescedSetVars, coalesceTriple.getSnd());
                optModule = coalesceTriple.getThd();
            }
            optModule = DuplicateConstraints.removeDuplicates(optModule);
        }
        optModule = LinearEquationOptimizer.optimize(optModule);
        commonSubexpressions.addAll(CommonSubexpression.findCommonSubexpressions(optModule));

        for (IrBoolExpr constraint : optModule.getConstraints()) {
            Constraint c = compileAsConstraint(constraint);
            if (c.equals(solver.TRUE)) {
                assert constraint instanceof IrRegister;
            } else {
                post(c);
            }
        }

        Map<IrSetVar, SetVar> setVarMapSet = new HashMap<>(setVarMap.size());
        for (Entry<IrSetVar, CSetVar> set : setVarMap.entrySet()) {
            setVarMapSet.put(set.getKey(), set.getValue().getSet());
        }

        return new IrSolutionMap(
                coalescedIntVars, intVarMap,
                coalescedSetVars, setVarMapSet);
    }

    private static <T> Map<T, T> compose(Map<T, T> f1, Map<T, T> f2) {
        if (f1.isEmpty()) {
            return f2;
        }
        if (f2.isEmpty()) {
            return f1;
        }
        Map<T, T> composed = new HashMap<>(f1.size() + f2.size());
        composed.putAll(f2);
        for (Entry<T, T> e : f1.entrySet()) {
            T key = e.getKey();
            T value = f2.get(e.getValue());
            if (value == null) {
                value = e.getValue();
            }
            composed.put(key, value);
        }
        return composed;
    }
    private final Map<IrIntVar, IntVar> intVarMap = new HashMap<>();
    private final Map<IrSetVar, CSetVar> setVarMap = new HashMap<>();
    private final Map<IrStringVar, CStringVar> stringVarMap = new HashMap<>();
    private final Map<TIntHashSet, SetVar> cachedSetConstants = new HashMap<>();
    private final Map<IntVar, IntVar> cachedMinus = new HashMap<>();
    private final Map<Pair<IntVar, Integer>, IntVar> cachedOffset = new HashMap<>();
    private final Set<IrExpr> commonSubexpressions = new HashSet<>();
    private final Map<IrIntExpr, IntVar> cachedCommonIntSubexpressions = new HashMap<>();
    private final Map<IrSetExpr, CSetVar> cachedCommonSetSubexpressions = new HashMap<>();
    private final Map<IrStringExpr, CStringVar> cachedCommonStringSubexpressions = new HashMap<>();
    private final Map<IrIntArrayExpr, IntVar[]> cachedCommonIntArraySubexpressions = new HashMap<>();
    private final Map<IrSetArrayExpr, CSetVar[]> cachedCommonSetArraySubexpressions = new HashMap<>();

    private void post(Constraint constraint) {
        assert (!solver.TRUE.equals(constraint));
        solver.post(constraint);
    }

    private BoolVar boolVar(String name, BoolDomain domain) {
        switch (domain) {
            case TrueDomain:
                return one(solver);
            case FalseDomain:
                return zero(solver);
            default:
                return bool(name, solver);
        }
    }

    private IntVar intVar(String name, Domain domain) {
        if (domain.size() == 1) {
            int constant = domain.getLowBound();
            switch (domain.getLowBound()) {
                case 0:
                    return zero(solver);
                case 1:
                    return one(solver);
                default:
                    return fixed(constant, solver);
            }
        }
        if (domain.getLowBound() == 0 && domain.getHighBound() == 1) {
            return bool(name, solver);
        }
        if (domain.isBounded()) {
            return enumerated(name, domain.getLowBound(), domain.getHighBound(), solver);
        }
        return enumerated(name, domain.getValues(), solver);
    }

    private SetVar setVar(String name, Domain env, Domain ker) {
        if (env.size() == ker.size()) {
            int[] values = ker.getValues();
            TIntHashSet valueSet = new TIntHashSet(values);
            SetVar var = cachedSetConstants.get(valueSet);
            if (var == null) {
                var = fixed(Arrays.toString(values), values, solver);
                cachedSetConstants.put(valueSet, var);
            }
            return var;
        }
        return set(name, env.getValues(), ker.getValues(), solver);
    }

    private CSetVar cset(String name, Domain env, Domain ker, Domain card) {
        SetVar set = setVar(name, env, ker);
        return new CSetVar(set, intVar("|" + name + "|", card));
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

    private CSetVar numCset(String name, Domain env, Domain ker, Domain card) {
        return cset(name + "#" + varNum++, env, ker, card);
    }

    private CSetVar[] numCsets(String name, Domain[] envs, Domain[] kers, Domain[] cards) {
        CSetVar[] sets = new CSetVar[envs.length];
        for (int i = 0; i < sets.length; i++) {
            sets[i] = cset(name + "[" + i + "]#" + varNum++, envs[i], kers[i], cards[i]);
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

    private CSetVar getSetVar(IrSetVar var) {
        CSetVar setVar = setVarMap.get(var);
        if (setVar == null) {
            setVar = new CSetVar(
                    setVar(var.getName(), var.getEnv(), var.getKer()),
                    getIntVar(var.getCardVar()));
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
        return op.reif();
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
                return _arithm(arg.useReify(), "=", constraint.reif());
            }
            return constraint;
        }
        BoolVar var = (BoolVar) result;
        if (arg.hasReify()) {
            return _arithm(arg.useReify(), "=", var);
        }
        return asConstraint(var);
    }

    private Constraint compileAsEqual(IrIntExpr expr, IntVar value, BoolVar reify) {
        Object result = commonSubexpressions.contains(expr)
                ? compile(expr)
                : expr.accept(intExprCompiler, value);
        if (result instanceof IntVar) {
            // The compliation failed to reify, explicitly reify now.
            return _reify_equal(reify, value, (IntVar) result);
        }
        return _arithm(((Constraint) result).reif(), "=", reify);
    }

    private Constraint compileAsNotEqual(IrIntExpr expr, IntVar value, BoolVar reify) {
        Object result = commonSubexpressions.contains(expr)
                ? compile(expr)
                : expr.accept(intExprCompiler, value);
        if (result instanceof IntVar) {
            // The compliation failed to reify, explicitly reify now.
            return _reify_not_equal(reify, value, (IntVar) result);
        }
        return _arithm(((Constraint) result).reif(), "!=", reify);
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

    private Constraint compileAsConstraint(IrSetExpr expr, CSetVar reify) {
        Object result = commonSubexpressions.contains(expr)
                ? compile(expr)
                : expr.accept(setExprCompiler, reify);
        if (result instanceof CSetVar) {
            CSetVar set = (CSetVar) result;
            // The compliation failed to reify, explicitly reify now.
            return _equal(reify, set);
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
        if (a instanceof IrIntConstant) {
            IrIntConstant constant = (IrIntConstant) a;
            // a ◁ b + c <=> b ▷ a - c
            // a ◁ b - c <=> b ▷ a + c
            return compileArithm(b, op1.reverse(), op2.negate().compute(constant.getValue(), c));
        }
        if (b instanceof IrIntConstant) {
            IrIntConstant constant = (IrIntConstant) b;
            return compileArithm(a, op1, op2.compute(constant.getValue(), c));
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
        if (a instanceof IrIntConstant) {
            IrIntConstant constant = (IrIntConstant) a;
            // a + b ◁ c <=> b ◁ c - a
            // a - b ◁ c <=> b ▷ a - c
            return Arithm.ADD.equals(op1)
                    ? compileArithm(b, op2, c - constant.getValue())
                    : compileArithm(b, op2.reverse(), constant.getValue() - c);
        }
        if (b instanceof IrIntConstant) {
            // a + b ◁ c <=> a ◁ c - b
            // a - b ◁ c <=> a ◁ c + b
            IrIntConstant constant = (IrIntConstant) b;
            return compileArithm(a, op2, op1.negate().compute(c, constant.getValue()));
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

    private CSetVar compile(IrSetExpr expr) {
        CSetVar set = cachedCommonSetSubexpressions.get(expr);
        if (set == null) {
            set = (CSetVar) expr.accept(setExprCompiler, null);
            if (commonSubexpressions.contains(expr)) {
                cachedCommonSetSubexpressions.put(expr, set);
            }
        }
        return set;
    }

    private CSetVar[] compile(IrSetExpr[] exprs) {
        CSetVar[] vars = new CSetVar[exprs.length];
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

    private CSetVar[] compile(IrSetArrayExpr expr) {
        CSetVar[] sets = cachedCommonSetArraySubexpressions.get(expr);
        if (sets == null) {
            sets = (CSetVar[]) expr.accept(setArrayExprCompiler, null);
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
            return solver.TRUE;
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
                    return solver.TRUE;
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
                    return compileArithm(operands[0], Arithm.ADD, operands[1], Rel.GE, 1);
                default:
                    return Constraints.or(compileAsBoolVars(ir.getOperands()));
            }
        }

        @Override
        public Object visit(IrImplies ir, BoolArg a) {
            return compileArithm(ir.getAntecedent(), Rel.LE, ir.getConsequent());
        }

        @Override
        public Object visit(IrNotImplies ir, BoolArg a) {
            return compileArithm(ir.getAntecedent(), Rel.GT, ir.getConsequent());
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
            return compileArithm(ir.getLeft(), Rel.EQ, ir.getRight());
        }

        @Override
        public Object visit(IrXor ir, BoolArg a) {
            return compileArithm(ir.getLeft(), Rel.NQ, ir.getRight());
        }

        @Override
        public Object visit(IrWithin ir, BoolArg a) {
            IntVar var = compile(ir.getValue());
            Domain range = ir.getRange();
            if (range.isBounded()) {
                return _within(var, range.getLowBound(), range.getHighBound());
            }
            return _within(var, range.getValues());
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
                    Integer constant = IrUtil.getConstant(right);
                    if (constant != null) {
                        return compileArithm(addends[0], Arithm.ADD, addends[1], op, constant - add.getOffset());
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
                    Integer constant = IrUtil.getConstant(left);
                    if (constant != null) {
                        return compileArithm(constant - add.getOffset(), op, addends[0], Arithm.ADD, addends[1]);
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
                    BoolVar reify = numBoolVar("ReifyEqual");
                    post(right instanceof IrIntVar
                            ? compileAsEqual(left, compile(right), reify)
                            : compileAsEqual(right, compile(left), reify));
                    return reify;
                }
            } else if (IrCompare.Op.NotEqual.equals(op)) {
                if (a.hasReify()) {
                    BoolVar reify = a.useReify();
                    return right instanceof IrIntVar
                            ? compileAsNotEqual(left, compile(right), reify)
                            : compileAsNotEqual(right, compile(left), reify);
                }
                if (a.getPreference().equals(Preference.BoolVar)) {
                    BoolVar reify = numBoolVar("ReifyNotEqual");
                    post(right instanceof IrIntVar
                            ? compileAsNotEqual(left, compile(right), reify)
                            : compileAsNotEqual(right, compile(left), reify));
                    return reify;
                }
            }
            return compileArithm(left, op, right);
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
            return _member(compile(ir.getElement()), compile(ir.getSet()).getSet());
        }

        @Override
        public Object visit(IrNotMember ir, BoolArg a) {
            return _not_member(compile(ir.getElement()), compile(ir.getSet()).getSet());
        }

        @Override
        public Object visit(IrSubsetEq ir, BoolArg a) {
            return _subset_eq(compile(ir.getSubset()), compile(ir.getSuperset()));
        }

        @Override
        public Constraint visit(IrBoolChannel ir, BoolArg a) {
            BoolVar[] bools = compileAsBoolVars(ir.getBools());
            CSetVar set = compile(ir.getSet());
            return SCF.bool_channel(bools, set.getSet(), 0);
        }

        @Override
        public Constraint visit(IrIntChannel ir, BoolArg a) {
            IntVar[] ints = compile(ir.getInts());
            CSetVar[] sets = compile(ir.getSets());
            return Constraints.intChannel(mapSet(sets), ints);
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
            CSetVar[] sets = compile(ir.getSets());
            return Constraints.sortedSets(mapSet(sets), mapCard(sets));
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
        public Object visit(IrUnreachable ir, BoolArg a) {
            IntVar[] edges = compile(ir.getEdges());
            return Constraints.unreachable(edges, ir.getFrom(), ir.getTo());
        }

        @Override
        public Object visit(IrFilterString ir, BoolArg a) {
            CSetVar set = compile(ir.getSet());
            int offset = ir.getOffset();
            IntVar[] string = compile(ir.getString());
            IntVar[] result = compile(ir.getResult());
            return Constraints.filterString(set.getSet(), set.getCard(), offset, string, result);
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
            IntVar minus = cachedMinus.get(expr);
            if (minus == null) {
                minus = minus(compile(ir.getExpr()));
                cachedMinus.put(expr, minus);
            }
            return minus;
        }

        @Override
        public Object visit(IrCard ir, IntVar reify) {
            CSetVar set = compile(ir.getSet());
            return set.getCard();
        }

        private Pair<Integer, IrIntExpr> getCoefficient(IrIntExpr e) {
            if (e instanceof IrMinus) {
                return new Pair<>(-1, ((IrMinus) e).getExpr());
            } else if (e instanceof IrMul) {
                IrMul mul = (IrMul) e;
                Integer constant = IrUtil.getConstant(mul.getMultiplicand());
                if (constant != null) {
                    return new Pair<>(constant, mul.getMultiplier());
                }
                constant = IrUtil.getConstant(mul.getMultiplier());
                if (constant != null) {
                    return new Pair<>(constant, mul.getMultiplicand());
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
                    return fixed(offset, solver);
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
                        operands = Util.cons(fixed(offset, solver), operands);
                    }
                    return _scalar(reify, coefficients, operands);
            }
        }

        @Override
        public Object visit(IrMul ir, IntVar reify) {
            IrIntExpr multiplicand = ir.getMultiplicand();
            IrIntExpr multiplier = ir.getMultiplier();
            if (reify != null) {
                return _times(compile(multiplicand), compile(multiplier), reify);
            }
            Integer multiplicandConstant = IrUtil.getConstant(multiplicand);
            Integer multiplierConstant = IrUtil.getConstant(multiplier);
            if (multiplicandConstant != null) {
                switch (multiplicandConstant) {
                    case 0:
                        return compileAsConstraint(multiplicand, reify);
                    case 1:
                        return compileAsConstraint(multiplier, reify);
                    default:
                        if (multiplicandConstant >= -1) {
                            return scale(compile(multiplier), multiplicandConstant);
                        }
                }
            }
            if (multiplierConstant != null) {
                switch (multiplierConstant) {
                    case 0:
                        return compileAsConstraint(multiplier, reify);
                    case 1:
                        return compileAsConstraint(multiplicand, reify);
                    default:
                        if (multiplierConstant >= -1) {
                            return scale(compile(multiplicand), multiplierConstant);
                        }
                }
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
                post(ICF.count(ir.getValue(), array, count));
                return count;
            }
            return ICF.count(ir.getValue(), array, reify);
        }

        @Override
        public Object visit(IrCountNotEqual ir, IntVar reify) {
            IntVar[] array = compile(ir.getArray());
            if (reify == null) {
                IntVar count = numIntVar("CountNotEqual", ir.getDomain());
                post(Constraints.countNotEqual(ir.getValue(), array, count));
                return count;
            }
            return Constraints.countNotEqual(ir.getValue(), array, reify);
        }

        @Override
        public Object visit(IrSetMax ir, IntVar reify) {
            CSetVar set = compile(ir.getSet());
            if (reify == null) {
                IntVar max = numIntVar("SetMax", ir.getDomain());
                post(Constraints.max(set.getSet(), set.getCard(), max, ir.getDefaultValue()));
                return max;
            }
            return Constraints.max(set.getSet(), set.getCard(), reify, ir.getDefaultValue());
        }

        @Override
        public Object visit(IrSetSum ir, IntVar reify) {
            CSetVar set = compile(ir.getSet());
            if (reify == null) {
                IntVar sum = numIntVar("SetSum", ir.getDomain());
                post(Constraints.setSum(set.getSet(), set.getCard(), sum));
                return sum;
            }
            return Constraints.setSum(set.getSet(), set.getCard(), reify);
        }

        @Override
        public Object visit(IrTernary ir, IntVar reify) {
            BoolVar antecedent = compileAsBoolVar(ir.getAntecedent());
            IntVar consequent = compile(ir.getConsequent());
            IntVar alternative = compile(ir.getAlternative());
            if (reify == null) {
                IntVar ternary = numIntVar("Ternary", ir.getDomain());
                BoolVar reifyConsequent = numBoolVar("ReifyEqual");
                BoolVar reifyAlternative = numBoolVar("ReifyEqual");
                post(_reify_equal(reifyConsequent, consequent, ternary));
                post(_reify_equal(reifyAlternative, alternative, ternary));
                post(_ifThenElse(antecedent, reifyConsequent, reifyAlternative));
                return ternary;
            }
            BoolVar reifyConsequent = numBoolVar("ReifyEqual");
            BoolVar reifyAlternative = numBoolVar("ReifyEqual");
            post(_reify_equal(reifyConsequent, consequent, reify));
            post(_reify_equal(reifyAlternative, alternative, reify));
            return _ifThenElse(antecedent, reifyConsequent, reifyAlternative);
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
        public Object visit(IrImplies ir, IntVar a) {
            return compileBool(ir, a);
        }

        @Override
        public Object visit(IrNotImplies ir, IntVar a) {
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
        public Object visit(IrXor ir, IntVar a) {
            return compileBool(ir, a);
        }

        @Override
        public Object visit(IrWithin ir, IntVar a) {
            return compileBool(ir, a);
        }

        @Override
        public Object visit(IrCompare ir, IntVar a) {
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
        public Object visit(IrUnreachable ir, IntVar a) {
            return compileBool(ir, a);
        }

        @Override
        public Object visit(IrFilterString ir, IntVar a) {
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
    private final IrSetExprVisitor<CSetVar, Object> setExprCompiler = new IrSetExprVisitor<CSetVar, Object>() {
        @Override
        public Object visit(IrSetVar ir, CSetVar reify) {
            return getSetVar(ir);
        }

        @Override
        public Object visit(IrSingleton ir, CSetVar reify) {
            IntVar value = compile(ir.getValue());
            if (reify == null) {
                SetVar singleton = numSetVar("Singleton", ir.getEnv(), ir.getKer());
                post(Constraints.singleton(value, singleton));
                return new CSetVar(singleton, one(solver));
            }
            return Constraints.singleton(value, reify.getSet(), reify.getCard());
        }

        @Override
        public Object visit(IrArrayToSet ir, CSetVar reify) {
            IntVar[] array = compile(ir.getArray());
            if (reify == null) {
                CSetVar set = numCset("ArrayToSet", ir.getEnv(), ir.getKer(), ir.getCard());
                post(Constraints.arrayToSet(array, set.getSet(), set.getCard(), ir.getGlobalCardinality()));
                return set;
            }
            return Constraints.arrayToSet(array, reify.getSet(), reify.getCard(), ir.getGlobalCardinality());
        }

        @Override
        public Object visit(IrSetElement ir, CSetVar reify) {
            CSetVar[] array = compile(ir.getArray());
            IntVar index = compile(ir.getIndex());
            if (reify == null) {
                CSetVar set = numCset("Element", ir.getEnv(), ir.getKer(), ir.getCard());
                post(Constraints.element(index, mapSet(array), mapCard(array), set.getSet(), set.getCard()));
                return set;
            }
            return Constraints.element(index, mapSet(array), mapCard(array), reify.getSet(), reify.getCard());
        }

        @Override
        public Object visit(IrJoinRelation ir, CSetVar reify) {
            CSetVar take = compile(ir.getTake());
            CSetVar[] children = compile(ir.getChildren());
            if (reify == null) {
                CSetVar joinRelation = numCset("JoinRelation", ir.getEnv(), ir.getKer(), ir.getCard());
                if (ir.isInjective()) {
                    post(Constraints.joinInjectiveRelation(take.getSet(), take.getCard(),
                            mapSet(children), mapCard(children), joinRelation.getSet(), joinRelation.getCard()));
                    return joinRelation;
                } else {
                    post(Constraints.joinRelation(take.getSet(), mapSet(children), joinRelation.getSet()));
                    return joinRelation;
                }
            }
            if (ir.isInjective()) {
                return Constraints.joinInjectiveRelation(take.getSet(), take.getCard(),
                        mapSet(children), mapCard(children), reify.getSet(), reify.getCard());
            }
            return Constraints.joinRelation(take.getSet(), mapSet(children), reify.getSet());
        }

        @Override
        public Object visit(IrJoinFunction ir, CSetVar reify) {
            CSetVar take = compile(ir.getTake());
            IntVar[] refs = compile(ir.getRefs());
            if (reify == null) {
                CSetVar joinFunction = numCset("JoinFunction", ir.getEnv(), ir.getKer(), ir.getCard());
                post(Constraints.joinFunction(take.getSet(), take.getCard(), refs, joinFunction.getSet(), joinFunction.getCard(), ir.getGlobalCardinality()));
                return joinFunction;
            }
            return Constraints.joinFunction(take.getSet(), take.getCard(), refs, reify.getSet(), reify.getCard(), ir.getGlobalCardinality());
        }

        @Override
        public Object visit(IrSetDifference ir, CSetVar reify) {
            CSetVar minuend = compile(ir.getMinuend());
            CSetVar subtrahend = compile(ir.getSubtrahend());
            if (reify == null) {
                CSetVar difference = numCset("Difference", ir.getEnv(), ir.getKer(), ir.getCard());
                post(_difference(minuend, subtrahend, difference));
                return difference;
            }
            return _difference(minuend, subtrahend, reify);
        }

        @Override
        public Object visit(IrSetIntersection ir, CSetVar reify) {
            CSetVar[] operands = compile(ir.getOperands());
            if (reify == null) {
                CSetVar intersection = numCset("Intersection", ir.getEnv(), ir.getKer(), ir.getCard());
                post(_intersection(operands, intersection));
                return intersection;
            }
            return _intersection(operands, reify);
        }

        @Override
        public Object visit(IrSetUnion ir, CSetVar reify) {
            CSetVar[] operands = compile(ir.getOperands());
            if (reify == null) {
                CSetVar union = numCset("Union", ir.getEnv(), ir.getKer(), ir.getCard());
                post(_union(operands, union, ir.isDisjoint()));
                return union;
            }
            return _union(operands, reify, ir.isDisjoint());
        }

        @Override
        public Object visit(IrOffset ir, CSetVar reify) {
            CSetVar set = compile(ir.getSet());
            if (reify == null) {
                SetVar offset = numSetVar("Offset", ir.getEnv(), ir.getKer());
                post(_offset(set.getSet(), offset, ir.getOffset()));
                return new CSetVar(offset, set.getCard());
            }
            return _offset(set.getSet(), reify.getSet(), ir.getOffset());
        }

        @Override
        public Object visit(IrMask ir, CSetVar reify) {
            CSetVar set = compile(ir.getSet());
            if (reify == null) {
                CSetVar mask = numCset("Mask", ir.getEnv(), ir.getKer(), ir.getCard());
                post(_mask(set, mask, ir.getFrom(), ir.getTo()));
                return mask;
            }
            return _mask(set, reify, ir.getFrom(), ir.getTo());
        }

        @Override
        public Object visit(IrSetTernary ir, CSetVar reify) {
            BoolVar antecedent = compileAsBoolVar(ir.getAntecedent());
            CSetVar consequent = compile(ir.getConsequent());
            CSetVar alternative = compile(ir.getAlternative());
            if (reify == null) {
                CSetVar ternary = numCset("Ternary", ir.getEnv(), ir.getKer(), ir.getCard());
                post(_implies(antecedent, _equal(ternary, consequent)));
                post(_implies(antecedent.not(), _equal(ternary, alternative)));
                return ternary;
            }
            return _arithm(
                    _implies(antecedent, _equal(reify, consequent)).reif(),
                    "+",
                    _implies(antecedent.not(), _equal(reify, alternative)).reif(),
                    "=", 2);
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
    };

    private final IrSetArrayExprVisitor<CSetVar[], Object> setArrayExprCompiler = new IrSetArrayExprVisitor<CSetVar[], Object>() {

        @Override
        public Object visit(IrSetArrayVar ir, CSetVar[] a) {
            return compile(ir.getArray());
        }

        @Override
        public Object visit(IrInverse ir, CSetVar[] reify) {
            CSetVar[] relation = compile(ir.getRelation());
            if (reify == null) {
                CSetVar[] inverse = numCsets("Inverse", ir.getEnvs(), ir.getKers(), ir.getCards());
                post(SCF.inverse_set(mapSet(relation), mapSet(inverse), 0, 0));
                return inverse;
            }
            return SCF.inverse_set(mapSet(relation), mapSet(reify), 0, 0);
        }

        @Override
        public Object visit(IrTransitiveClosure ir, CSetVar[] reify) {
            CSetVar[] relation = compile(ir.getRelation());
            if (reify == null) {
                CSetVar[] closure = numCsets("TransitiveClosure", ir.getEnvs(), ir.getKers(), ir.getCards());
                post(Constraints.transitiveClosure(mapSet(relation), mapSet(closure), ir.isReflexive()));
                return closure;
            }
            return Constraints.transitiveClosure(mapSet(relation), mapSet(reify), ir.isReflexive());
        }
    };

    private static Constraint _implies(BoolVar antecedent, Constraint consequent) {
        return _implies(antecedent, consequent.reif());
    }

    private static Constraint _ifThenElse(BoolVar antecedent, BoolVar consequent, BoolVar alternative) {
        return Constraints.ifThenElse(antecedent, consequent, alternative);
    }

    private static Constraint _sum(IntVar sum, IntVar... vars) {
        for (IntVar var : vars) {
            if (!(var instanceof BoolVar)) {
                return ICF.sum(vars, sum);
            }
        }
        return _sum(sum, Arrays.copyOf(vars, vars.length, BoolVar[].class));
    }

    private static Constraint _sum(IntVar sum, BoolVar... vars) {
        return ICF.sum(vars, sum);
    }

    private static Constraint _scalar(IntVar sum, int[] coefficients, IntVar[] vars) {
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
        return ICF.scalar(vars, coefficients, sum);
    }

    private static Constraint _times(IntVar multiplicand, IntVar multiplier, IntVar product) {
        return ICF.times(multiplicand, multiplier, product);
    }

    private static Constraint _div(IntVar dividend, IntVar divisor, IntVar quotient) {
        if (divisor.contains(0)) {
            divisor.getSolver().post(_arithm(divisor, "!=", 0));
        }
        return ICF.eucl_div(dividend, divisor, quotient);
    }

    private static Constraint _mod(IntVar dividend, IntVar divisor, IntVar remainder) {
        if (divisor.contains(0)) {
            divisor.getSolver().post(_arithm(divisor, "!=", 0));
        }
        return ICF.mod(dividend, divisor, remainder);
    }

    private static Constraint _arithm(IntVar var1, String op1, IntVar var2, String op2, int cste) {
        if (cste == 0) {
            switch (Operator.get(op2)) {
                case PL:
                    return ICF.arithm(var1, op1, var2);
                case MN:
                    return ICF.arithm(var1, op1, var2);
            }
        }
        return ICF.arithm(var1, op1, var2, op2, cste);
    }

    private static Constraint _implies(BoolVar antecedent, IntVar consequent) {
        return _arithm(antecedent, "<=", consequent);
    }

    private static Constraint _arithm(IntVar var1, String op, IntVar var2) {
        if (var2.isInstantiated()) {
            return ICF.arithm(var1, op, var2.getValue());
        }
        return ICF.arithm(var1, op, var2);
    }

    private static Constraint _arithm(IntVar var1, String op, int c) {
        return ICF.arithm(var1, op, c);
    }

    private Constraint _reify_equal(BoolVar reify, IntVar var1, IntVar var2) {
        if (var1.isInstantiated()) {
            if (var2.isInstantiated()) {
                return _arithm(reify, "=", var1.getValue() == var2.getValue() ? 1 : 0);
            }
            return Constraints.reifyEqual(reify, var2, var1.getValue());
        } else if (var2.isInstantiated()) {
            return Constraints.reifyEqual(reify, var1, var2.getValue());
        } else {
            return Constraints.reifyEqual(reify, var1, var2);
        }
    }

    private Constraint _reify_not_equal(BoolVar reify, IntVar var1, IntVar var2) {
        if (var1.isInstantiated()) {
            if (var2.isInstantiated()) {
                return _arithm(reify, "=", var1.getValue() != var2.getValue() ? 1 : 0);
            }
            return Constraints.reifyNotEqual(reify, var2, var1.getValue());
        } else if (var2.isInstantiated()) {
            return Constraints.reifyNotEqual(reify, var1, var2.getValue());
        } else {
            return Constraints.reifyNotEqual(reify, var1, var2);
        }
    }

    private static Constraint _element(IntVar index, IntVar[] array, IntVar value) {
        return ICF.element(value, array, index, 0);
    }

    private static Constraint _equal(CSetVar var1, CSetVar var2) {
        return Constraints.equal(var1.getSet(), var1.getCard(), var2.getSet(), var2.getCard());
    }

    private static Constraint _not_equal(CSetVar var1, CSetVar var2) {
        if (var1.getSet().isInstantiated()) {
            return Constraints.notEqual(var2.getSet(), var1.getSet().getValues());
        }
        if (var2.getSet().isInstantiated()) {
            return Constraints.notEqual(var1.getSet(), var2.getSet().getValues());
        }
        return Constraints.notEqual(var1.getSet(), var1.getCard(), var2.getSet(), var2.getCard());
    }

    private static Constraint _all_different(IntVar... vars) {
        return ICF.alldifferent(vars, "AC");
    }

    private static Constraint _within(IntVar var, int low, int high) {
        return ICF.member(var, low, high);
    }

    private static Constraint _within(IntVar var, int[] values) {
        return ICF.member(var, values);
    }

    private static Constraint _member(IntVar element, SetVar set) {
        return Constraints.member(element, set);
    }

    private static Constraint _not_member(IntVar element, SetVar set) {
        return Constraints.notMember(element, set);
    }

    private static Constraint _lex_chain_less(IntVar[]... vars) {
        if (vars.length == 2) {
            return ICF.lex_less(vars[0], vars[1]);
        }
        return ICF.lex_chain_less(vars);
    }

    private static Constraint _lex_chain_less_eq(IntVar[]... vars) {
        if (vars.length == 2) {
            return ICF.lex_less_eq(vars[0], vars[1]);
        }
        return ICF.lex_chain_less_eq(vars);
    }

    private static Constraint _lex_chain_channel(IntVar[][] strings, IntVar[] ints) {
        return Constraints.lexChainChannel(strings, ints);
    }

    private static Constraint _difference(CSetVar minuend, CSetVar subtrahend, CSetVar difference) {
        return Constraints.difference(
                minuend.getSet(), minuend.getCard(),
                subtrahend.getSet(), subtrahend.getCard(),
                difference.getSet(), difference.getCard());
    }

    private static Constraint _intersection(CSetVar[] operands, CSetVar intersection) {
        return Constraints.intersection(mapSet(operands), mapCard(operands), intersection.getSet(), intersection.getCard());
    }

    private static Constraint _union(CSetVar[] operands, CSetVar union, boolean disjoint) {
        return Constraints.union(
                mapSet(operands), mapCard(operands),
                union.getSet(), union.getCard(),
                disjoint);
    }

    private IntVar _offset(IntVar var, int offset) {
        Pair<IntVar, Integer> pair = new Pair<>(var, offset);
        IntVar cache = cachedOffset.get(pair);
        if (cache == null) {
            cache = offset(var, offset);
            cachedOffset.put(pair, cache);
        }
        return cache;
    }

    private static Constraint _offset(SetVar set, SetVar offseted, int offset) {
        return SCF.offSet(set, offseted, offset);
    }

    private static Constraint _mask(CSetVar set, CSetVar masked, int from, int to) {
        return Constraints.mask(set.getSet(), set.getCard(), masked.getSet(), masked.getCard(), from, to);
    }

    private static Constraint _subset_eq(CSetVar sub, CSetVar sup) {
        return Constraints.subsetEq(sub.getSet(), sub.getCard(), sup.getSet(), sup.getCard());
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
                case LessThan:
                    return LT;
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
