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
import org.clafer.ir.IrBoolDomain;
import org.clafer.ir.IrBoolExpr;
import org.clafer.ir.IrBoolExprVisitor;
import org.clafer.ir.IrBoolVar;
import org.clafer.ir.IrCard;
import org.clafer.ir.IrCompare;
import org.clafer.ir.IrConcat;
import org.clafer.ir.IrCount;
import org.clafer.ir.IrDiv;
import org.clafer.ir.IrDomain;
import org.clafer.ir.IrElement;
import org.clafer.ir.IrElementString;
import org.clafer.ir.IrExpr;
import org.clafer.ir.IrFilterString;
import org.clafer.ir.IrIfOnlyIf;
import org.clafer.ir.IrIfThenElse;
import org.clafer.ir.IrImplies;
import org.clafer.ir.IrIntChannel;
import org.clafer.ir.IrIntConstant;
import org.clafer.ir.IrIntExpr;
import org.clafer.ir.IrIntExprVisitor;
import org.clafer.ir.IrIntVar;
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
import org.clafer.ir.IrNotWithin;
import org.clafer.ir.IrOffset;
import org.clafer.ir.IrOne;
import org.clafer.ir.IrOr;
import org.clafer.ir.IrPrefix;
import org.clafer.ir.IrRegister;
import org.clafer.ir.IrSelectN;
import org.clafer.ir.IrSetDifference;
import org.clafer.ir.IrSetExpr;
import org.clafer.ir.IrSetExprVisitor;
import org.clafer.ir.IrSetIntersection;
import org.clafer.ir.IrSetSum;
import org.clafer.ir.IrSetTernary;
import org.clafer.ir.IrSetTest;
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
import org.clafer.ir.IrUnreachable;
import org.clafer.ir.IrUtil;
import org.clafer.ir.IrVar;
import org.clafer.ir.IrWithin;
import org.clafer.ir.IrXor;
import org.clafer.ir.Irs;
import org.clafer.ir.analysis.CoalesceException;
import org.clafer.ir.analysis.Coalescer;
import org.clafer.ir.analysis.CommonSubexpression;
import org.clafer.ir.analysis.DuplicateConstraints;
import org.clafer.ir.analysis.Optimizer;
import solver.Solver;
import solver.constraints.Constraint;
import solver.constraints.ICF;
import solver.constraints.Operator;
import solver.constraints.set.SCF;
import solver.variables.BoolVar;
import solver.variables.IntVar;
import solver.variables.SetVar;
import solver.variables.VF;

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
            try {
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
            } catch (CoalesceException e) {
                // Compile anyways?
            }
            optModule = DuplicateConstraints.removeDuplicates(optModule);
        }

        commonSubexpressions.addAll(CommonSubexpression.findCommonSubexpressions(optModule));

        for (IrBoolExpr constraint : optModule.getConstraints()) {
            Constraint c = compileAsConstraint(constraint);
            if (c.equals(solver.TRUE)) {
                assert constraint instanceof IrRegister;
            } else {
                post(c);
            }
        }

        return new IrSolutionMap(
                coalescedIntVars, intVarMap,
                coalescedSetVars, mapSet(setVarMap));
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
    private final Map<IrSetVar, CSet> setVarMap = new HashMap<>();
    private final Map<IrStringVar, CString> stringVarMap = new HashMap<>();
    private final Map<TIntHashSet, SetVar> cachedSetConstants = new HashMap<>();
    private final Map<IntVar, IntVar> cachedMinus = new HashMap<>();
    private final Map<Pair<IntVar, Integer>, IntVar> cachedOffset = new HashMap<>();
    private final Set<IrExpr> commonSubexpressions = new HashSet<>();
    private final Map<IrIntExpr, IntVar> cachedCommonIntSubexpressions = new HashMap<>();
    private final Map<IrSetExpr, CSet> cachedCommonSetSubexpressions = new HashMap<>();
    private final Map<IrStringExpr, CString> cachedCommonStringSubexpressions = new HashMap<>();

    private void post(Constraint constraint) {
        assert (!solver.TRUE.equals(constraint));
        solver.post(constraint);
    }

    private BoolVar boolVar(String name, IrBoolDomain domain) {
        switch (domain) {
            case TrueDomain:
                return VF.one(solver);
            case FalseDomain:
                return VF.zero(solver);
            default:
                return VF.bool(name, solver);
        }
    }

    private IntVar intVar(String name, IrDomain domain) {
        if (domain.size() == 1) {
            int constant = domain.getLowBound();
            switch (domain.getLowBound()) {
                case 0:
                    return VF.zero(solver);
                case 1:
                    return VF.one(solver);
                default:
                    return VF.fixed(constant, solver);
            }
        }
        if (domain.getLowBound() == 0 && domain.getHighBound() == 1) {
            return VF.bool(name, solver);
        }
        if (domain.isBounded()) {
            return VF.enumerated(name, domain.getLowBound(), domain.getHighBound(), solver);
        }
        return VF.enumerated(name, domain.getValues(), solver);
    }

    private SetVar setVar(String name, IrDomain env, IrDomain ker) {
        assert IrUtil.isSubsetOf(ker, env);
        if (env.size() == ker.size()) {
            int[] values = ker.getValues();
            TIntHashSet valueSet = new TIntHashSet(values);
            SetVar var = cachedSetConstants.get(valueSet);
            if (var == null) {
                var = VF.fixed(Arrays.toString(values), values, solver);
                cachedSetConstants.put(valueSet, var);
            }
            return var;
        }
        return VF.set(name, env.getValues(), ker.getValues(), solver);
    }

    private CSet cset(String name, IrDomain env, IrDomain ker, IrDomain card) {
        SetVar set = setVar(name, env, ker);
        return new CSet(set, intVar("|" + name + "|", card));
    }

    private CString cstring(String name, IrDomain[] chars, IrDomain length) {
        IntVar[] $chars = new IntVar[chars.length];
        for (int i = 0; i < $chars.length; i++) {
            $chars[i] = intVar(name + "[" + i + "]", chars[i]);
        }
        IntVar $length = intVar(name + "@Length", length);
        post(Constraints.length($chars, $length));
        return new CString($chars, $length);
    }

    private BoolVar numBoolVar(String name) {
        return boolVar(name + "#" + varNum++, IrBoolDomain.BoolDomain);
    }

    private IntVar numIntVar(String name, IrDomain domain) {
        return intVar(name + "#" + varNum++, domain);
    }

    private SetVar numSetVar(String name, IrDomain env, IrDomain ker) {
        return setVar(name + "#" + varNum++, env, ker);
    }

    private CSet numCset(String name, IrDomain env, IrDomain ker, IrDomain card) {
        return cset(name + "#" + varNum++, env, ker, card);
    }

    private CString numCstring(String name, IrDomain[] chars, IrDomain length) {
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

    private CSet getSetVar(IrSetVar var) {
        CSet setVar = setVarMap.get(var);
        if (setVar == null) {
            setVar = new CSet(
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

    private Constraint compileAsConstraint(IrSetExpr expr, CSet reify) {
        Object result = commonSubexpressions.contains(expr)
                ? compile(expr)
                : expr.accept(setExprCompiler, reify);
        if (result instanceof CSet) {
            CSet set = (CSet) result;
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
        if (a instanceof IrIntConstant) {
            IrIntConstant constant = (IrIntConstant) a;
            return _arithm(compile(b), op.reverse().getSyntax(), constant.getValue());
        }
        if (b instanceof IrIntConstant) {
            IrIntConstant constant = (IrIntConstant) b;
            return _arithm(compile(a), op.getSyntax(), constant.getValue());
        }
        switch (op) {
            case EQ:
                if (b instanceof IrBoolVar) {
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

    private CSet compile(IrSetExpr expr) {
        CSet set = cachedCommonSetSubexpressions.get(expr);
        if (set == null) {
            set = (CSet) expr.accept(setExprCompiler, null);
            if (commonSubexpressions.contains(expr)) {
                cachedCommonSetSubexpressions.put(expr, set);
            }
        }
        return set;
    }

    private CSet[] compile(IrSetExpr[] exprs) {
        CSet[] vars = new CSet[exprs.length];
        for (int i = 0; i < vars.length; i++) {
            vars[i] = compile(exprs[i]);
        }
        return vars;
    }

    private CString compile(IrStringExpr expr) {
        CString set = cachedCommonStringSubexpressions.get(expr);
        if (set == null) {
            set = (CString) expr.accept(stringExprCompiler, null);
            if (commonSubexpressions.contains(expr)) {
                cachedCommonStringSubexpressions.put(expr, set);
            }
        }
        return set;
    }

    private CString[] compile(IrStringExpr[] exprs) {
        CString[] vars = new CString[exprs.length];
        for (int i = 0; i < vars.length; i++) {
            vars[i] = compile(exprs[i]);
        }
        return vars;
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
            IrDomain range = ir.getRange();
            if (range.isBounded()) {
                return _within(var, range.getLowBound(), range.getHighBound());
            }
            return _within(var, range.getValues());
        }

        @Override
        public Object visit(IrNotWithin ir, BoolArg a) {
            IntVar var = compile(ir.getValue());
            IrDomain range = ir.getRange();
            if (range.isBounded()) {
                return _not_within(var, range.getLowBound(), range.getHighBound());
            }
            return _not_within(var, range.getValues());
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
                        return compileArithm(addends[0], Arithm.ADD, addends[1], op, constant.intValue() - add.getOffset());
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
                        return compileArithm(constant.intValue() - add.getOffset(), op, addends[0], Arithm.ADD, addends[1]);
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
        public Object visit(IrSetTest ir, BoolArg a) {
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
            CString string1 = compile(ir.getLeft());
            CString string2 = compile(ir.getRight());
            switch (ir.getOp()) {
                case Equal:
                    return Constraints.equal(
                            string1.getChars(), string1.getLength(),
                            string2.getChars(), string2.getLength());
                case NotEqual:
                    return Constraints.notEqual(
                            string1.getChars(), string1.getLength(),
                            string2.getChars(), string2.getLength());
                default:
                    throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
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
            CSet set = compile(ir.getSet());
            return SCF.bool_channel(bools, set.getSet(), 0);
        }

        @Override
        public Constraint visit(IrIntChannel ir, BoolArg a) {
            IntVar[] ints = compile(ir.getInts());
            CSet[] sets = compile(ir.getSets());
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
            CSet[] sets = compile(ir.getSets());
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
            CSet set = compile(ir.getSet());
            int offset = ir.getOffset();
            IntVar[] string = compile(ir.getString());
            IntVar[] result = compile(ir.getResult());
            return Constraints.filterString(set.getSet(), set.getCard(), offset, string, result);
        }

        @Override
        public Object visit(IrPrefix ir, BoolArg a) {
            CString prefix = compile(ir.getPrefix());
            CString word = compile(ir.getWord());
            return Constraints.prefix(
                    prefix.getChars(), prefix.getLength(),
                    word.getChars(), word.getLength());
        }

        @Override
        public Object visit(IrSuffix ir, BoolArg a) {
            CString suffix = compile(ir.getSuffix());
            CString word = compile(ir.getWord());
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
                minus = VF.minus(compile(ir.getExpr()));
                cachedMinus.put(expr, minus);
            }
            return minus;
        }

        @Override
        public Object visit(IrCard ir, IntVar reify) {
            CSet set = compile(ir.getSet());
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
                    return VF.fixed(offset, solver);
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
                        IntVar sum = numIntVar("Sum", IrUtil.offset(ir.getDomain(), -offset));
                        post(_scalar(sum, coefficients, operands));
                        return _offset(sum, offset);
                    }
                    if (offset != 0) {
                        coefficients = Util.cons(1, coefficients);
                        operands = Util.cons(VF.fixed(offset, solver), operands);
                    }
                    return _scalar(reify, coefficients, operands);
            }
        }

        @Override
        public Object visit(IrMul ir, IntVar reify) {
            IrIntExpr multiplicand = ir.getMultiplicand();
            IrIntExpr multiplier = ir.getMultiplier();
            Integer multiplicandConstant = IrUtil.getConstant(multiplicand);
            Integer multiplierConstant = IrUtil.getConstant(multiplier);
            if (multiplicandConstant != null) {
                switch (multiplicandConstant.intValue()) {
                    case 0:
                        return compileAsConstraint(multiplicand, reify);
                    case 1:
                        return compileAsConstraint(multiplier, reify);
                    default:
                        if (multiplicandConstant.intValue() >= -1) {
                            return VF.scale(compile(multiplier), multiplicandConstant.intValue());
                        }
                }
            }
            if (multiplierConstant != null) {
                switch (multiplierConstant.intValue()) {
                    case 0:
                        return compileAsConstraint(multiplier, reify);
                    case 1:
                        return compileAsConstraint(multiplicand, reify);
                    default:
                        if (multiplierConstant.intValue() >= -1) {
                            return VF.scale(compile(multiplicand), multiplierConstant.intValue());
                        }
                }
            }
            if (reify == null) {
                IntVar product = numIntVar("Mul", ir.getDomain());
                post(_times(compile(multiplicand), compile(multiplier), product));
                return product;
            }
            return _times(compile(multiplicand), compile(multiplier), reify);
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
                post(_count(ir.getValue(), array, count));
                return count;
            }
            return _count(ir.getValue(), array, reify);
        }

        @Override
        public Object visit(IrSetSum ir, IntVar reify) {
            CSet set = compile(ir.getSet());
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
            CString string = compile(ir.getString());
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
        public Object visit(IrNotWithin ir, IntVar a) {
            return compileBool(ir, a);
        }

        @Override
        public Object visit(IrCompare ir, IntVar a) {
            return compileBool(ir, a);
        }

        @Override
        public Object visit(IrSetTest ir, IntVar a) {
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
    private final IrSetExprVisitor<CSet, Object> setExprCompiler = new IrSetExprVisitor<CSet, Object>() {
        @Override
        public Object visit(IrSetVar ir, CSet reify) {
            return getSetVar(ir);
        }

        @Override
        public Object visit(IrSingleton ir, CSet reify) {
            IntVar value = compile(ir.getValue());
            if (reify == null) {
                SetVar singleton = numSetVar("Singleton", ir.getEnv(), ir.getKer());
                post(Constraints.singleton(value, singleton));
                return new CSet(singleton, VF.one(solver));
            }
            return Constraints.singleton(value, reify.getSet(), reify.getCard());
        }

        @Override
        public Object visit(IrArrayToSet ir, CSet reify) {
            IntVar[] array = compile(ir.getArray());
            if (reify == null) {
                CSet set = numCset("ArrayToSet", ir.getEnv(), ir.getKer(), ir.getCard());
                post(Constraints.arrayToSet(array, set.getSet(), set.getCard(), ir.getGlobalCardinality()));
                return set;
            }
            return Constraints.arrayToSet(array, reify.getSet(), reify.getCard(), ir.getGlobalCardinality());
        }

        @Override
        public Object visit(IrJoinRelation ir, CSet reify) {
            CSet take = compile(ir.getTake());
            CSet[] children = compile(ir.getChildren());
            if (reify == null) {
                CSet joinRelation = numCset("JoinRelation", ir.getEnv(), ir.getKer(), ir.getCard());
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
        public Object visit(IrJoinFunction ir, CSet reify) {
            CSet take = compile(ir.getTake());
            IntVar[] refs = compile(ir.getRefs());
            if (reify == null) {
                CSet joinFunction = numCset("JoinFunction", ir.getEnv(), ir.getKer(), ir.getCard());
                post(Constraints.joinFunction(take.getSet(), take.getCard(), refs, joinFunction.getSet(), joinFunction.getCard(), ir.getGlobalCardinality()));
                return joinFunction;
            }
            return Constraints.joinFunction(take.getSet(), take.getCard(), refs, reify.getSet(), reify.getCard(), ir.getGlobalCardinality());
        }

        @Override
        public Object visit(IrSetDifference ir, CSet reify) {
            CSet minuend = compile(ir.getMinuend());
            CSet subtrahend = compile(ir.getSubtrahend());
            if (reify == null) {
                CSet difference = numCset("Difference", ir.getEnv(), ir.getKer(), ir.getCard());
                post(_difference(minuend, subtrahend, difference));
                return difference;
            }
            return _difference(minuend, subtrahend, reify);
        }

        @Override
        public Object visit(IrSetIntersection ir, CSet reify) {
            CSet[] operands = compile(ir.getOperands());
            if (reify == null) {
                CSet intersection = numCset("Intersection", ir.getEnv(), ir.getKer(), ir.getCard());
                post(_intersection(operands, intersection));
                return intersection;
            }
            return _intersection(operands, reify);
        }

        @Override
        public Object visit(IrSetUnion ir, CSet reify) {
            CSet[] operands = compile(ir.getOperands());
            if (reify == null) {
                CSet union = numCset("Union", ir.getEnv(), ir.getKer(), ir.getCard());
                post(_union(operands, union, ir.isDisjoint()));
                return union;
            }
            return _union(operands, reify, ir.isDisjoint());
        }

        @Override
        public Object visit(IrOffset ir, CSet reify) {
            CSet set = compile(ir.getSet());
            if (reify == null) {
                SetVar offset = numSetVar("Offset", ir.getEnv(), ir.getKer());
                post(_offset(set.getSet(), offset, ir.getOffset()));
                return new CSet(offset, set.getCard());
            }
            return _offset(set.getSet(), reify.getSet(), ir.getOffset());
        }

        @Override
        public Object visit(IrMask ir, CSet reify) {
            CSet set = compile(ir.getSet());
            if (reify == null) {
                CSet mask = numCset("Mask", ir.getEnv(), ir.getKer(), ir.getCard());
                post(_mask(set, mask, ir.getFrom(), ir.getTo()));
                return mask;
            }
            return _mask(set, reify, ir.getFrom(), ir.getTo());
        }

        @Override
        public Object visit(IrSetTernary ir, CSet reify) {
            BoolVar antecedent = compileAsBoolVar(ir.getAntecedent());
            CSet consequent = compile(ir.getConsequent());
            CSet alternative = compile(ir.getAlternative());
            if (reify == null) {
                CSet ternary = numCset("Ternary", ir.getEnv(), ir.getKer(), ir.getCard());
                post(_ifThenElse(antecedent,
                        _equal(ternary, consequent),
                        _equal(ternary, alternative)));
                return ternary;
            }
            return _ifThenElse(antecedent,
                    _equal(reify, consequent),
                    _equal(reify, alternative));
        }
    };

    private IrStringExprVisitor<CString, Object> stringExprCompiler = new IrStringExprVisitor<CString, Object>() {

        private boolean lengthEntailed(IntVar[] chars, IntVar length) {
            if (!length.isInstantiated()) {
                return false;
            }
            int l = length.getValue();
            for (int i = 0; i < l; i++) {
                if (chars[i].contains(0)) {
                    return false;
                }
            }
            for (int i = l; i < chars.length; i++) {
                if (!chars[i].isInstantiatedTo(0)) {
                    return false;
                }
            }
            return l <= chars.length;
        }

        @Override
        public Object visit(IrStringVar ir, CString reify) {
            CString string = stringVarMap.get(ir);
            if (string == null) {
                IntVar length = compile(ir.getLengthVar());
                IntVar[] chars = compile(ir.getCharVars());
                if (!lengthEntailed(chars, length)) {
                    post(Constraints.length(chars, length));
                }
                string = new CString(chars, length);
                stringVarMap.put(ir, string);
            }
            return string;
        }

        @Override
        public Object visit(IrConcat ir, CString reify) {
            CString left = compile(ir.getLeft());
            CString right = compile(ir.getRight());
            if (reify == null) {
                CString concat = numCstring("Concat", ir.getChars(), ir.getLength());
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
        public Object visit(IrElementString ir, CString reify) {
            IntVar index = compile(ir.getIndex());
            CString[] array = compile(ir.getArray());
            if (reify == null) {
                CString element = numCstring("ElementString", ir.getChars(), ir.getLength());
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

    private static Constraint _implies(BoolVar antecedent, Constraint consequent) {
        return _implies(antecedent, consequent.reif());
    }

    private static Constraint _ifThenElse(BoolVar antecedent, BoolVar consequent, BoolVar alternative) {
        return Constraints.ifThenElse(antecedent, consequent, alternative);
    }

    private static Constraint _ifThenElse(BoolVar antecedent, Constraint consequent, Constraint alternative) {
        Constraint thenClause = _implies(antecedent, consequent);
        Constraint elseClause = _implies(antecedent.not(), alternative);
        return _arithm(thenClause.reif(), "+", elseClause.reif(), "=", 2);
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
        return ICF.eucl_div(dividend, divisor, quotient);
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

    private static Constraint _count(int value, IntVar[] array, IntVar count) {
        return ICF.count(value, array, count);
    }

    private static Constraint _equal(CSet var1, CSet var2) {
        return Constraints.equal(var1.getSet(), var1.getCard(), var2.getSet(), var2.getCard());
    }

    private static Constraint _not_equal(CSet var1, CSet var2) {
        if (var1.getSet().isInstantiated()) {
            return Constraints.notEqual(var2.getSet(), var1.getSet().getValue());
        }
        if (var2.getSet().isInstantiated()) {
            return Constraints.notEqual(var1.getSet(), var2.getSet().getValue());
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

    private static Constraint _not_within(IntVar var, int low, int high) {
        return ICF.not_member(var, low, high);
    }

    private static Constraint _not_within(IntVar var, int[] values) {
        return ICF.not_member(var, values);
    }

    private static Constraint _member(IntVar element, SetVar set) {
        return SCF.member(element, set);
    }

    private static Constraint _not_member(IntVar element, SetVar set) {
        return Constraints.notMember(element, set);
    }

    private static Constraint _lex_chain_less(IntVar[]  
        ... vars) {
        if (vars.length == 2) {
            return ICF.lex_less(vars[0], vars[1]);
        }
        return ICF.lex_chain_less(vars);
    }

    private static Constraint _lex_chain_less_eq(IntVar[]  
        ... vars) {
        if (vars.length == 2) {
            return ICF.lex_less_eq(vars[0], vars[1]);
        }
        return ICF.lex_chain_less_eq(vars);
    }

    private static Constraint _lex_chain_channel(IntVar[][] strings, IntVar[] ints) {
        return Constraints.lexChainChannel(strings, ints);
    }

    private static Constraint _difference(CSet minuend, CSet subtrahend, CSet difference) {
        return Constraints.difference(
                minuend.getSet(), minuend.getCard(),
                subtrahend.getSet(), subtrahend.getCard(),
                difference.getSet(), difference.getCard());
    }

    private static Constraint _intersection(CSet[] operands, CSet intersection) {
        return Constraints.intersection(mapSet(operands), mapCard(operands), intersection.getSet(), intersection.getCard());
    }

    private static Constraint _union(CSet[] operands, CSet union, boolean disjoint) {
        return Constraints.union(
                mapSet(operands), mapCard(operands),
                union.getSet(), union.getCard(),
                disjoint);
    }

    private IntVar _offset(IntVar var, int offset) {
        Pair<IntVar, Integer> pair = new Pair<>(var, offset);
        IntVar cache = cachedOffset.get(pair);
        if (cache == null) {
            cache = VF.offset(var, offset);
            cachedOffset.put(pair, cache);
        }
        return cache;
    }

    private static Constraint _offset(SetVar set, SetVar offseted, int offset) {
        return SCF.offSet(set, offseted, offset);
    }

    private static Constraint _mask(CSet set, CSet masked, int from, int to) {
        return Constraints.mask(set.getSet(), set.getCard(), masked.getSet(), masked.getCard(), from, to);
    }

    private static Constraint _subset_eq(CSet sub, CSet sup) {
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

    private class CSet {

        private final SetVar set;
        private final IntVar card;

        CSet(SetVar set, IntVar card) {
            this.set = set;
            this.card = card;

            if (!(set.isInstantiated() && card.isInstantiatedTo(set.getKernelSize()))) {
                post(SCF.cardinality(set, card));
            }
        }

        SetVar getSet() {
            return set;
        }

        IntVar getCard() {
            return card;
        }
    }

    private static SetVar[] mapSet(CSet[] sets) {
        SetVar[] vars = new SetVar[sets.length];
        for (int i = 0; i < sets.length; i++) {
            vars[i] = sets[i].getSet();
        }
        return vars;
    }

    private static <T> Map<T, SetVar> mapSet(Map<T, CSet> sets) {
        Map<T, SetVar> map = new HashMap<>(sets.size());
        for (Entry<T, CSet> set : sets.entrySet()) {
            map.put(set.getKey(), set.getValue().getSet());
        }
        return map;
    }

    private static IntVar[] mapCard(CSet[] sets) {
        IntVar[] vars = new IntVar[sets.length];
        for (int i = 0; i < sets.length; i++) {
            vars[i] = sets[i].getCard();
        }
        return vars;
    }

    private class CString {

        private final IntVar[] chars;
        private final IntVar length;

        CString(IntVar[] chars, IntVar length) {
            this.chars = chars;
            this.length = length;
        }

        IntVar[] getChars() {
            return chars;
        }

        IntVar getLength() {
            return length;
        }
    }

    private static IntVar[][] mapChars(CString[] strings) {
        IntVar[][] vars = new IntVar[strings.length][];
        for (int i = 0; i < strings.length; i++) {
            vars[i] = strings[i].getChars();
        }
        return vars;
    }

    private static IntVar[] mapLength(CString[] strings) {
        IntVar[] vars = new IntVar[strings.length];
        for (int i = 0; i < strings.length; i++) {
            vars[i] = strings[i].getLength();
        }
        return vars;
    }
}
