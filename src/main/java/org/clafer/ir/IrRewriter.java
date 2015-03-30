package org.clafer.ir;

import org.clafer.common.Util;
import static org.clafer.ir.Irs.*;

/**
 *
 * @param <T> the parameter type
 * @author jimmy
 */
public abstract class IrRewriter<T>
        implements IrIntExprVisitor<T, IrIntExpr>,
        IrSetExprVisitor<T, IrSetExpr>,
        IrStringExprVisitor<T, IrStringExpr>,
        IrIntArrayExprVisitor<T, IrIntArrayExpr>,
        IrSetArrayExprVisitor<T, IrSetArrayExpr> {

    protected static <T> boolean changed(T t1, T t2) {
        if (t1 == t2) {
            return false;
        }
        assert !t1.equals(t2) : "Likely not optimized, the rewriter duplicated an object. Possible false negative.";
        return true;
    }

    protected static <T> boolean changed(T[] t1, T[] t2) {
        if (t1 == t2) {
            return false;
        }
        if (t1.length != t2.length) {
            return true;
        }
        for (int i = 0; i < t1.length; i++) {
            if (changed(t1[i], t2[i])) {
                return true;
            }
        }
        return false;
    }

    protected static <T> boolean changed(T[][] t1, T[][] t2) {
        if (t1 == t2) {
            return false;
        }
        if (t1.length != t2.length) {
            return true;
        }
        for (int i = 0; i < t1.length; i++) {
            if (changed(t1[i], t2[i])) {
                return true;
            }
        }
        return false;
    }

    public IrModule rewrite(IrModule module, T t) {
        IrModule optModule = new IrModule(module.getConstraints().size());
        for (IrBoolExpr constraint : module.getConstraints()) {
            optModule.addConstraint(rewrite(constraint, t));
        }
        return optModule;
    }

    public IrBoolExpr rewrite(IrBoolExpr expr, T t) {
        return (IrBoolExpr) expr.accept(this, t);
    }

    public IrBoolExpr[] rewrite(IrBoolExpr[] exprs, T t) {
        IrBoolExpr[] rewritten = new IrBoolExpr[exprs.length];
        for (int i = 0; i < rewritten.length; i++) {
            rewritten[i] = rewrite(exprs[i], t);
        }
        return rewritten;
    }

    public IrIntExpr rewrite(IrIntExpr expr, T t) {
        return expr.accept(this, t);
    }

    public IrIntExpr[] rewrite(IrIntExpr[] exprs, T t) {
        IrIntExpr[] rewritten = new IrIntExpr[exprs.length];
        for (int i = 0; i < rewritten.length; i++) {
            rewritten[i] = rewrite(exprs[i], t);
        }
        return rewritten;
    }

    public IrIntExpr[][] rewrite(IrIntExpr[][] exprs, T t) {
        IrIntExpr[][] rewritten = new IrIntExpr[exprs.length][];
        for (int i = 0; i < rewritten.length; i++) {
            rewritten[i] = rewrite(exprs[i], t);
        }
        return rewritten;
    }

    public IrSetExpr rewrite(IrSetExpr expr, T t) {
        return expr.accept(this, t);
    }

    public IrSetExpr[] rewrite(IrSetExpr[] exprs, T t) {
        IrSetExpr[] rewritten = new IrSetExpr[exprs.length];
        for (int i = 0; i < rewritten.length; i++) {
            rewritten[i] = rewrite(exprs[i], t);
        }
        return rewritten;
    }

    public IrSetExpr[][] rewrite(IrSetExpr[][] exprs, T t) {
        IrSetExpr[][] rewritten = new IrSetExpr[exprs.length][];
        for (int i = 0; i < rewritten.length; i++) {
            rewritten[i] = rewrite(exprs[i], t);
        }
        return rewritten;
    }

    public IrStringExpr rewrite(IrStringExpr expr, T t) {
        return expr.accept(this, t);
    }

    public IrStringExpr[] rewrite(IrStringExpr[] exprs, T t) {
        IrStringExpr[] rewritten = new IrStringExpr[exprs.length];
        for (int i = 0; i < rewritten.length; i++) {
            rewritten[i] = rewrite(exprs[i], t);
        }
        return rewritten;
    }

    public IrStringExpr[][] rewrite(IrStringExpr[][] exprs, T t) {
        IrStringExpr[][] rewritten = new IrStringExpr[exprs.length][];
        for (int i = 0; i < rewritten.length; i++) {
            rewritten[i] = rewrite(exprs[i], t);
        }
        return rewritten;
    }

    public IrIntArrayExpr rewrite(IrIntArrayExpr expr, T t) {
        return expr.accept(this, t);
    }

    public IrSetArrayExpr rewrite(IrSetArrayExpr expr, T t) {
        return expr.accept(this, t);
    }

    @Override
    public IrIntExpr visit(IrRegister ir, T a) {
        IrVar variable = ir.getVariable();
        if (variable instanceof IrBoolVar) {
            variable = (IrVar) rewrite((IrBoolVar) variable, a);
        } else if (variable instanceof IrIntVar) {
            variable = (IrVar) rewrite((IrIntVar) variable, a);
        } else if (variable instanceof IrSetVar) {
            variable = (IrVar) rewrite((IrSetVar) variable, a);
        } else {
            variable = (IrVar) rewrite((IrStringVar) variable, a);
        }
        return changed(ir.getVariable(), variable)
                ? new IrRegister(variable)
                : ir;
    }

    @Override
    public IrBoolVar visit(IrBoolVar ir, T a) {
        return ir;
    }

    @Override
    public IrBoolExpr visit(IrNot ir, T a) {
        IrBoolExpr expr = rewrite(ir.getExpr(), a);
        return changed(ir.getExpr(), expr)
                ? not(expr)
                : ir;
    }

    @Override
    public IrBoolExpr visit(IrAnd ir, T a) {
        IrBoolExpr[] operands = rewrite(ir.getOperands(), a);
        return changed(ir.getOperands(), operands)
                ? and(operands)
                : ir;
    }

    @Override
    public IrBoolExpr visit(IrLone ir, T a) {
        IrBoolExpr[] operands = rewrite(ir.getOperands(), a);
        return changed(ir.getOperands(), operands)
                ? lone(operands)
                : ir;
    }

    @Override
    public IrBoolExpr visit(IrOne ir, T a) {
        IrBoolExpr[] operands = rewrite(ir.getOperands(), a);
        return changed(ir.getOperands(), operands)
                ? one(operands)
                : ir;
    }

    @Override
    public IrBoolExpr visit(IrOr ir, T a) {
        IrBoolExpr[] operands = rewrite(ir.getOperands(), a);
        return changed(ir.getOperands(), operands)
                ? or(operands)
                : ir;
    }

    @Override
    public IrBoolExpr visit(IrImplies ir, T a) {
        IrBoolExpr antecedent = rewrite(ir.getAntecedent(), a);
        IrBoolExpr consequent = rewrite(ir.getConsequent(), a);
        return changed(ir.getAntecedent(), antecedent)
                || changed(ir.getConsequent(), consequent)
                        ? implies(antecedent, consequent)
                        : ir;
    }

    @Override
    public IrBoolExpr visit(IrNotImplies ir, T a) {
        IrBoolExpr antecedent = rewrite(ir.getAntecedent(), a);
        IrBoolExpr consequent = rewrite(ir.getConsequent(), a);
        return changed(ir.getAntecedent(), antecedent)
                || changed(ir.getConsequent(), consequent)
                        ? notImplies(antecedent, consequent)
                        : ir;
    }

    @Override
    public IrBoolExpr visit(IrIfThenElse ir, T a) {
        IrBoolExpr antecedent = rewrite(ir.getAntecedent(), a);
        IrBoolExpr consequent = rewrite(ir.getConsequent(), a);
        IrBoolExpr alternative = rewrite(ir.getAlternative(), a);
        return changed(ir.getAntecedent(), antecedent)
                || changed(ir.getConsequent(), consequent)
                || changed(ir.getAlternative(), alternative)
                        ? ifThenElse(antecedent, consequent, alternative)
                        : ir;
    }

    @Override
    public IrBoolExpr visit(IrIfOnlyIf ir, T a) {
        IrBoolExpr left = rewrite(ir.getLeft(), a);
        IrBoolExpr right = rewrite(ir.getRight(), a);
        return changed(ir.getLeft(), left) || changed(ir.getRight(), right)
                ? ifOnlyIf(left, right)
                : ir;
    }

    @Override
    public IrBoolExpr visit(IrXor ir, T a) {
        IrBoolExpr left = rewrite(ir.getLeft(), a);
        IrBoolExpr right = rewrite(ir.getRight(), a);
        return changed(ir.getLeft(), left) || changed(ir.getRight(), right)
                ? xor(left, right)
                : ir;
    }

    @Override
    public IrBoolExpr visit(IrWithin ir, T a) {
        IrIntExpr value = rewrite(ir.getValue(), a);
        return changed(ir.getValue(), value)
                ? within(value, ir.getRange())
                : ir;
    }

    @Override
    public IrBoolExpr visit(IrCompare ir, T a) {
        IrIntExpr left = rewrite(ir.getLeft(), a);
        IrIntExpr right = rewrite(ir.getRight(), a);
        return changed(ir.getLeft(), left) || changed(ir.getRight(), right)
                ? compare(left, ir.getOp(), right)
                : ir;
    }

    @Override
    public IrBoolExpr visit(IrSetEquality ir, T a) {
        IrSetExpr left = rewrite(ir.getLeft(), a);
        IrSetExpr right = rewrite(ir.getRight(), a);
        return changed(ir.getLeft(), left) || changed(ir.getRight(), right)
                ? equality(left, ir.getOp(), right)
                : ir;
    }

    @Override
    public IrIntExpr visit(IrStringCompare ir, T a) {
        IrStringExpr left = rewrite(ir.getLeft(), a);
        IrStringExpr right = rewrite(ir.getRight(), a);
        return changed(ir.getLeft(), left) || changed(ir.getRight(), right)
                ? compare(left, ir.getOp(), right)
                : ir;
    }

    @Override
    public IrBoolExpr visit(IrMember ir, T a) {
        IrIntExpr element = rewrite(ir.getElement(), a);
        IrSetExpr set = rewrite(ir.getSet(), a);
        return changed(ir.getElement(), element) || changed(ir.getSet(), set)
                ? member(element, set)
                : ir;
    }

    @Override
    public IrBoolExpr visit(IrNotMember ir, T a) {
        IrIntExpr element = rewrite(ir.getElement(), a);
        IrSetExpr set = rewrite(ir.getSet(), a);
        return changed(ir.getElement(), element) || changed(ir.getSet(), set)
                ? notMember(element, set)
                : ir;
    }

    @Override
    public IrBoolExpr visit(IrSubsetEq ir, T a) {
        IrSetExpr subset = rewrite(ir.getSubset(), a);
        IrSetExpr superset = rewrite(ir.getSuperset(), a);
        return changed(ir.getSubset(), subset) || changed(ir.getSuperset(), superset)
                ? subsetEq(subset, superset)
                : ir;
    }

    @Override
    public IrBoolExpr visit(IrBoolChannel ir, T a) {
        IrBoolExpr[] bools = rewrite(ir.getBools(), a);
        IrSetExpr set = rewrite(ir.getSet(), a);
        return changed(ir.getBools(), bools) || changed(ir.getSet(), set)
                ? boolChannel(bools, set)
                : ir;
    }

    @Override
    public IrBoolExpr visit(IrIntChannel ir, T a) {
        IrIntExpr[] ints = rewrite(ir.getInts(), a);
        IrSetExpr[] sets = rewrite(ir.getSets(), a);
        return changed(ir.getInts(), ints) || changed(ir.getSets(), sets)
                ? intChannel(ints, sets)
                : ir;
    }

    @Override
    public IrBoolExpr visit(IrSortStrings ir, T a) {
        IrIntExpr[][] strings = rewrite(ir.getStrings(), a);
        return changed(ir.getStrings(), strings)
                ? (ir.isStrict() ? sortStrict(strings) : sort(strings))
                : ir;
    }

    @Override
    public IrBoolExpr visit(IrSortSets ir, T a) {
        IrSetExpr[] sets = rewrite(ir.getSets(), a);
        return changed(ir.getSets(), sets)
                ? sort(sets)
                : ir;
    }

    @Override
    public IrBoolExpr visit(IrSortStringsChannel ir, T a) {
        IrIntExpr[][] strings = rewrite(ir.getStrings(), a);
        IrIntExpr[] ints = rewrite(ir.getInts(), a);
        return changed(ir.getStrings(), strings) || changed(ir.getInts(), ints)
                ? sortChannel(strings, ints)
                : ir;
    }

    @Override
    public IrBoolExpr visit(IrAllDifferent ir, T a) {
        IrIntExpr[] operands = rewrite(ir.getOperands(), a);
        return changed(ir.getOperands(), operands)
                ? allDifferent(operands)
                : ir;
    }

    @Override
    public IrBoolExpr visit(IrSelectN ir, T a) {
        IrBoolExpr[] bools = rewrite(ir.getBools(), a);
        IrIntExpr n = rewrite(ir.getN(), a);
        return changed(ir.getBools(), bools) || changed(ir.getN(), n)
                ? selectN(bools, n)
                : ir;
    }

    @Override
    public IrIntExpr visit(IrAcyclic ir, T a) {
        IrIntExpr[] edges = rewrite(ir.getEdges(), a);
        return changed(ir.getEdges(), edges)
                ? acyclic(edges)
                : ir;
    }

    @Override
    public IrIntExpr visit(IrUnreachable ir, T a) {
        IrIntExpr[] edges = rewrite(ir.getEdges(), a);
        return changed(ir.getEdges(), edges)
                ? unreachable(edges, ir.getFrom(), ir.getTo())
                : ir;
    }

    @Override
    public IrBoolExpr visit(IrFilterString ir, T a) {
        IrSetExpr set = rewrite(ir.getSet(), a);
        IrIntExpr[] string = rewrite(ir.getString(), a);
        IrIntExpr[] result = rewrite(ir.getResult(), a);
        return changed(ir.getSet(), set)
                || changed(ir.getString(), string)
                || changed(ir.getResult(), result)
                        ? filterString(set, ir.getOffset(), string, result)
                        : ir;
    }

    @Override
    public IrIntExpr visit(IrPrefix ir, T a) {
        IrStringExpr prefix = rewrite(ir.getPrefix(), a);
        IrStringExpr word = rewrite(ir.getWord(), a);
        return changed(ir.getPrefix(), prefix)
                || changed(ir.getWord(), word)
                        ? prefix(prefix, word)
                        : ir;
    }

    @Override
    public IrIntExpr visit(IrSuffix ir, T a) {
        IrStringExpr suffix = rewrite(ir.getSuffix(), a);
        IrStringExpr word = rewrite(ir.getWord(), a);
        return changed(ir.getSuffix(), suffix)
                || changed(ir.getWord(), word)
                        ? suffix(suffix, word)
                        : ir;
    }

    @Override
    public IrIntVar visit(IrIntVar ir, T a) {
        return ir;
    }

    @Override
    public IrIntExpr visit(IrMinus ir, T a) {
        IrIntExpr expr = rewrite(ir.getExpr(), a);
        return changed(ir.getExpr(), expr)
                ? minus(expr)
                : ir;
    }

    @Override
    public IrIntExpr visit(IrCard ir, T a) {
        IrSetExpr set = rewrite(ir.getSet(), a);
        return changed(ir.getSet(), set)
                ? card(set)
                : ir;
    }

    @Override
    public IrIntExpr visit(IrAdd ir, T a) {
        IrIntExpr[] addends = rewrite(ir.getAddends(), a);
        return changed(ir.getAddends(), addends)
                ? add(Util.cons(constant(ir.getOffset()), addends))
                : ir;
    }

    @Override
    public IrIntExpr visit(IrMul ir, T a) {
        IrIntExpr multiplicand = rewrite(ir.getMultiplicand(), a);
        IrIntExpr multiplier = rewrite(ir.getMultiplier(), a);
        return changed(ir.getMultiplicand(), multiplicand) || changed(ir.getMultiplier(), multiplier)
                ? mul(multiplicand, multiplier, ir.getIntRange())
                : ir;
    }

    @Override
    public IrIntExpr visit(IrDiv ir, T a) {
        IrIntExpr dividend = rewrite(ir.getDividend(), a);
        IrIntExpr divisor = rewrite(ir.getDivisor(), a);
        return changed(ir.getDividend(), dividend) || changed(ir.getDivisor(), divisor)
                ? div(dividend, divisor)
                : ir;
    }

    @Override
    public IrIntExpr visit(IrMod ir, T a) {
        IrIntExpr dividend = rewrite(ir.getDividend(), a);
        IrIntExpr divisor = rewrite(ir.getDivisor(), a);
        return changed(ir.getDividend(), dividend) || changed(ir.getDivisor(), divisor)
                ? mod(dividend, divisor)
                : ir;
    }

    @Override
    public IrIntExpr visit(IrElement ir, T a) {
        IrIntArrayExpr array = rewrite(ir.getArray(), a);
        IrIntExpr index = rewrite(ir.getIndex(), a);
        return changed(ir.getArray(), array) || changed(ir.getIndex(), index)
                ? element(array, index)
                : ir;
    }

    @Override
    public IrIntExpr visit(IrCount ir, T a) {
        IrIntExpr[] array = rewrite(ir.getArray(), a);
        return changed(ir.getArray(), array)
                ? count(ir.getValue(), array)
                : ir;
    }

    @Override
    public IrIntExpr visit(IrCountNotEqual ir, T a) {
        IrIntArrayExpr array = rewrite(ir.getArray(), a);
        return changed(ir.getArray(), array)
                ? countNotEqual(ir.getValue(), array)
                : ir;
    }

    @Override
    public IrIntExpr visit(IrSetMax ir, T a) {
        IrSetExpr set = rewrite(ir.getSet(), a);
        return changed(ir.getSet(), set)
                ? max(set, ir.getDefaultValue())
                : ir;
    }

    @Override
    public IrIntExpr visit(IrSetSum ir, T a) {
        IrSetExpr set = rewrite(ir.getSet(), a);
        return changed(ir.getSet(), set)
                ? sum(set)
                : ir;
    }

    @Override
    public IrIntExpr visit(IrTernary ir, T a) {
        IrBoolExpr antecedent = rewrite(ir.getAntecedent(), a);
        IrIntExpr consequent = rewrite(ir.getConsequent(), a);
        IrIntExpr alternative = rewrite(ir.getAlternative(), a);
        return changed(ir.getAntecedent(), antecedent)
                || changed(ir.getConsequent(), consequent)
                || changed(ir.getAlternative(), alternative)
                        ? ternary(antecedent, consequent, alternative)
                        : ir;
    }

    @Override
    public IrIntExpr visit(IrLength ir, T a) {
        IrStringExpr string = rewrite(ir.getString(), a);
        return changed(ir.getString(), string)
                ? length(string)
                : ir;
    }

    @Override
    public IrSetVar visit(IrSetVar ir, T a) {
        IrIntVar card = (IrIntVar) rewrite(ir.getCardVar(), a);
        return changed(ir.getCardVar(), card)
                ? set(ir.getName(), ir.getEnv(), ir.getKer(), card)
                : ir;
    }

    @Override
    public IrSetExpr visit(IrSingleton ir, T a) {
        IrIntExpr value = rewrite(ir.getValue(), a);
        return changed(ir.getValue(), value)
                ? singleton(value)
                : ir;
    }

    @Override
    public IrSetExpr visit(IrArrayToSet ir, T a) {
        IrIntExpr[] array = rewrite(ir.getArray(), a);
        return changed(ir.getArray(), array)
                ? arrayToSet(array, ir.getGlobalCardinality())
                : ir;
    }

    @Override
    public IrSetExpr visit(IrSetElement ir, T a) {
        IrSetArrayExpr array = rewrite(ir.getArray(), a);
        IrIntExpr index = rewrite(ir.getIndex(), a);
        return changed(ir.getArray(), array) || changed(ir.getIndex(), index)
                ? element(array, index)
                : ir;
    }

    @Override
    public IrSetExpr visit(IrJoinRelation ir, T a) {
        IrSetExpr take = rewrite(ir.getTake(), a);
        IrSetArrayExpr children = rewrite(ir.getChildren(), a);
        return changed(ir.getTake(), take) || changed(ir.getChildren(), children)
                ? joinRelation(take, children, ir.isInjective())
                : ir;
    }

    @Override
    public IrSetExpr visit(IrJoinFunction ir, T a) {
        IrSetExpr take = rewrite(ir.getTake(), a);
        IrIntArrayExpr refs = rewrite(ir.getRefs(), a);
        return changed(ir.getTake(), take) || changed(ir.getRefs(), refs)
                ? joinFunction(take, refs, ir.getGlobalCardinality())
                : ir;
    }

    @Override
    public IrSetExpr visit(IrSetDifference ir, T a) {
        IrSetExpr minuend = rewrite(ir.getMinuend(), a);
        IrSetExpr subtrahend = rewrite(ir.getSubtrahend(), a);
        return changed(ir.getMinuend(), minuend) || changed(ir.getSubtrahend(), subtrahend)
                ? difference(minuend, subtrahend)
                : ir;
    }

    @Override
    public IrSetExpr visit(IrSetIntersection ir, T a) {
        IrSetExpr[] operands = rewrite(ir.getOperands(), a);
        return changed(ir.getOperands(), operands)
                ? intersection(operands)
                : ir;
    }

    @Override
    public IrSetExpr visit(IrSetUnion ir, T a) {
        IrSetExpr[] operands = rewrite(ir.getOperands(), a);
        return changed(ir.getOperands(), operands)
                ? union(operands, ir.isDisjoint())
                : ir;
    }

    @Override
    public IrSetExpr visit(IrOffset ir, T a) {
        IrSetExpr set = rewrite(ir.getSet(), a);
        return changed(ir.getSet(), set)
                ? offset(set, ir.getOffset())
                : ir;
    }

    @Override
    public IrSetExpr visit(IrMask ir, T a) {
        IrSetExpr set = rewrite(ir.getSet(), a);
        return changed(ir.getSet(), set)
                ? mask(set, ir.getFrom(), ir.getTo())
                : ir;
    }

    @Override
    public IrSetExpr visit(IrSetTernary ir, T a) {
        IrBoolExpr antecedent = rewrite(ir.getAntecedent(), a);
        IrSetExpr consequent = rewrite(ir.getConsequent(), a);
        IrSetExpr alternative = rewrite(ir.getAlternative(), a);
        return changed(ir.getAntecedent(), antecedent)
                || changed(ir.getConsequent(), consequent)
                || changed(ir.getAlternative(), alternative)
                        ? ternary(antecedent, consequent, alternative)
                        : ir;
    }

    @Override
    public IrStringVar visit(IrStringVar ir, T a) {
        IrIntVar[] chars = Util.<IrIntVar>cast(rewrite(ir.getCharVars(), a));
        IrIntVar length = (IrIntVar) rewrite(ir.getLengthVar(), a);
        return changed(ir.getCharVars(), chars)
                || changed(ir.getLengthVar(), length)
                        ? string(ir.getName(), chars, length)
                        : ir;
    }

    @Override
    public IrStringExpr visit(IrConcat ir, T a) {
        IrStringExpr left = rewrite(ir.getLeft(), a);
        IrStringExpr right = rewrite(ir.getRight(), a);
        return changed(ir.getLeft(), left) || changed(ir.getRight(), right)
                ? concat(left, right)
                : ir;
    }

    @Override
    public IrStringExpr visit(IrStringElement ir, T a) {
        IrStringExpr[] array = rewrite(ir.getArray(), a);
        IrIntExpr index = rewrite(ir.getIndex(), a);
        return changed(ir.getArray(), array) || changed(ir.getIndex(), index)
                ? element(array, index)
                : ir;
    }

    @Override
    public IrIntArrayExpr visit(IrIntArrayVar ir, T a) {
        IrIntExpr[] array = rewrite(ir.getArray(), a);
        return changed(ir.getArray(), array)
                ? array(array)
                : ir;
    }

    @Override
    public IrSetArrayExpr visit(IrSetArrayVar ir, T a) {
        IrSetExpr[] array = rewrite(ir.getArray(), a);
        return changed(ir.getArray(), array)
                ? array(array)
                : ir;
    }

    @Override
    public IrSetArrayExpr visit(IrInverse ir, T a) {
        IrSetArrayExpr relation = rewrite(ir.getRelation(), a);
        return changed(ir.getRelation(), relation)
                ? inverse(relation, ir.getEnvs().length)
                : ir;
    }

    @Override
    public IrSetArrayExpr visit(IrTransitiveClosure ir, T a) {
        IrSetArrayExpr relation = rewrite(ir.getRelation(), a);
        return changed(ir.getRelation(), relation)
                ? transitiveClosure(relation, ir.isReflexive())
                : ir;
    }
}
