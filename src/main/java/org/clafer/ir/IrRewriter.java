package org.clafer.ir;

import java.util.ArrayList;
import java.util.List;
import org.clafer.collection.Pair;
import static org.clafer.ir.Irs.*;

/**
 *
 * @param <T> the parameter type
 * @author jimmy
 */
public abstract class IrRewriter<T>
        implements IrIntExprVisitor<T, IrIntExpr>, IrSetExprVisitor<T, IrSetExpr> {
public boolean a;
    private static <T> boolean changed(T t1, T t2) {
        return t1 != t2;
    }

    private static <T> boolean changed(T[] t1, T[] t2) {
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

    private static <T> boolean changed(T[][] t1, T[][] t2) {
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
        List<IrBoolExpr> rewrittenConstraints = new ArrayList<IrBoolExpr>(module.getConstraints().size());
        for (IrBoolExpr constraint : module.getConstraints()) {
            IrBoolExpr e = rewrite(constraint, t);
//            if(a && constraint.toString().contains("c708_cc_system_on_off@Membership#0")) {
//                System.out.println(constraint);
//                System.out.println("    " + e);
//            }
            rewrittenConstraints.add(e);
        }
        return module.withConstraints(rewrittenConstraints);
    }

    public IrModule rewriteAndNonNops(IrModule module, T t) {
        Pair<List<IrNop>, List<IrBoolExpr>> pair = partitionNops(module.getConstraints());
        IrModule rewritten = module.withConstraints(rewrite(and(pair.getSnd()), t));
        rewritten.addConstraints(pair.getFst());
        return rewritten;
    }

    private static Pair<List<IrNop>, List<IrBoolExpr>> partitionNops(List<IrBoolExpr> constraints) {
        List<IrNop> nops = new ArrayList<IrNop>();
        List<IrBoolExpr> nonNops = new ArrayList<IrBoolExpr>(constraints.size());

        for (IrBoolExpr constraint : constraints) {
            if (constraint instanceof IrNop) {
                nops.add((IrNop) constraint);
            } else {
                nonNops.add(constraint);
            }
        }
        return new Pair<List<IrNop>, List<IrBoolExpr>>(nops, nonNops);
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

    @Override
    public IrBoolExpr visit(IrBoolVar ir, T a) {
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
    public IrBoolExpr visit(IrNotWithin ir, T a) {
        IrIntExpr value = rewrite(ir.getValue(), a);
        return changed(ir.getValue(), value)
                ? notWithin(value, ir.getRange())
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
    public IrBoolExpr visit(IrSetTest ir, T a) {
        IrSetExpr left = rewrite(ir.getLeft(), a);
        IrSetExpr right = rewrite(ir.getRight(), a);
        return changed(ir.getLeft(), left) || changed(ir.getRight(), right)
                ? equality(left, ir.getOp(), right)
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
                ? sort(strings)
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
    public IrBoolExpr visit(IrIntNop ir, T a) {
        IrIntExpr expr = rewrite(ir.getExpr(), a);
        return changed(ir.getExpr(), expr)
                ? nop(expr)
                : ir;
    }

    @Override
    public IrBoolExpr visit(IrSetNop ir, T a) {
        IrSetExpr expr = rewrite(ir.getExpr(), a);
        return changed(ir.getExpr(), expr)
                ? nop(expr)
                : ir;
    }

    @Override
    public IrIntExpr visit(IrIntVar ir, T a) {
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
                ? add(addends)
                : ir;
    }

    @Override
    public IrIntExpr visit(IrSub ir, T a) {
        IrIntExpr[] subtrahends = rewrite(ir.getSubtrahends(), a);
        return changed(ir.getSubtrahends(), subtrahends)
                ? sub(subtrahends)
                : ir;
    }

    @Override
    public IrIntExpr visit(IrMul ir, T a) {
        IrIntExpr multiplicand = rewrite(ir.getMultiplicand(), a);
        IrIntExpr multiplier = rewrite(ir.getMultiplier(), a);
        return changed(ir.getMultiplicand(), multiplicand) || changed(ir.getMultiplier(), multiplier)
                ? mul(multiplicand, multiplier)
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
    public IrIntExpr visit(IrElement ir, T a) {
        IrIntExpr[] array = rewrite(ir.getArray(), a);
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
    public IrSetExpr visit(IrSetVar ir, T a) {
        return ir;
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
    public IrSetExpr visit(IrJoinRelation ir, T a) {
        IrSetExpr take = rewrite(ir.getTake(), a);
        IrSetExpr[] children = rewrite(ir.getChildren(), a);
        return changed(ir.getTake(), take) || changed(ir.getChildren(), children)
                ? joinRelation(take, children, ir.isInjective())
                : ir;
    }

    @Override
    public IrSetExpr visit(IrJoinFunction ir, T a) {
        IrSetExpr take = rewrite(ir.getTake(), a);
        IrIntExpr[] refs = rewrite(ir.getRefs(), a);
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
                ? union(operands)
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
}
