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

    public IrModule rewrite(IrModule module, T t) {
        List<IrBoolExpr> rewrittenConstraints = new ArrayList<IrBoolExpr>(module.getConstraints().size());
        for (IrBoolExpr constraint : module.getConstraints()) {
            rewrittenConstraints.add(rewrite(constraint, t));
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

    @Override
    public IrBoolExpr visit(IrBoolVar ir, T a) {
        return ir;
    }

    @Override
    public IrBoolExpr visit(IrNot ir, T a) {
        return not(rewrite(ir.getExpr(), a));
    }

    @Override
    public IrBoolExpr visit(IrAnd ir, T a) {
        return and(rewrite(ir.getOperands(), a));
    }

    @Override
    public IrBoolExpr visit(IrLone ir, T a) {
        return lone(rewrite(ir.getOperands(), a));
    }

    @Override
    public IrBoolExpr visit(IrOne ir, T a) {
        return one(rewrite(ir.getOperands(), a));
    }

    @Override
    public IrBoolExpr visit(IrOr ir, T a) {
        return or(rewrite(ir.getOperands(), a));
    }

    @Override
    public IrBoolExpr visit(IrImplies ir, T a) {
        return implies(rewrite(ir.getAntecedent(), a), rewrite(ir.getConsequent(), a));
    }

    @Override
    public IrBoolExpr visit(IrNotImplies ir, T a) {
        return notImplies(rewrite(ir.getAntecedent(), a), rewrite(ir.getConsequent(), a));
    }

    @Override
    public IrBoolExpr visit(IrIfThenElse ir, T a) {
        return ifThenElse(
                rewrite(ir.getAntecedent(), a),
                rewrite(ir.getConsequent(), a),
                rewrite(ir.getAlternative(), a));
    }

    @Override
    public IrBoolExpr visit(IrIfOnlyIf ir, T a) {
        return ifOnlyIf(rewrite(ir.getLeft(), a), rewrite(ir.getRight(), a));
    }

    @Override
    public IrBoolExpr visit(IrXor ir, T a) {
        return xor(rewrite(ir.getLeft(), a), rewrite(ir.getRight(), a));
    }

    @Override
    public IrBoolExpr visit(IrWithin ir, T a) {
        return within(rewrite(ir.getVar(), a), ir.getRange());
    }

    @Override
    public IrBoolExpr visit(IrNotWithin ir, T a) {
        return notWithin(rewrite(ir.getVar(), a), ir.getRange());
    }

    @Override
    public IrBoolExpr visit(IrCompare ir, T a) {
        return compare(rewrite(ir.getLeft(), a), ir.getOp(), rewrite(ir.getRight(), a));
    }

    @Override
    public IrBoolExpr visit(IrSetTest ir, T a) {
        return equality(rewrite(ir.getLeft(), a), ir.getOp(), rewrite(ir.getRight(), a));
    }

    @Override
    public IrBoolExpr visit(IrMember ir, T a) {
        return member(rewrite(ir.getElement(), a), rewrite(ir.getSet(), a));
    }

    @Override
    public IrBoolExpr visit(IrNotMember ir, T a) {
        return notMember(rewrite(ir.getElement(), a), rewrite(ir.getSet(), a));
    }

    @Override
    public IrBoolExpr visit(IrSubsetEq ir, T a) {
        return subsetEq(rewrite(ir.getSubset(), a), rewrite(ir.getSuperset(), a));
    }

    @Override
    public IrBoolExpr visit(IrBoolChannel ir, T a) {
        return boolChannel(rewrite(ir.getBools(), a), rewrite(ir.getSet(), a));
    }

    @Override
    public IrBoolExpr visit(IrIntChannel ir, T a) {
        return intChannel(rewrite(ir.getInts(), a), rewrite(ir.getSets(), a));
    }

    @Override
    public IrBoolExpr visit(IrSortStrings ir, T a) {
        IrIntExpr[][] rewritten = new IrIntExpr[ir.getStrings().length][];
        for (int i = 0; i < rewritten.length; i++) {
            rewritten[i] = rewrite(ir.getStrings()[i], a);
        }
        return sort(rewritten);
    }

    @Override
    public IrBoolExpr visit(IrSortStringsChannel ir, T a) {
        IrIntExpr[][] rewritten = new IrIntExpr[ir.getStrings().length][];
        for (int i = 0; i < rewritten.length; i++) {
            rewritten[i] = rewrite(ir.getStrings()[i], a);
        }
        return sortChannel(rewritten, rewrite(ir.getInts(), a));
    }

    @Override
    public IrBoolExpr visit(IrAllDifferent ir, T a) {
        return allDifferent(rewrite(ir.getOperands(), a));
    }

    @Override
    public IrBoolExpr visit(IrSelectN ir, T a) {
        return selectN(rewrite(ir.getBools(), a), rewrite(ir.getN(), a));
    }

    @Override
    public IrBoolExpr visit(IrFilterString ir, T a) {
        return filterString(rewrite(ir.getSet(), a), ir.getOffset(), rewrite(ir.getString(), a), rewrite(ir.getResult(), a));
    }

    @Override
    public IrBoolExpr visit(IrIntNop ir, T a) {
        return nop(rewrite(ir.getExpr(), a));
    }

    @Override
    public IrBoolExpr visit(IrSetNop ir, T a) {
        return nop(rewrite(ir.getExpr(), a));
    }

    @Override
    public IrIntExpr visit(IrIntVar ir, T a) {
        return ir;
    }

    @Override
    public IrIntExpr visit(IrMinus ir, T a) {
        return minus(rewrite(ir.getExpr(), a));
    }

    @Override
    public IrIntExpr visit(IrCard ir, T a) {
        return card(rewrite(ir.getSet(), a));
    }

    @Override
    public IrIntExpr visit(IrAdd ir, T a) {
        return add(rewrite(ir.getAddends(), a));
    }

    @Override
    public IrIntExpr visit(IrSub ir, T a) {
        return sub(rewrite(ir.getSubtrahends(), a));
    }

    @Override
    public IrIntExpr visit(IrMul ir, T a) {
        return mul(rewrite(ir.getMultiplicand(), a), rewrite(ir.getMultiplier(), a));
    }

    @Override
    public IrIntExpr visit(IrDiv ir, T a) {
        return div(rewrite(ir.getDividend(), a), rewrite(ir.getDivisor(), a));
    }

    @Override
    public IrIntExpr visit(IrElement ir, T a) {
        return element(rewrite(ir.getArray(), a), rewrite(ir.getIndex(), a));
    }

    @Override
    public IrIntExpr visit(IrCount ir, T a) {
        return count(ir.getValue(), rewrite(ir.getArray(), a));
    }

    @Override
    public IrIntExpr visit(IrSetSum ir, T a) {
        return sum(rewrite(ir.getSet(), a));
    }

    @Override
    public IrIntExpr visit(IrTernary ir, T a) {
        return ternary(rewrite(ir.getAntecedent(), a), rewrite(ir.getConsequent(), a), rewrite(ir.getAlternative(), a));
    }

    @Override
    public IrSetExpr visit(IrSetVar ir, T a) {
        return ir;
    }

    @Override
    public IrSetExpr visit(IrSingleton ir, T a) {
        return singleton(rewrite(ir.getValue(), a));
    }

    @Override
    public IrSetExpr visit(IrArrayToSet ir, T a) {
        return arrayToSet(rewrite(ir.getArray(), a), ir.getGlobalCardinality());
    }

    @Override
    public IrSetExpr visit(IrJoinRelation ir, T a) {
        return joinRelation(rewrite(ir.getTake(), a), rewrite(ir.getChildren(), a), ir.isInjective());
    }

    @Override
    public IrSetExpr visit(IrJoinFunction ir, T a) {
        return joinFunction(rewrite(ir.getTake(), a), rewrite(ir.getRefs(), a), ir.getGlobalCardinality());
    }

    @Override
    public IrSetExpr visit(IrSetDifference ir, T a) {
        return difference(rewrite(ir.getMinuend(), a), rewrite(ir.getSubtrahend(), a));
    }

    @Override
    public IrSetExpr visit(IrSetIntersection ir, T a) {
        return intersection(rewrite(ir.getOperands(), a));
    }

    @Override
    public IrSetExpr visit(IrSetUnion ir, T a) {
        return union(rewrite(ir.getOperands(), a));
    }

    @Override
    public IrSetExpr visit(IrOffset ir, T a) {
        return offset(rewrite(ir.getSet(), a), ir.getOffset());
    }

    @Override
    public IrSetExpr visit(IrSetTernary ir, T a) {
        return ternary(rewrite(ir.getAntecedent(), a), rewrite(ir.getConsequent(), a), rewrite(ir.getAlternative(), a));
    }
}
