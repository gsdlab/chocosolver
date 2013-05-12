package org.clafer.ir;

import gnu.trove.iterator.TIntIterator;
import gnu.trove.set.hash.TIntHashSet;
import java.util.Arrays;
import org.clafer.Check;
import org.clafer.ir.IrDomain.IrBoundDomain;
import org.clafer.ir.IrDomain.IrEnumDomain;

/**
 *
 * @author jimmy
 */
public class IrJoinRef implements IrSetExpr {

    private final IrSetExpr take;
    private final IrIntExpr[] refs;

    IrJoinRef(IrSetExpr take, IrIntExpr[] refs) {
        this.take = Check.notNull(take);
        this.refs = Check.noNulls(refs);
    }

    public IrSetExpr getTake() {
        return take;
    }

    public IrIntExpr[] getRefs() {
        return refs;
    }

    @Override
    public IrDomain getEnv() {
        TIntIterator iter = take.getEnv().iterator();
        if (iter.hasNext()) {
            IrDomain domain = refs[iter.next()].getDomain();
            int low = domain.getLowerBound();
            int high = domain.getUpperBound();
            while (iter.hasNext()) {
                domain = refs[iter.next()].getDomain();
                low = Math.min(low, domain.getLowerBound());
                high = Math.max(high, domain.getUpperBound());
            }
            return new IrBoundDomain(low, high);
        }
        return Irs.EmptyDomain;
    }

    @Override
    public IrDomain getKer() {
        TIntIterator iter = take.getKer().iterator();
        TIntHashSet values = new TIntHashSet(0);
        while (iter.hasNext()) {
            Integer constant = IrUtil.getConstant(refs[iter.next()]);
            if (constant != null) {
                values.add(constant.intValue());
            }
        }
        return values.isEmpty() ? Irs.EmptyDomain : new IrEnumDomain(values.toArray());
    }

    @Override
    public IrDomain getCard() {
        IrDomain takeCard = take.getCard();
        int lowTakeCard = takeCard.getLowerBound();
        int highTakeCard = takeCard.getUpperBound();
        return lowTakeCard == 0
                ? new IrBoundDomain(0, highTakeCard)
                : new IrBoundDomain(1, highTakeCard);
    }

    @Override
    public <A, B> B accept(IrSetExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public String toString() {
        return take + "." + Arrays.toString(refs);
    }
}
