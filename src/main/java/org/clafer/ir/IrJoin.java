package org.clafer.ir;

import gnu.trove.iterator.TIntIterator;
import gnu.trove.set.hash.TIntHashSet;
import java.util.Arrays;
import org.clafer.Check;
import org.clafer.ir.IrDomain.IrBoundDomain;
import org.clafer.ir.IrDomain.IrEnumDomain;
import util.iterators.IntIterator;

/**
 * Assumption: children are disjoint
 * 
 * @author jimmy
 */
public class IrJoin implements IrSetExpr {

    private final IrSetExpr take;
    private final IrSetExpr[] children;

    IrJoin(IrSetExpr take, IrSetExpr[] children) {
        this.take = Check.notNull(take);
        this.children = Check.noNulls(children);
    }

    public IrSetExpr getTake() {
        return take;
    }

    public IrSetExpr[] getChildren() {
        return children;
    }

    @Override
    public IrDomain getEnv() {
        TIntIterator iter = take.getEnv().iterator();
        TIntHashSet values = new TIntHashSet(0);
        if (iter.hasNext()) {
            IrDomain domain = children[iter.next()].getEnv();
            values.addAll(domain.getValues());
        }
        return values.isEmpty() ? Irs.EmptyDomain : new IrEnumDomain(values.toArray());
    }

    @Override
    public IrDomain getKer() {
        TIntIterator iter = take.getKer().iterator();
        TIntHashSet values = new TIntHashSet(0);
        while (iter.hasNext()) {
            int[] constant = IrUtil.getConstant(children[iter.next()]);
            if (constant != null) {
                values.addAll(constant);
            }
        }
        return values.isEmpty() ? Irs.EmptyDomain : new IrEnumDomain(values.toArray());
    }

    @Override
    public IrDomain getCard() {
        IrDomain takeEnv = take.getEnv();
        IrDomain takeKer = take.getKer();
        IrDomain takeCard = take.getCard();
        int index = 0;
        int[] childrenLowCards = new int[takeEnv.size() - takeKer.size()];
        int[] childrenHighCards = new int[takeEnv.size() - takeKer.size()];
        int cardLow = 0, cardHigh = 0;

        TIntIterator iter = takeEnv.iterator();
        while (iter.hasNext()) {
            int val = iter.next();
            IrDomain childDomain = children[val].getCard();
            if (takeEnv.contains(val)) {
                cardLow += childDomain.getLowerBound();
                cardHigh += childDomain.getUpperBound();
            } else {
                childrenLowCards[index] = childDomain.getLowerBound();
                childrenHighCards[index] = childDomain.getUpperBound();
                index++;
            }
        }
        assert index == childrenLowCards.length;
        assert index == childrenHighCards.length;

        Arrays.sort(childrenLowCards);
        Arrays.sort(childrenHighCards);

        for (int i = 0; i < takeCard.getLowerBound() - takeKer.size(); i++) {
            cardLow += childrenLowCards[i];
        }
        for (int i = 0; i < takeCard.getUpperBound() - takeKer.size(); i++) {
            cardHigh += childrenHighCards[childrenHighCards.length - 1 - i];
        }

        return new IrBoundDomain(cardLow, cardHigh);
    }

    @Override
    public <A, B> B accept(IrSetExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }
}
