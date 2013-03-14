package org.clafer.tree;

import static choco.Choco.*;
import choco.kernel.model.Model;
import choco.kernel.model.variables.integer.IntegerExpressionVariable;
import choco.kernel.model.variables.integer.IntegerVariable;
import java.util.List;
import choco.kernel.solver.Solver;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Map;
import org.clafer.Check;
import org.clafer.constraint.BoolChannelManager;

/**
 * Note: Abstracts are optimized last but built first.
 * 
 * @author jimmy
 */
public class AbstractClafer extends AtomicClafer {

    private final Map<AtomicClafer, IntegerVariable> offsetMap = new HashMap<AtomicClafer, IntegerVariable>();
    private final List<AtomicClafer> subs = new ArrayList<AtomicClafer>();

    public AbstractClafer(String name, int scope, AtomicClafer... subclafers) {
        super(name, scope, makeSetVar(name, 0, scope - 1), makeBooleanVarArray(name + "@Membership", scope));
    }

    @Override
    protected void print(Solver solver, String indent, int parent, Appendable output)
            throws IOException {
        for (Clafer child : getRefAndChildren()) {
            child.print(solver, indent + "  ", parent, output);
        }
    }

    void addSubclafer(AtomicClafer sub) {
        subs.add(Check.notNull(sub));
    }

    public List<AtomicClafer> getSubs() {
        return Collections.unmodifiableList(subs);
    }

    @Override
    public void build(Model model) {
        model.addConstraint(BoolChannelManager.boolChannel(getMembership(), getSet()));
        super.build(model);
    }

    @Override
    protected void optimize(final Model model, final Card parentCard) {
        /**
         * What is this optimization?
         * 
         * This optimization is improve the performance of upcasting.
         * 
         * Let's say the list of children are: [Tigers, Lions, Bears] where Lions
         * and Bears are fixed global cardinality (ie. we know exactly how many there
         * are at compile time), but Tigers is not fixed.
         * 
         * Let's say we have a set of Bears, and let's call this set B. If we need to upcast
         * B to a set of animals, we offset B by |Tiger| + |Lion|. However, |Tiger| is not
         * fixed so |Tiger| + |Lion| is variable.
         * 
         * However, if the list of children was rearranged to [Lions, Bears, Tigers], then
         * offseting is "easier" because |Lion| is fixed. Upcasting a set of Tigers to
         * a set of Animals is also "easier" because |Lions| + |Bears| is fixed.
         * 
         * In general, we want children with lower global cardinality difference (ie. the
         * difference between the high global cardinality and the low global cardinality)
         * since they have lower variance.
         */
        Collections.sort(subs, new GlobalCardGapComparator());

        int constantOffset = 0;
        IntegerExpressionVariable variableOffset = null;
        for (final AtomicClafer sub : subs) {
//            for (int id = 0; id < sub.getScope(); id++) {
//                if (variableOffset == null) {
//                    model.addConstraint(
//                            ifOnlyIf(member(id, sub.getSet()), member(id + constantOffset, getSet())));
//                } else {
//                    IntegerVariable offset =
//                            Choco.makeIntVar("Offset" + sub.getName() + "->" + getName(), variableOffset.getLowB(), variableOffset.getUppB());
//                    model.addConstraint(Choco.eq(offset, variableOffset));
//                    model.addConstraint(
//                            ifOnlyIf(member(id, sub.getSet()), member(variableOffset, id, getSet())));
//                }
//            }

        }

        Card card = new Card(0, 0);
        for (AtomicClafer sub : subs) {
            if (sub.globalCard == null) {
                throw new IllegalStateException("Optimize the subs before the super");
            }
            card = card.add(sub.globalCard);
        }
        globalCard = card;
        for (AtomicClafer child : getChildren()) {
            child.optimize(model, globalCard);
        }
    }

    /**
     * Compare the gap of global cards
     */
    private static class GlobalCardGapComparator implements Comparator<AtomicClafer> {

        @Override
        public int compare(AtomicClafer o1, AtomicClafer o2) {
            int diff1 = o1.globalCard.getHigh() - o1.globalCard.getLow();
            int diff2 = o2.globalCard.getHigh() - o2.globalCard.getLow();
            return (diff1 < diff2) ? -1 : ((diff1 == diff2) ? 0 : 1);
        }
    }
}
