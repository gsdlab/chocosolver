package org.clafer.tree;

import static choco.Choco.*;
import choco.kernel.model.Model;
import java.util.List;
import choco.kernel.solver.Solver;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import org.clafer.Check;
import org.clafer.constraint.BoolChannelManager;
import org.clafer.constraint.BoolSlideManager;
import org.clafer.constraint.SelectNManager;
import org.clafer.func.FP;
import org.clafer.tree.analysis.Analysis;

/**
 * Note: Abstracts are optimized last but built first.
 * 
 * @author jimmy
 */
public class AbstractClafer extends AtomicClafer {

    private final List<AtomicClafer> subs = new ArrayList<AtomicClafer>();

    public AbstractClafer(String name, int scope) {
        super(name, scope, makeSetVar(name, 0, scope - 1), makeBooleanVarArray(name + "@Membership", scope));
    }

    protected void print(Solver solver, AtomicClafer sub, String indent, int parent, Appendable output)
            throws IOException {
        parent += getUpcastOffset(sub);
        print(solver, indent, parent, output);
    }

    @Override
    protected void print(Solver solver, String indent, int parent, Appendable output)
            throws IOException {
        for (Clafer child : getRefAndChildren()) {
            child.print(solver, indent + "  ", parent, output);
        }
        if (hasSuperClafer()) {
            getSuperClafer().print(solver, this, indent, parent, output);
        }
    }

    void addSubclafer(AtomicClafer sub) {
        subs.add(Check.notNull(sub));
    }

    public List<AtomicClafer> getSubs() {
        return Collections.unmodifiableList(subs);
    }

    /**
     * @param sub
     * @return - A list of sub clafers that appear before sub in the set.
     */
    @Deprecated
    private List<AtomicClafer> getHigherPrioritySubs(AtomicClafer sub) {
        for (int i = 0; i < subs.size(); i++) {
            if (subs.get(i).equals(sub)) {
                return subs.subList(0, i);
            }
        }
        throw new IllegalArgumentException(getName() + " is not a super clafer of " + sub.getName());
    }

    @Deprecated
    public int getUpcastOffset(AtomicClafer sub) {
        return FP.sum(FP.mapped(getHigherPrioritySubs(sub), FP.getScope));
    }

    @Override
    public AbstractClafer extending(AbstractClafer superClafer) {
        super.extending(superClafer);
        return this;
    }

    private void greedyLowAtoms(Analysis analysis) {
        /**
         * What is this optimization?
         * 
         * This optimization is improve the performance of upcasting.
         * 
         * Concrete clafers always select the lowest N, whereas abstract clafers do not.
         * Certain optimizations like skip cards only works by assuming the lowest N are
         * selected.
         * 
         * For example, suppose the sub order is Lion, Tiger, then Bear where scope(Lion) = 3,
         * scope(Tiger) = 4, and scope(Bear) = 2. Suppose upcast(Lion) superset of {0, 1},
         * upcast{Tiger} superset of {3, 4, 5}, and upcast(Bear) superset of {7, 8} is
         * guaranteed to exist as determined by global card analaysis. The problem with is
         * order is that only the lowest two numbers in Animal are selected, and the skip
         * card optimization will only work for those two.
         * 
         * The optimal order would be Bear, Tiger, then Lion where we know the following to
         * exists, upcast(Bear) superset of {0, 1}, upcast(Tiger) superset of {2, 3, 4}, and
         * upcast(Lion) superset of {5, 6}. In this order, we are guaranteed that the lowest 5,
         * in improvement over the previous order.
         */
        List<AtomicClafer> greedy = new ArrayList<AtomicClafer>();
        AtomicClafer largestLowGlobalCard = null;
        for (AtomicClafer sub : subs) {
            Card globalCard = analysis.getGlobalCard(sub);
            if (globalCard.getLow() == sub.getScope()) {
                // The sub clafer is fill their scope and leave no gaps. Always a safe pick.
                greedy.add(sub);
            } else {
                if (largestLowGlobalCard == null
                        || analysis.getGlobalCard(sub).getLow() > analysis.getGlobalCard(largestLowGlobalCard).getLow()) {
                    largestLowGlobalCard = sub;
                }
            }
        }
        /*
         * Next we need to choose sub clafers that do have gaps. Start with the one that has
         * the largest low global cardinality. In the previous example, this would be the tiger.
         */
        if (largestLowGlobalCard != null) {
            greedy.add(largestLowGlobalCard);
        }
        /*
         * The rest of the order does not matter, fill it with whatever is missing.
         */

        for (AtomicClafer sub : subs) {
            if (!greedy.contains(sub)) {
                greedy.add(sub);
            }
        }

        subs.clear();
        subs.addAll(greedy);
    }

    @Override
    public void build(Model model, Analysis analysis) {
        if (getScope() != FP.sum(FP.mapped(subs, FP.getScope))) {
            throw new IllegalStateException(getName() + ": " + getScope() + "!=" + FP.sum(FP.mapped(subs, FP.getScope)));
        }

        model.addConstraint(BoolChannelManager.boolChannel(getMembership(), getSet()));

        greedyLowAtoms(analysis);

        int offset = 0;

        for (AtomicClafer sub : subs) {
            model.addConstraint(BoolSlideManager.boolSlide(sub.getMembership(), getMembership(), offset));
            // TODO: not needed?
            model.addConstraint(SelectNManager.selectN(
                    Arrays.copyOfRange(getMembership(), offset, offset + sub.getScope()),
                    sub.getSet().getCard()));
            offset += sub.getScope();
        }

        super.build(model, analysis);
    }

    @Override
    public String toString() {
        return "abstract " + getName();
    }
}
