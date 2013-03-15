package org.clafer.tree;

import choco.Choco;
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
import org.clafer.Check;
import org.clafer.Util;
import org.clafer.constraint.BoolChannelManager;
import org.clafer.constraint.SelectNManager;
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
        for(AtomicClafer high : getHigherPrioritySubs(sub)) {
            parent += solver.getVar(high.getSet().getCard()).getVal();
        }
        print(solver, indent, parent, output);
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

    /**
     * @param sub
     * @return - A list of sub clafers that appear before sub in the set.
     */
    public List<AtomicClafer> getHigherPrioritySubs(AtomicClafer sub) {
        for (int i = 0; i < subs.size(); i++) {
            if (subs.get(i).equals(sub)) {
                return subs.subList(0, i);
            }
        }
        throw new IllegalArgumentException(getName() + " is not a super clafer of " + sub.getName());
    }

    @Override
    public AbstractClafer extending(AbstractClafer superClafer) {
        super.extending(superClafer);
        return this;
    }

    @Override
    public void build(Model model, Analysis analysis) {
        model.addConstraint(BoolChannelManager.boolChannel(getMembership(), getSet()));
        model.addConstraint(SelectNManager.selectN(getMembership(), getSet().getCard()));

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
        Collections.sort(subs, new GlobalCardGapComparator(analysis));

        int constantOffset = 0;
        List<IntegerVariable> variableOffset = new ArrayList<IntegerVariable>();

        for (AtomicClafer sub : subs) {
            Card globalCard = analysis.getGlobalCard(sub);
            if (globalCard.isExact()) {
                constantOffset += globalCard.getLow();
            } else {
                variableOffset.add(sub.getSet().getCard());
            }
        }
        if (constantOffset > 0) {
            if (variableOffset.isEmpty()) {
                model.addConstraint(SelectNManager.selectN(getMembership(), Choco.constant(constantOffset)));
            } else {
                List<IntegerExpressionVariable> offsets = Util.<IntegerExpressionVariable>cons(Choco.constant(constantOffset), variableOffset);
                model.addConstraint(eq(getSet().getCard(), sum(offsets.toArray(new IntegerExpressionVariable[offsets.size()]))));
                model.addConstraint(SelectNManager.selectN(getMembership(), getSet().getCard()));
            }
        } else {
            if (variableOffset.isEmpty()) {
                model.addConstraint(eq(getSet(), Choco.emptySet()));
            } else {
                model.addConstraint(eq(getSet().getCard(), sum(variableOffset.toArray(new IntegerExpressionVariable[variableOffset.size()]))));
                model.addConstraint(SelectNManager.selectN(getMembership(), getSet().getCard()));
            }
        }


        super.build(model, analysis);
    }

    private IntegerVariable plus(String name, Model model, IntegerVariable e1, IntegerVariable e2) {
        Integer c1 = Util.getConstant(e1);
        Integer c2 = Util.getConstant(e2);

        if (c1 != null && c1.intValue() == 0) {
            return e2;
        }
        if (c2 != null && c2.intValue() == 0) {
            return e1;
        }
        if (c1 != null && c2 != null) {
            return Choco.constant(c1.intValue() + c2.intValue());
        }
        IntegerExpressionVariable added;
        if (c1 != null) {
            added = Choco.plus(c1.intValue(), e2);
        } else if (c2 != null) {
            added = Choco.plus(e1, c2.intValue());
        } else {
            added = Choco.plus(e1, e2);
        }
        IntegerVariable addedVar = Choco.makeIntVar(name, added.getLowB(), added.getUppB());
        model.addConstraint(eq(addedVar, added));
        return addedVar;
    }

    /**
     * Compare the gap of global cards
     */
    private static class GlobalCardGapComparator implements Comparator<AtomicClafer> {

        private final Analysis analysis;

        public GlobalCardGapComparator(Analysis analysis) {
            this.analysis = analysis;
        }

        @Override
        public int compare(AtomicClafer o1, AtomicClafer o2) {
            Card card1 = analysis.getGlobalCard(o1);
            Card card2 = analysis.getGlobalCard(o2);
            int diff1 = card1.getHigh() - card1.getLow();
            int diff2 = card2.getHigh() - card2.getLow();
            return (diff1 < diff2) ? -1 : ((diff1 == diff2) ? 0 : 1);
        }
    }
}
