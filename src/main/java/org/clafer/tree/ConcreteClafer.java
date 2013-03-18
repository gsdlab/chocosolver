package org.clafer.tree;

import choco.Choco;
import org.clafer.func.MinFunc;
import org.clafer.func.LinearFunc;
import org.clafer.func.ConstFunc;
import static org.clafer.Util.*;
import static choco.Choco.*;
import choco.kernel.model.Model;
import choco.kernel.model.variables.integer.IntegerVariable;
import choco.kernel.model.variables.set.SetVariable;
import choco.kernel.solver.Solver;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import org.clafer.Check;
import org.clafer.ChocoUtil;
import org.clafer.constraint.BoolChannelManager;
import org.clafer.constraint.IncreasingManager;
import org.clafer.constraint.SelectNManager;
import org.clafer.constraint.SetLexManager;
import org.clafer.func.FP;
import org.clafer.func.Func;
import org.clafer.tree.analysis.Analysis;

/**
 *
 * @author jimmy
 */
public class ConcreteClafer extends AtomicClafer {

    private final Card card;
    private final SetVariable[] childSet;
    private final IntegerVariable[] parentPointers;
    private final AtomicClafer parent;

    public ConcreteClafer(String name, int scope, Card card, AtomicClafer parent) {
        super(name, scope, makeSetVar(name, 0, scope - 1), makeBooleanVarArray(name + "@Membership", scope));

        // if card.isexact and card.low == 1 then
        //     set = parent.set
        //     childset = parent.childset
        this.card = Check.notNull(card);
        this.parent = Check.notNull(parent);

        if (scope == 1) {
            this.childSet = new SetVariable[]{getSet()};
        } else {
            this.childSet =
                    // Abstract clafers do not select the lowest N
                    // TODO: Optimize with global cardinality to skip Card the first M where
                    // M is known to exists.
                    parent instanceof AbstractClafer
                    ? (parent.getName().equals("c1_IMeasurable")
                    ? skipCards(name + "@Child", parent.getScope(), 0, scope - 1, card, 5)
                    : makeSetVarArray(name + "@Child", parent.getScope(), 0, scope - 1))
                    : skipCards(name + "@Child", parent.getScope(), 0, scope - 1, card);
        }
        this.parentPointers = makeIntVarArray(name + "@Parent", scope, 0, parent.getScope());
    }

    public Card getCard() {
        return card;
    }

    public AtomicClafer getParent() {
        return parent;
    }

    public SetVariable[] getChildSet() {
        return childSet;
    }

    public IntegerVariable[] getParentPointers() {
        return parentPointers;
    }

    private static SetVariable[] skipCards(String name, int dimension, int low, int high, Card card) {
        return skipCards(name, dimension, low, high, card, dimension);
    }

    // Optimize
    private static SetVariable[] skipCards(String name, int dimension, int low, int high, Card card, int select) {
        Func<Integer, Integer> lowFunc =
                card.hasLow()
                ? new MinFunc(new LinearFunc(card.getLow(), low), high)
                : new ConstFunc(low);
        Func<Integer, Integer> highFunc =
                card.hasHigh()
                ? new MinFunc(new LinearFunc(card.getHigh(), low + card.getHigh() - 1), high)
                : new ConstFunc(high);

        SetVariable[] skip = new SetVariable[dimension];
        int i = 0;
        for (; i < select; i++) {
            skip[i] =
                    makeSetVar(name + "#" + i,
                    lowFunc.apply(i),
                    highFunc.apply(i));
        }
        int variableLow = lowFunc.apply(i);
        for (; i < dimension; i++) {
            skip[i] =
                    makeSetVar(name + "#" + i,
                    variableLow,
                    highFunc.apply(i));
        }
        return skip;
    }

    @Override
    public ConcreteClafer extending(AbstractClafer superClafer) {
        super.extending(superClafer);
        return this;
    }

    @Override
    public void build(Model model, Analysis analysis) {
        int selectLowN =
                parent instanceof AbstractClafer
                ? 0 // TODO: optimize here too
                : analysis.getGlobalCard(this).getLow();
        if (childSet.length == 1) {
            model.addConstraint(eq(childSet[0], getSet()));
        } else {
            // Because they are disjoint
            model.addConstraint(eq(ChocoUtil.sum(FP.mapped(childSet, FP.getCard)), getSet().getCard()));
            model.addConstraint(setUnion(childSet, getSet()));
        }
        model.addConstraint(BoolChannelManager.boolChannel(getMembership(), getSet()));
        if (selectLowN < getScope()) {
            SetVariable unused = makeSetVar(getName() + "@Unused", selectLowN, getScope() - 1);
            model.addConstraint(eq(plus(getSet().getCard(), unused.getCard()), getScope()));
            model.addConstraint(inverseSet(parentPointers, cons(childSet, unused)));
        } else {
            model.addConstraint(inverseSet(parentPointers, childSet));
        }
        for (int i = 0; i < childSet.length; i++) {
            if (card.isBounded()) {
                model.addConstraint(implies(eq(parent.getMembership()[i], 1),
                        ChocoUtil.betweenCard(childSet[i], card)));
            }
            model.addConstraint(implies(eq(parent.getMembership()[i], 0),
                    eqCard(childSet[i], 0)));
        }

        model.addConstraint(SetLexManager.setLex(childSet, card));

        // TODO: redundant?
//         if (card.isExact()) {
//         model.addConstraint(eq(getSet().getCard(), mult(parent.getSet().getCard(), card.getLow())));
//         } else {
//         if (card.hasLow()) {
//         model.addConstraint(geq(getSet().getCard(), mult(parent.getSet().getCard(), card.getLow())));
//         }
//         if (card.hasHigh()) {
//         model.addConstraint(leq(getSet().getCard(), mult(parent.getSet().getCard(), card.getHigh())));
//         }
//         }

        breakSymmetry(model);

        super.build(model, analysis);
    }

    @Override
    protected void print(Solver solver, String indent, int parent, Appendable output)
            throws IOException {
        int[] nums = solver.getVar(childSet[parent]).getValue();
        for (int num : nums) {
            output.append(indent + getName() + num).append('\n');
            for (Clafer child : getRefAndChildren()) {
                child.print(solver, indent + "  ", num, output);
            }
            if (hasSuperClafer()) {
                getSuperClafer().print(solver, this, indent, num, output);
            }
        }
    }

    private void breakSymmetry(Model model) {
        if (getScope() <= 1) {
            // No symmetry
            return;
        }
        // Although this constraint is redundant, it lowers the number of backtracks.
        model.addConstraint(SelectNManager.selectN(getMembership(), getSet().getCard()));
        
        /**
         * What is this optimization?
         * 
         * Force the lower number atoms to choose lower number parents. For example consider
         * the following clafer model:
         * 
         *   Person 2
         *     Hand 2
         * 
         * The constraint forbids the case where Hand0 belongs to Person1 and Hand1 belongs
         * to Person0. Otherwise, the children can swap around creating many isomorphic
         * solutions.
         */
        model.addConstraint(IncreasingManager.increasing(parentPointers));

        /**
         * What is this optimization?
         * 
         * This optimization is to remove isomorphic solutions due to swapping of children.
         * 
         * Consider the following Clafer model.
         * 
         *   Diner 2
         *     Burger 1..*
         * 
         * Let the scope be {Diner=2, Food=3}. What this says is that we have 2 Diners and 3
         * Burgers but each Diner gets at least one serving. Logically, there are two solutions:
         * 
         *   1. Each Diner gets 1 Burger each (the last Burger is unused)
         *   2. One Diner gets 2 Burgers and the other Diner gets 1 Burger
         * 
         * There are two unique solutions, other isomorphic solutions may arise from the solver,
         * but ideally we want to eliminate the isomorphic duplicates. For example, here are
         * two isomorphic instances that will arrise with symmetry breaking:
         * 
         *   Diner0
         *     Burger0
         *     Burger1
         *   Diner1
         *     Burger2
         * 
         *  
         *   Diner0
         *     Burger0
         *   Diner1
         *     Burger1
         *     Burger2
         * 
         * We add the constraint |Diner0.Burger| >= |Diner1.Burger| to break the symmetry.
         * This is how it works when Diner only has one type as a child. This optimization
         * generalizes to multi-children. For example, consider the Clafer model:
         * 
         *   Diner 2
         *     Burger 1..*
         *     Drink 1..*
         * 
         * Let the scope be {Diner=2, Food=3, Drink=4}. First we'll see a wrong generalization.
         * Adding the two constraints DO NOT WORK:
         * 
         *   1. |Diner0.Burger| >= |Diner1.Burger|
         *   2. |Diner0.Drink| >= |Diner1.Drink|
         * 
         * The two constraints above DO NOT WORK because it rules out the case where one Diner has
         * more Burgers but the other Diner has more drinks. Instead, we want tuple comparison like
         * the following constraint (tuple comparision as implemented in Haskell, ie. compare the
         * first indices, then use the second index to break ties, then use the third index to break
         * the next tie, etc.):
         * 
         *   (|Diner0.Burger|, |Diner0.Drink|) >= (|Diner1.Burger|, |Diner1.Drink|)
         * 
         * However, Choco does not implement tuple comparision but it's we can simulate it with
         * the equation constraint since the size of the sets are bounded.
         * 
         *   5 * |Diner0.Burger| + 1 * |Diner0.Drink| >= 5 * |Diner1.Burger| + 1 * |Diner1.Drink|
         * 
         * The "5" is coefficient comes from the fact that scope(Drink) = 4.
         */
        List<Term> terms = new ArrayList<Term>();
        for (ConcreteClafer child : getChildren()) {
            // Children with exact cards will always contribute the same score hence it
            // is a waste of computation time to include them in the scoring constraints.
            if (!child.getCard().isExact()) {
                terms.add(new Term(child));
            }
        }

        AtomicClafer sup = this;
        int offset = 0;
        while (sup.hasSuperClafer()) {
            offset += sup.getSuperClafer().getUpcastOffset(sup);
            for (ConcreteClafer child : sup.getSuperClafer().getChildren()) {
                if (!child.getCard().isExact()) {
                    terms.add(new Term(child, offset, offset + getScope()));
                }
            }
            sup = sup.getSuperClafer();
        }
        if (terms.isEmpty()) {
            // Already sorted (since no cards with inexact bounds).
            return;
        }
        /*
         * What is this optimization?
         *
         * Reversing is so that earlier children have higher scores. Not necessary,
         * but the solutions will have instances that are similar closer together.
         * Technically not an optimization.
         */
        Collections.reverse(terms);
        model.addConstraint(IncreasingManager.decreasing(score(model, terms)));
    }

    IntegerVariable[] score(Model model, List<Term> terms) {
        assert FP.same(FP.mapped(terms, FP.compose(FP.<SetVariable>length(), getChildSet)));

        switch (terms.size()) {
            case 0:
                return new IntegerVariable[0];
            case 1:
                Term term = terms.get(0);
                IntegerVariable[] score = new IntegerVariable[term.getChildSet().length];
                for (int i = 0; i < score.length; i++) {
                    score[i] = term.getChildSet()[i].getCard();
                }
                return score;
            default:
                long max = 1;
                int[] scales = new int[terms.size()];
                for (int i = 0; i < scales.length; i++) {
                    scales[i] = (int) max;
                    term = terms.get(i);

                    max *= term.getMax() + 1;
                    if (max > Integer.MAX_VALUE) {
                        // It's possible to overflow
                        // TODO: What's the best option in this case?
                        throw new RuntimeException("Overflow");
                    }
                }

                score = Choco.makeIntVarArray(getName() + "@Score", getScope(), 0, (int) max);
                for (int i = 0; i < score.length; i++) {
                    IntegerVariable[] termVars = new IntegerVariable[terms.size()];
                    for (int j = 0; j < termVars.length; j++) {
                        termVars[j] = terms.get(j).getChildSet()[i].getCard();
                    }
                    model.addConstraint(equation(score[i], termVars, scales));
                }
                return score;
        }
    }

    @Override
    public String toString() {
        return getName();
    }

    static class Term {

        // TODO: use min
        @Deprecated
        private final int min;
        // The max card of each child set
        private final int max;
        private final SetVariable[] childSet;

        public Term(ConcreteClafer clafer) {
            this(0,
                    Math.min(clafer.getCard().getHigh(), clafer.getScope()),
                    clafer.getChildSet());
        }

        public Term(ConcreteClafer clafer, int from, int to) {
            this(0,
                    Math.min(clafer.getCard().getHigh(), clafer.getScope()),
                    Arrays.copyOfRange(clafer.getChildSet(), from, to));
        }

        public Term(int min, int max, SetVariable[] childSet) {
            this.min = min;
            this.max = max;
            this.childSet = childSet;
        }

        public int getMin() {
            return min;
        }

        public int getMax() {
            return max;
        }

        public SetVariable[] getChildSet() {
            return childSet;
        }
    }
    private static final Func<Term, SetVariable[]> getChildSet =
            new Func<Term, SetVariable[]>() {

                @Override
                public SetVariable[] apply(Term param) {
                    return param.getChildSet();
                }
            };
}
