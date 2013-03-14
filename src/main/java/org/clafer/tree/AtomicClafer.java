package org.clafer.tree;

import choco.Choco;
import static choco.Choco.*;
import choco.Options;
import choco.kernel.model.Model;
import choco.kernel.model.variables.integer.IntegerVariable;
import choco.kernel.model.variables.set.SetVariable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import org.clafer.Check;
import org.clafer.Util;
import org.clafer.collection.Pair;

/**
 *
 * @author jimmy
 */
public abstract class AtomicClafer extends Clafer {

    private final SetVariable set;
    private final IntegerVariable[] membership;
    private final List<AtomicClafer> children = new ArrayList<AtomicClafer>();
    private final List<ClaferConstraint> constraints = new ArrayList<ClaferConstraint>();
    private RefClafer ref = null;
    protected Card globalCard;

    public AtomicClafer(String name, int scope, SetVariable set, IntegerVariable[] membership) {
        super(name, scope);
        this.set = Check.notNull(set);
        this.membership = Check.noNulls(membership);
    }

    public SetVariable getSet() {
        return set;
    }

    public IntegerVariable[] getMembership() {
        return membership;
    }

    void addChild(AtomicClafer child) {
        children.add(Check.notNull(child));
    }

    public boolean hasChild(AtomicClafer child) {
        return children.contains(child);
    }

    public boolean hasChildren() {
        return !children.isEmpty();
    }

    public List<AtomicClafer> getChildren() {
        return Collections.unmodifiableList(children);
    }

    public void addConstraint(ClaferConstraint constraint) {
        constraints.add(Check.notNull(constraint));
    }

    public boolean hasConstraints() {
        return !constraints.isEmpty();
    }

    public List<ClaferConstraint> getConstraints() {
        return Collections.unmodifiableList(constraints);
    }

    public boolean hasRef() {
        return ref != null;
    }

    public RefClafer getRef() {
        return ref;
    }

    void setRef(RefClafer ref) {
        if (hasRef()) {
            throw new IllegalStateException(getName() + " already has a reference");
        }
        this.ref = Check.notNull(ref);
    }

    /**
     * @return ref + children
     */
    public List<Clafer> getRefAndChildren() {
        return hasRef()
                ? Util.cons(ref, children)
                : Collections.<Clafer>unmodifiableList(children);
    }

    /**
     * @return the children, children's children, children's children's children, etc.
     */
    public List<AtomicClafer> getNestedChildren() {
        List<AtomicClafer> nested = new ArrayList<AtomicClafer>();
        for (AtomicClafer child : getChildren()) {
            nested.add(child);
            nested.addAll(child.getNestedChildren());
        }
        return nested;
    }

    private void breakSymmetry(Model model) {
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
        if (getScope() <= 1) {
            // Already sorted (since at most one card)
            return;
        }
        // Children with exact cards will always contribute the same score hence it
        // is a waste of computation time to include them in the scoring constraints.
        List<ConcreteClafer> inexactChildren = Util.filterInexactCard(Util.filterConcrete(getChildren()));
        switch (inexactChildren.size()) {
            case 0:
                // Already sorted (since no cards with inexact bounds).
                break;
            case 1:
                ConcreteClafer child = (ConcreteClafer) inexactChildren.get(0);
                IntegerVariable[] reverseCard = reverseCards(child.getChildSet());
                model.addConstraint(increasingSum(reverseCard, child.getSet().getCard()));
                break;
            default:
                Pair<IntegerVariable[], Integer> pair = reverseCardsAndScale(model, inexactChildren);
                IntegerVariable[] reverseScaledCards = pair.getFst();
                int scale = pair.getSnd();
                model.addConstraint(increasingSum(reverseScaledCards,
                        makeIntVar("increasingSumCard" + getName(), 0, scale)));
                break;
        }
    }

    private static IntegerVariable[] reverseCards(SetVariable... childSet) {
        IntegerVariable[] reverseCards = new IntegerVariable[childSet.length];
        for (int i = 0; i < reverseCards.length; i++) {
            reverseCards[i] = childSet[childSet.length - i - 1].getCard();
        }
        return reverseCards;
    }

    private static boolean sameChildSetLength(List<ConcreteClafer> clafers) {
        if (clafers.size() > 0) {
            ConcreteClafer fst = clafers.get(0);
            for (ConcreteClafer clafer : clafers) {
                if (fst.getChildSet().length != clafer.getChildSet().length) {
                    return false;
                }
            }
        }
        return true;
    }

    private Pair<IntegerVariable[], Integer> reverseCardsAndScale(Model model, List<ConcreteClafer> clafers) {
        assert sameChildSetLength(clafers);

        // Give clafers earlier in the list higher scales
        Collections.reverse(clafers);

        IntegerVariable[][] cards = new IntegerVariable[clafers.size()][];

        /**
         * Why reverse the cards? No real reason. Continuing from the final example in the
         * breakSymmetry method, we can either give Burger or Drink the higher coefficent.
         * By reversing the cards, it gives Burger the larger coefficent.
         */
        for (int i = 0; i < clafers.size(); i++) {
            ConcreteClafer clafer = clafers.get(i);
            cards[i] = reverseCards(clafer.getChildSet());
        }
        cards = Util.transpose(cards);

        long scale = 1;
        int[] scales = new int[clafers.size()];
        for (int i = 0; i < clafers.size(); i++) {
            ConcreteClafer clafer = clafers.get(i);
            scales[i] = (int) scale;
            scale *= clafer.getScope() + 1;
            if (scale > Integer.MAX_VALUE) {
                // It's possible to overflow
                return null;
            }
        }
        scale--;

        IntegerVariable[] scaledCards = makeIntVarArray("scaledCards" + getName(), cards.length, 0, (int) scale);
        for (int i = 0; i < cards.length; i++) {
            model.addConstraint(equation(scaledCards[i], cards[i], scales));
        }

        return new Pair(scaledCards, (int) scale);
    }

    /**
     * Rearrange and optimize data structures before the actually building the final
     * Choco model.
     * 
     * @param parentCard - The global cardinality of the parent clafer
     */
    protected abstract void optimize(Model model, Card parentCard);

    @Override
    public void build(Model model) {
        breakSymmetry(model);
        if (hasRef()) {
            getRef().build(model);
        }
        for (AtomicClafer clafer : getChildren()) {
            clafer.build(model);
        }
        ThisFactory thisFactory = new CacheThisFactory(model);
        for (ClaferConstraint constraint : constraints) {
            constraint.build(model, thisFactory);
        }
    }

    /**
     * Creating SetVariables for "this" has a cost since it requires a constraints.
     * The idea is to not create SetVariables unless they are needed, and reuse them
     * if they are needed mutiple times.
     */
    private class CacheThisFactory implements ThisFactory {

        private final Model model;
        private final int offset = getScopeLow();
        private final SetExpr[] cache = new SetExpr[getScopeHigh() - getScopeLow() + 1];

        public CacheThisFactory(Model model) {
            this.model = model;
        }

        private void checkId(int id) {
            if (id < getScopeLow() || id > getScopeHigh()) {
                throw new IllegalArgumentException(id + " is outside the scope of " + getName());
            }
        }

        @Override
        public IntExpr newIntThis(int id) {
            checkId(id);
            return new IntExpr(AtomicClafer.this, constant(id));
        }

        @Override
        public SetExpr newSetThis(int id) {
            checkId(id);
            int index = id - offset;
            if (cache[index] == null) {
                SetVariable s = getSet();
                SetVariable thisV = makeSetVar("constraintUnder" + getName() + id, id, id, Options.V_NO_DECISION);
                // TODO: if then else
                model.addConstraint(implies(member(id, s), eq(thisV, constant(new int[]{id}))));
                model.addConstraint(implies(notMember(id, s), eq(thisV, emptySet())));
                cache[index] = new SetExpr(AtomicClafer.this, thisV);
            }
            return cache[index];
        }
    }
}