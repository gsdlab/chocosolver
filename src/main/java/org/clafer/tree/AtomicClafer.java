package org.clafer.tree;

import static choco.Choco.*;
import static org.clafer.ChocoUtil.*;
import choco.kernel.model.Model;
import choco.kernel.model.variables.integer.IntegerVariable;
import choco.kernel.model.variables.set.SetVariable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import org.clafer.Check;
import org.clafer.Util;
import org.clafer.collection.Pair;
import org.clafer.func.FP;
import org.clafer.tree.analysis.Analysis;

/**
 *
 * @author jimmy
 */
public abstract class AtomicClafer extends Clafer {

    private final SetVariable set;
    private final IntegerVariable[] membership;
    private final List<ConcreteClafer> children = new ArrayList<ConcreteClafer>();
    private final List<ClaferConstraint> constraints = new ArrayList<ClaferConstraint>();
    private AbstractClafer superClafer = null;
    private RefClafer ref = null;

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

    public ConcreteClafer addChildClafer(String name, int scope, Card card) {
        ConcreteClafer child = new ConcreteClafer(name, scope, card, this);
        children.add(child);
        return child;
    }

    public boolean hasChild(ConcreteClafer child) {
        return children.contains(child);
    }

    public boolean hasChildren() {
        return !children.isEmpty();
    }

    public List<ConcreteClafer> getChildren() {
        return Collections.unmodifiableList(children);
    }

    public void addConstraint(SetConstraint setConstraint) {
        constraints.add(new ClaferConstraint(this, setConstraint));
    }

    public void addConstraint(SetConstraint setConstraint, IntConstraint intConstraint) {
        constraints.add(new ClaferConstraint(this, setConstraint, intConstraint));
    }

    public boolean hasConstraints() {
        return !constraints.isEmpty();
    }

    public List<ClaferConstraint> getConstraints() {
        return Collections.unmodifiableList(constraints);
    }

    /**
     * @param superClafer - this clafer extends superClafer
     * @return this clafer
     */
    public AtomicClafer extending(AbstractClafer superClafer) {
        if (hasSuperClafer()) {
            throw new IllegalArgumentException(getName() + " already has a super clafer");
        }
        superClafer.addSubclafer(this);
        this.superClafer = Check.notNull(superClafer);
        return this;
    }

    public boolean hasSuperClafer() {
        return superClafer != null;
    }

    public AbstractClafer getSuperClafer() {
        return superClafer;
    }

    /**
     * Create a new reference with bag semantics, ie. can reference
     * the same value under the same parent.
     */
    public void refTo(AtomicClafer type) {
        if (hasRef()) {
            throw new IllegalStateException(getName() + " already has a reference");
        }
        ref = new RefClafer(type, this, false);
    }

    /**
     * Create a new reference with set semantics, ie. cannot reference
     * the same value under the same parent.
     */
    public void refToUnique(AtomicClafer type) {
        if (hasRef()) {
            throw new IllegalStateException(getName() + " already has a reference");
        }
        ref = new RefClafer(type, this, true);
    }

    public boolean hasRef() {
        return ref != null;
    }

    public RefClafer getRef() {
        return ref;
    }

    /**
     * @return ref + children
     */
    public List<Clafer> getRefAndChildren() {
        return hasRef()
                ? Util.cons(ref, children)
                : Collections.<Clafer>unmodifiableList(children);
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
        List<ConcreteClafer> inexactChildren = Util.filterInexactCard(getChildren());
        switch (inexactChildren.size()) {
            case 0:
                // Already sorted (since no cards with inexact bounds).
                break;
            case 1:
                ConcreteClafer child = (ConcreteClafer) inexactChildren.get(0);
                model.addConstraint(decreasingSum(
                        FP.mapped(child.getChildSet(), FP.getCard),
                        child.getSet().getCard()));
                break;
            default:
                Pair<IntegerVariable[], Integer> pair = scale(model, inexactChildren);
                IntegerVariable[] scaledCards = pair.getFst();
                int scale = pair.getSnd();
                model.addConstraint(decreasingSum(scaledCards,
                        makeIntVar("decreasingSumCard" + getName(), 0, scale)));
                break;
        }
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

    private Pair<IntegerVariable[], Integer> scale(Model model, List<ConcreteClafer> clafers) {
        // same $ clafers <$> (length . getChildSet)
        assert FP.same(FP.mapped(clafers, FP.compose(FP.<SetVariable>length(), FP.getChildSet)));

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
            cards[i] = FP.mapped(clafer.getChildSet(), FP.getCard);
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

    @Override
    public void build(Model model, Analysis analysis) {
        breakSymmetry(model);
    }
}