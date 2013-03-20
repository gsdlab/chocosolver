package org.clafer.tree;

import choco.Choco;
import choco.kernel.model.Model;
import choco.kernel.model.variables.integer.IntegerExpressionVariable;
import choco.kernel.model.variables.integer.IntegerVariable;
import choco.kernel.model.variables.set.SetVariable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import org.clafer.Check;
import org.clafer.ChocoUtil;
import org.clafer.Util;
import org.clafer.tree.analysis.Analysis;

/**
 *
 * @author jimmy
 */
public abstract class AtomicClafer extends Clafer {

    private final int scope;
    private final SetVariable set;
    private final IntegerVariable[] membership;
    private final List<ConcreteClafer> children = new ArrayList<ConcreteClafer>();
    private final List<ClaferConstraint> constraints = new ArrayList<ClaferConstraint>();
    private AbstractClafer superClafer = null;
    private RefClafer ref = null;
    private Card groupCard;

    public AtomicClafer(String name, int scope, SetVariable set, IntegerVariable[] membership) {
        super(name);
        if (scope < 1) {
            throw new IllegalArgumentException("Scope has to be positive, received \"" + scope + "\"");
        }
        this.scope = scope;
        this.set = Check.notNull(set);
        this.membership = Check.noNulls(membership);
    }

    public int getScope() {
        return scope;
    }

    public int getScopeLow() {
        return 0;
    }

    public int getScopeHigh() {
        return scope - 1;
    }

    public SetVariable getSet() {
        return set;
    }

    public IntegerVariable[] getMembership() {
        return membership;
    }

    public ConcreteClafer addChild(String name, int scope, Card card) {
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
    public RefClafer refTo(AtomicClafer type) {
        if (hasRef()) {
            throw new IllegalStateException(getName() + " already has a reference");
        }
        ref = new RefClafer(type, this, false);
        return ref;
    }

    /**
     * Create a new reference with set semantics, ie. cannot reference
     * the same value under the same parent.
     */
    public RefClafer refToUnique(AtomicClafer type) {
        if (hasRef()) {
            throw new IllegalStateException(getName() + " already has a reference");
        }
        ref = new RefClafer(type, this, true);
        return ref;
    }

    public boolean hasRef() {
        return ref != null;
    }

    public RefClafer getRef() {
        return ref;
    }

    public AtomicClafer withGroupCard(Card groupCard) {
        this.groupCard = groupCard;
        return this;
    }

    public boolean hasGroupCard() {
        return groupCard != null;
    }

    public Card getGroupCard() {
        return groupCard;
    }

    /**
     * @return ref + children
     */
    public List<Clafer> getRefAndChildren() {
        return hasRef()
                ? Util.cons(ref, children)
                : Collections.<Clafer>unmodifiableList(children);
    }

    @Override
    protected void build(Model model, Analysis analysis) {
        if (hasGroupCard()
                && getGroupCard().isBounded()
                && !children.isEmpty()) {
            // build group card
            for (int i = 0; i < getScope(); i++) {
                IntegerVariable[] cards = new IntegerVariable[children.size()];
                for (int j = 0; j < cards.length; j++) {
                    cards[j] = children.get(j).getChildSet()[i].getCard();
                }
                IntegerExpressionVariable cardSum = ChocoUtil.sum(cards);

                model.addConstraint(Choco.implies(
                        Choco.eq(membership[i], 1), ChocoUtil.betweenCard(cardSum, groupCard)));
            }
        }
    }
}