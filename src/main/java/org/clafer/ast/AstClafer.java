package org.clafer.ast;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import org.clafer.common.Check;

/**
 * A Clafer in the model. A Clafer represents both the set and the type. A
 * concrete is either abstract, concrete, or primitive.
 *
 * @author jimmy
 */
public abstract class AstClafer implements AstVar {

    private final String name;
    // The topmost Clafer in the type hierarchy.
    protected final AstAbstractClafer claferClafer;
    private AstAbstractClafer superClafer;
    private AstRef ref;
    private Card groupCard = new Card();
    private final List<AstConcreteClafer> children = new ArrayList<>();
    private final List<AstConstraint> constraints = new ArrayList<>();

    AstClafer(String name, AstAbstractClafer claferClafer) {
        this.name = Check.notNull(name);
        this.claferClafer = claferClafer;
    }

    /**
     * Checks if this Clafer is a primitive. The primitives are int, bool, and
     * string.
     *
     * @return {@code true} if and only if this Clafer is a primitive,
     * {@code false} otherwise
     */
    public boolean isPrimitive() {
        return false;
    }

    /**
     * Returns the name of the Clafer.
     *
     * @return the name of the Clafer
     */
    @Override
    public String getName() {
        return name;
    }

    /**
     * Check if this Clafer has a supertype. Every Clafer has a supertype except
     * for the special Clafer at the root of the type hierarchy.
     *
     * @return {@code true} if and only if this Clafer has a supertype,
     * {@code false} otherwise
     */
    public boolean hasSuperClafer() {
        return superClafer != null;
    }

    /**
     * Returns the supertype of this Clafer.
     *
     * @return the supertype of this Clafer
     */
    public AstAbstractClafer getSuperClafer() {
        return superClafer;
    }

    /**
     * Set the supertype of this Clafer.
     *
     * @param superClafer the supertype
     * @return this Clafer
     */
    public AstClafer extending(AstAbstractClafer superClafer) {
        Check.notNull(superClafer);
        if (hasSuperClafer()) {
            // Allowed to specialize.
            if (!AstUtil.getSupers(superClafer).contains(getSuperClafer())) {
                throw new IllegalArgumentException(this + " already has a super clafer");
            }
            getSuperClafer().removeSub(this);
        }
        superClafer.addSub(this);
        this.superClafer = superClafer;
        return this;
    }

    /**
     * Check if this Clafer references another Clafer.
     *
     * @return {@code true} if and only if this Clafer references another
     * Clafer, {@code false} otherwise
     */
    public boolean hasRef() {
        return ref != null;
    }

    /**
     * Returns this Clafer's reference
     *
     * @return this Clafer's reference
     */
    public AstRef getRef() {
        return ref;
    }

    /**
     * Set this Clafer's reference to target type.
     *
     * @param targetType the type to refer to
     * @return this Clafer
     */
    public AstClafer refTo(AstClafer targetType) {
        if (hasRef()) {
            throw new IllegalArgumentException(this + " already has a ref");
        }
        this.ref = new AstRef(this, targetType, false);
        return this;
    }

    /**
     * Set this Clafer's reference to target type along with a uniqueness
     * constraint.
     *
     * @param targetType the type to refer to
     * @return this Clafer
     */
    public AstClafer refToUnique(AstClafer targetType) {
        if (hasRef()) {
            throw new IllegalArgumentException(this + " already has a ref");
        }
        this.ref = new AstRef(this, targetType, true);
        return this;
    }

    /**
     * Returns this Clafer's group cardinality.
     *
     * @return this Clafer's group cardinality
     */
    public Card getGroupCard() {
        return groupCard;
    }

    /**
     * Set this Clafer's group cardinality.
     *
     * @param groupCard the group cardinality
     * @return this Clafer
     */
    public AstClafer withGroupCard(Card groupCard) {
        this.groupCard = Check.notNull(groupCard);
        return this;
    }

    /**
     * Set this Clafer's low group cardinality and set the high group
     * cardinality to unbounded.
     *
     * @param low the low group cardinality
     * @return this Clafer
     */
    public AstClafer withGroupCard(int low) {
        return withGroupCard(new Card(low));
    }

    /**
     * Set this Clafer's group cardinality.
     *
     * @param low the low group cardinality
     * @param high the high group cardinality
     * @return this Clafer
     */
    public AstClafer withGroupCard(int low, int high) {
        return withGroupCard(new Card(low, high));
    }

    /**
     * Checks if this Clafer has any concrete children.
     *
     * @return {@code true} if and only if this Clafer has children,
     * {@code false} otherwise
     */
    public boolean hasChildren() {
        return !children.isEmpty();
    }

    /**
     * Returns this Clafer's concrete children
     *
     * @return this Clafer's concrete children
     */
    public List<AstConcreteClafer> getChildren() {
        return Collections.unmodifiableList(children);
    }

    /**
     * Add a new concrete child under this Clafer.
     *
     * @param name the name of the child
     * @return the new child
     */
    public AstConcreteClafer addChild(String name) {
        AstConcreteClafer child = new AstConcreteClafer(name, this, claferClafer);
        children.add(child);
        return child.extending(claferClafer);
    }

    /**
     * Checks if this Clafer has any constraints.
     *
     * @return {@code true} if and only if this Clafer has constraints,
     * {@code false} otherwise
     */
    public boolean hasConstraints() {
        return !constraints.isEmpty();
    }

    /**
     * Returns this Clafer's constraints.
     *
     * @return this Clafer's constraints
     */
    public List<AstConstraint> getConstraints() {
        return Collections.unmodifiableList(constraints);
    }

    /**
     * Add a new constraint under this Clafer.
     *
     * @param expr a tautology
     * @return the constraint
     */
    public AstConstraint addConstraint(AstBoolExpr expr) {
        AstConstraint constraint = new AstConstraint(this, expr);
        constraints.add(constraint);
        return constraint;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean equals(Object obj) {
        return this == obj;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int hashCode() {
        return getName().hashCode();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String toString() {
        return getName();
    }
}
