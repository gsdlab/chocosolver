package org.clafer.ast;

import org.clafer.common.Check;

/**
 * A concrete Clafer.
 *
 * @author jimmy
 */
public class AstConcreteClafer extends AstClafer {

    private final AstClafer parent;
    private Card card = new Card();

    AstConcreteClafer(String name, AstAbstractClafer claferClafer) {
        super(name, claferClafer);
        this.parent = null;
    }

    AstConcreteClafer(String name, AstClafer parent, AstAbstractClafer claferClafer) {
        super(name, claferClafer);
        this.parent = Check.notNull(parent);
    }

    /**
     * Checks if this Clafer has a parent. Every Clafer has a parent except for
     * the special Clafer at the root of the hierarchy.
     *
     * @return {@code true} if and only if this Clafer has a parent,
     * {@code false} otherwise
     */
    public boolean hasParent() {
        return parent != null;
    }

    /**
     * Returns this Clafer's parent.
     *
     * @return this Clafer's parent
     */
    public AstClafer getParent() {
        return parent;
    }

    /**
     * Returns this Clafer's cardinality.
     *
     * @return this Clafer's cardinality
     */
    public Card getCard() {
        return card;
    }

    /**
     * Set this Clafer's cardinality.
     *
     * @param card the cardinality
     * @return this Clafer
     */
    public AstConcreteClafer withCard(Card card) {
        this.card = Check.notNull(card);
        return this;
    }

    /**
     * Set this Clafer's low cardinality and set the high cardinality to
     * unbounded.
     *
     * @param low the low group cardinality
     * @return this Clafer
     */
    public AstConcreteClafer withCard(int low) {
        return withCard(new Card(low));
    }

    /**
     * Set this Clafer's cardinality.
     *
     * @param low the low cardinality
     * @param high the high cardinality
     * @return this Clafer
     */
    public AstConcreteClafer withCard(int low, int high) {
        return withCard(new Card(low, high));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public AstConcreteClafer withGroupCard(Card groupCard) {
        super.withGroupCard(groupCard);
        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public AstConcreteClafer withGroupCard(int low) {
        return withGroupCard(new Card(low));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public AstConcreteClafer withGroupCard(int low, int high) {
        return withGroupCard(new Card(low, high));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public AstConcreteClafer extending(AstAbstractClafer superClafer) {
        super.extending(superClafer);
        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public AstConcreteClafer refTo(AstClafer targetType) {
        super.refTo(targetType);
        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public AstConcreteClafer refToUnique(AstClafer targetType) {
        super.refToUnique(targetType);
        return this;
    }
}
