package org.clafer.ast;

/**
 * A primitive Clafer. Primitive Clafers require built in support from the
 * solver.
 *
 * @author jimmy
 */
public abstract class AstPrimClafer extends AstClafer {

    AstPrimClafer(String name) {
        super(name,
                // It's okay to create a new factory since it will never be used.
                new AstIdFactory());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public AstClafer extending(AstAbstractClafer superClafer) {
        throw new UnsupportedOperationException("Cannot extend from " + getName() + " primitive");
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public AstClafer refTo(AstClafer targetType) {
        throw new UnsupportedOperationException("Cannot ref from " + getName() + " primitive");
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public AstClafer refToUnique(AstClafer targetType) {
        throw new UnsupportedOperationException("Cannot ref from " + getName() + " primitive");
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public AstConcreteClafer withGroupCard(Card groupCard) {
        throw new UnsupportedOperationException("Cannot set group cardinality for " + getName() + " primitive");
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public AstConcreteClafer addChild(String name) {
        throw new UnsupportedOperationException("Cannot add a child under " + getName() + " primitive");
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void addConstraint(AstBoolExpr constraint) {
        throw new UnsupportedOperationException("Cannot add a constraint under " + getName() + " primitive");
    }
}
