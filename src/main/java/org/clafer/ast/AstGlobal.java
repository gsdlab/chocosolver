package org.clafer.ast;

import org.clafer.common.Check;

/**
 * All the Clafers of a type. For example, A and B are the globals in the
 * expression {@code some A => some B}.
 *
 * @author jimmy
 */
public class AstGlobal implements AstSetExpr {

    private final AstClafer type;

    AstGlobal(AstClafer type) {
        this.type = Check.notNull(type);
    }

    /**
     * The type of this expression.
     *
     * @return the type of this expression
     */
    public AstClafer getType() {
        return type;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public <A, B> B accept(AstExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String toString() {
        return type.getName();
    }
}
