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

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof AstGlobal) {
            AstGlobal other = (AstGlobal) obj;
            return type.equals(other.getType());
        }
        return false;
    }

    @Override
    public int hashCode() {
        return type.hashCode() + 91;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String toString() {
        return type.getName();
    }
}
