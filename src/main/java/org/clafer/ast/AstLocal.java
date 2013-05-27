package org.clafer.ast;

import org.clafer.common.Check;

/**
 * A local is a binding of a value to a name in quantifiers. For example, x, y,
 * and z are the locals in the expression {@code all x;y:A, z:B | ...}.
 *
 * @author jimmy
 */
public class AstLocal implements AstSetExpr, AstVar {

    private final String name;

    AstLocal(String name) {
        this.name = Check.notNull(name);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getName() {
        return name;
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
    public boolean equals(Object obj) {
        return this == obj;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int hashCode() {
        return name.hashCode();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String toString() {
        return name;
    }
}
