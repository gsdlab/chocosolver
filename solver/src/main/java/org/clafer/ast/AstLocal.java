package org.clafer.ast;

import org.clafer.common.Check;

/**
 * A local is a binding of a value to a name in quantifiers. For example, x, y,
 * and z are the locals in the expression {@code all x;y:A, z:B | ...}.
 * <p>
 * Not a record: two locals with the same name, such as the x of two
 * quantifiers, are different variables, so a local equals only itself.
 *
 * @author jimmy
 */
public final class AstLocal implements AstSetExpr, AstVar {

    private final String name;

    AstLocal(String name) {
        this.name = Check.notNull(name);
    }

    @Override
    public String getName() {
        return name;
    }

    @Override
    public boolean equals(Object obj) {
        return this == obj;
    }

    @Override
    public int hashCode() {
        return name.hashCode();
    }

    @Override
    public String toString() {
        return name;
    }
}
