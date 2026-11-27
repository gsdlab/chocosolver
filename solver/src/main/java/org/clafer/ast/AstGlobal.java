package org.clafer.ast;

import org.clafer.common.Check;

/**
 * All the Clafers of a type. For example, A and B are the globals in the
 * expression {@code some A => some B}.
 *
 * @author jimmy
 */
public record AstGlobal(AstClafer type) implements AstSetExpr {

    public AstGlobal {
        type = Check.notNull(type);
    }

    @Override
    public String toString() {
        return type.getName();
    }
}
