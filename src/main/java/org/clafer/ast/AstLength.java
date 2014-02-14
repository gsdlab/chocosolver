package org.clafer.ast;

import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class AstLength implements AstSetExpr {

    private final AstSetExpr string;

    public AstLength(AstSetExpr string) {
        this.string = Check.notNull(string);
    }

    public AstSetExpr getString() {
        return string;
    }

    @Override
    public <A, B> B accept(AstExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof AstLength) {
            AstLength other = (AstLength) obj;
            return string.equals(other.string);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return 193 ^ string.hashCode();
    }

    @Override
    public String toString() {
        return "length(" + string + ")";
    }
}
