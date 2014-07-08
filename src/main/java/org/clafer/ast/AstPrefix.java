package org.clafer.ast;

import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class AstPrefix implements AstBoolExpr {

    private final AstSetExpr prefix;
    private final AstSetExpr word;

    AstPrefix(AstSetExpr prefix, AstSetExpr word) {
        this.prefix = Check.notNull(prefix);
        this.word = Check.notNull(word);
    }

    public AstSetExpr getPrefix() {
        return prefix;
    }

    public AstSetExpr getWord() {
        return word;
    }

    @Override
    public <A, B> B accept(AstExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof AstPrefix) {
            AstPrefix other = (AstPrefix) obj;
            return prefix.equals(other.prefix) && word.equals(other.word);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return prefix.hashCode() ^ word.hashCode();
    }

    @Override
    public String toString() {
        return prefix + " prefix " + word;
    }
}
