package org.clafer.ast;

import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class AstSuffix implements AstBoolExpr {

    private final AstSetExpr suffix;
    private final AstSetExpr word;

    AstSuffix(AstSetExpr suffix, AstSetExpr word) {
        this.suffix = Check.notNull(suffix);
        this.word = Check.notNull(word);
    }

    public AstSetExpr getSuffix() {
        return suffix;
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
        if (obj instanceof AstSuffix) {
            AstSuffix other = (AstSuffix) obj;
            return suffix.equals(other.suffix) && word.equals(other.word);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return suffix.hashCode() ^ word.hashCode();
    }

    @Override
    public String toString() {
        return suffix + " suffix " + word;
    }
}
