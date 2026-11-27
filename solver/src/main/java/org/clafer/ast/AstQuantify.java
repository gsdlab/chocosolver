package org.clafer.ast;

import java.util.Arrays;
import org.clafer.common.Check;
import org.clafer.common.Util;

/**
 *
 * @author jimmy
 */
public record AstQuantify(Quantifier quantifier, AstDecl[] decls, AstBoolExpr body) implements AstBoolExpr {

    public AstQuantify {
        quantifier = Check.notNull(quantifier);
        decls = Check.noNullsNotEmpty(decls);
        body = Check.notNull(body);
    }

    @Override
    public boolean equals(Object obj) {
        return obj instanceof AstQuantify other && quantifier == other.quantifier
                && Arrays.equals(decls, other.decls) && body.equals(other.body);
    }

    @Override
    public int hashCode() {
        return quantifier.ordinal() ^ Arrays.hashCode(decls) ^ body.hashCode();
    }

    @Override
    public String toString() {
        return quantifier.getSyntax() + " " + Util.commaSeparate(decls) + " | " + body;
    }

    public static enum Quantifier {

        All("all"),
        Lone("lone"),
        None("no"),
        One("one"),
        Some("some");
        private final String syntax;

        private Quantifier(String syntax) {
            this.syntax = syntax;
        }

        public String getSyntax() {
            return syntax;
        }
    }
}
