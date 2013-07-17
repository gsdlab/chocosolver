package org.clafer.ast;

import org.clafer.common.Check;
import org.clafer.common.Util;

/**
 *
 * @author jimmy
 */
public class AstQuantify implements AstBoolExpr {

    private final Quantifier quantifier;
    private final AstDecl[] decls;
    private final AstBoolExpr body;

    public AstQuantify(Quantifier quantifier, AstDecl[] decls, AstBoolExpr body) {
        this.quantifier = Check.notNull(quantifier);
        this.decls = Check.noNullsNotEmpty(decls);
        this.body = Check.notNull(body);
    }

    public Quantifier getQuantifier() {
        return quantifier;
    }

    public AstDecl[] getDecls() {
        return decls;
    }

    public AstBoolExpr getBody() {
        return body;
    }

    @Override
    public <A, B> B accept(AstExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
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
