package org.clafer.ast;

import org.clafer.common.Check;

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

    public static enum Quantifier {

        All("all"),
        Lone("lone"),
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
