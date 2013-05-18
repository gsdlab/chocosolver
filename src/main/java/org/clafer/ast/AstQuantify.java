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
        this.decls = Check.notNull(decls);
        if (decls.length < 1) {
            throw new IllegalArgumentException();
        }
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

        Some,
        All
    }
}
