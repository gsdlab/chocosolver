package org.clafer.ast;

import org.clafer.Check;

/**
 *
 * @author jimmy
 */
public class AstQuantify implements AstBoolExpression {

    private final Quantifier quantifier;
    private final AstDecl[] decls;
    private final AstBoolExpression body;

    public AstQuantify(Quantifier quantifier, AstDecl[] decls, AstBoolExpression body) {
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

    public AstBoolExpression getBody() {
        return body;
    }

    @Override
    public <A, B> B accept(AstExpressionVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    public static enum Quantifier {

        Some,
        All
    }
}
