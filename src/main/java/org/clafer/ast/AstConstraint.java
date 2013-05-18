package org.clafer.ast;

import java.io.Serializable;
import org.clafer.common.Check;
import org.clafer.common.Util;

/**
 *
 * @author jimmy
 */
public class AstConstraint implements Serializable {

    private final AstClafer context;
    private AstBoolExpr expr;
    private boolean soft = false;

    AstConstraint(AstClafer context, AstBoolExpr expr) {
        this.context = Check.notNull(context);
        this.expr = Check.notNull(expr);
    }

    public AstClafer getContext() {
        return context;
    }

    public boolean isHard() {
        return !isSoft();
    }

    public AstConstraint asHard() {
        soft = false;
        return this;
    }

    public boolean isSoft() {
        return soft;
    }

    public AstConstraint asSoft() {
        soft = true;
        return this;
    }

    public AstBoolExpr getExpr() {
        return expr;
    }

    public void setExpr(AstBoolExpr expr) {
        this.expr = expr;
    }

    @Override
    public String toString() {
        return isHard() ? "[" + expr + "]" : "(" + expr + ")";
    }
}
