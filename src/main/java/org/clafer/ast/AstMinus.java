package org.clafer.ast;

/**
 *
 * @author jimmy
 */
public record AstMinus(AstSetExpr expr) implements AstSetExpr {

    @Override
    public String toString() {
        return "-(" + expr + ")";
    }
}
