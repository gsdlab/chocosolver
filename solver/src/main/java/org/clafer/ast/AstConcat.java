package org.clafer.ast;

/**
 *
 * @author jimmy
 */
public record AstConcat(AstSetExpr left, AstSetExpr right) implements AstSetExpr {

    @Override
    public String toString() {
        return left + " ++ " + right;
    }
}
