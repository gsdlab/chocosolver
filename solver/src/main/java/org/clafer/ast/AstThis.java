package org.clafer.ast;

/**
 * Do NOT reuse for different expressions.
 * 
 * @author jimmy
 */
public record AstThis() implements AstSetExpr {

    @Override
    public String toString() {
        return "this";
    }
}
