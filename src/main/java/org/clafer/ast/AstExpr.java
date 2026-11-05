package org.clafer.ast;

/**
 * An expression in the AST language. Expressions are immutable records,
 * except {@link AstLocal}, so a pass over them is a {@code switch} with a case
 * for each kind of expression, which the compiler checks for exhaustiveness.
 *
 * @author jimmy
 */
public sealed interface AstExpr permits AstSetExpr, AstBoolExpr {
}
