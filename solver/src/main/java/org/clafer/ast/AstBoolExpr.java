package org.clafer.ast;

/**
 * An expression whose value is true or false.
 *
 * @author jimmy
 */
public sealed interface AstBoolExpr extends AstExpr permits
        AstBoolArithm, AstCompare, AstConnected, AstIfThenElse, AstMembership,
        AstNot, AstPrefix, AstQuantify, AstSetTest, AstSuffix {
}
