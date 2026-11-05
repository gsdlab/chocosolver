package org.clafer.ast;

/**
 * An expression whose value is a set, an integer, a decimal or a string.
 *
 * @author jimmy
 */
public sealed interface AstSetExpr extends AstExpr permits
        AstArithm, AstCard, AstChildRelation, AstConcat, AstConstant,
        AstDecimalConstant, AstDifference, AstDomainRestriction, AstDowncast,
        AstGlobal, AstIntersection, AstInverse, AstJoin, AstJoinParent,
        AstJoinRef, AstLength, AstLocal, AstMax, AstMin, AstMinus, AstMod,
        AstParentRelation, AstProduct, AstRangeRestriction, AstRefRelation,
        AstStringConstant, AstSum, AstTernary, AstThis, AstTransitiveClosure,
        AstUnion, AstUpcast {
}
