package org.clafer.ast;

/**
 * Dynamic dispatch for AST expressions.
 *
 * @param <A> the parameter type
 * @param <B> the return type
 * @author jimmy
 */
public interface AstExprVisitor<A, B> {

    public B visit(AstThis ast, A a);

    public B visit(AstGlobal ast, A a);

    public B visit(AstConstant ast, A a);

    public B visit(AstStringConstant ast, A a);

    public B visit(AstJoin ast, A a);

    public B visit(AstJoinParent ast, A a);

    public B visit(AstJoinRef ast, A a);

    public B visit(AstNot ast, A a);

    public B visit(AstMinus ast, A a);

    public B visit(AstCard ast, A a);

    public B visit(AstSetTest ast, A a);

    public B visit(AstCompare ast, A a);

    public B visit(AstArithm ast, A a);

    public B visit(AstMod ast, A a);

    public B visit(AstSum ast, A a);

    public B visit(AstProduct ast, A a);

    public B visit(AstBoolArithm ast, A a);

    public B visit(AstDifference ast, A a);

    public B visit(AstIntersection ast, A a);

    public B visit(AstUnion ast, A a);

    public B visit(AstMembership ast, A a);

    public B visit(AstTernary ast, A a);

    public B visit(AstIfThenElse ast, A a);

    public B visit(AstDowncast ast, A a);

    public B visit(AstUpcast ast, A a);

    public B visit(AstLocal ast, A a);

    public B visit(AstQuantify ast, A a);

    public B visit(AstLength ast, A a);

    public B visit(AstConcat ast, A a);

    public B visit(AstPrefix ast, A a);

    public B visit(AstSuffix ast, A a);

    public B visit(AstChildRelation ast, A a);

    public B visit(AstParentRelation ast, A a);

    public B visit(AstRefRelation ast, A a);

    public B visit(AstInverse ast, A a);

    public B visit(AstTransitiveClosure ast, A a);
}
