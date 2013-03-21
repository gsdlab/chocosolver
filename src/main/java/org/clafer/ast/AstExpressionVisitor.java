package org.clafer.ast;

/**
 *
 * @author jimmy
 */
public interface AstExpressionVisitor<A, B> {

    public B visit(AstThis ast, A a);

    public B visit(AstConstantInt ast, A a);

    public B visit(AstJoin ast, A a);

    public B visit(AstJoinParent ast, A a);

    public B visit(AstJoinRef ast, A a);

    public B visit(AstCard ast, A a);

    public B visit(AstCompare ast, A a);

    public B visit(AstUpcast ast, A a);

    public B visit(AstNone ast, A a);
}
