package org.clafer.ast;

/**
 *
 * @author jimmy
 */
public interface AstExprVisitor<A, B> {

    public B visit(AstThis ast, A a);

    public B visit(AstGlobal ast, A a);

    public B visit(AstConstant ast, A a);

    public B visit(AstJoin ast, A a);

    public B visit(AstJoinParent ast, A a);

    public B visit(AstJoinRef ast, A a);

    public B visit(AstCard ast, A a);

    public B visit(AstEqual ast, A a);

    public B visit(AstCompare ast, A a);

    public B visit(AstUpcast ast, A a);

    public B visit(AstNone ast, A a);

    public B visit(AstLocal ast, A a);

    public B visit(AstQuantify ast, A a);
}
