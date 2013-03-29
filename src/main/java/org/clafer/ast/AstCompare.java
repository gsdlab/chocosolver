package org.clafer.ast;

import org.clafer.Check;

/**
 *
 * @author jimmy
 */
public class AstCompare implements AstBoolExpression {

    private final AstSetExpression left;
    private final Op op;
    private final AstSetExpression right;

    AstCompare(AstSetExpression left, Op op, AstSetExpression right) {
        this.left = Check.notNull(left);
        this.op = Check.notNull(op);
        this.right = Check.notNull(right);
    }

    public Op getOp() {
        return op;
    }

    public AstSetExpression getLeft() {
        return left;
    }

    public AstSetExpression getRight() {
        return right;
    }

    @Override
    public <A, B> B accept(AstExpressionVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    public static enum Op {

        Equal("="),
        NotEqual("!="),
        LessThan("<"),
        LessThanEqual("<="),
        GreaterThan(">"),
        GreaterThanEqual(">=");
        private final String syntax;

        private Op(String syntax) {
            this.syntax = syntax;
        }

        public String getSyntax() {
            return syntax;
        }
    }
}
