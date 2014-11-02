package org.clafer.ast;

import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class AstMembership implements AstBoolExpr {

    private final AstSetExpr member;
    private final Op op;
    private final AstSetExpr set;

    AstMembership(AstSetExpr member, Op op, AstSetExpr set) {
        this.member = Check.notNull(member);
        this.op = Check.notNull(op);
        this.set = Check.notNull(set);
    }

    public AstSetExpr getMember() {
        return member;
    }

    public Op getOp() {
        return op;
    }

    public AstSetExpr getSet() {
        return set;
    }

    @Override
    public <A, B> B accept(AstExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public String toString() {
        return member + " " + op + " " + set;
    }

    public static enum Op {

        In("in"),
        NotIn("not in");
        private final String syntax;

        private Op(String syntax) {
            this.syntax = syntax;
        }

        public Op negate() {
            switch (this) {
                case In:
                    return NotIn;
                case NotIn:
                    return In;
                default:
                    throw new IllegalStateException();
            }
        }

        public String getSyntax() {
            return syntax;
        }
    }
}
