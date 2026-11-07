package org.clafer.ast;

import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public record AstMembership(AstSetExpr member, Op op, AstSetExpr set) implements AstBoolExpr {

    public AstMembership {
        member = Check.notNull(member);
        op = Check.notNull(op);
        set = Check.notNull(set);
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
