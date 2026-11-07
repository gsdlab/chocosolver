package org.clafer.ast;

import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public record AstCompare(AstSetExpr left, Op op, AstSetExpr right) implements AstBoolExpr {

    public AstCompare {
        left = Check.notNull(left);
        op = Check.notNull(op);
        right = Check.notNull(right);
    }

    @Override
    public String toString() {
        return left + " " + op.getSyntax() + " " + right;
    }

    public static enum Op {

        LessThan("<"),
        LessThanEqual("<="),
        GreaterThan(">"),
        GreaterThanEqual(">=");
        private final String syntax;

        private Op(String syntax) {
            this.syntax = syntax;
        }

        public Op negate() {
            switch (this) {
                case LessThan:
                    return GreaterThanEqual;
                case LessThanEqual:
                    return GreaterThan;
                case GreaterThan:
                    return LessThanEqual;
                case GreaterThanEqual:
                    return LessThan;
                default:
                    throw new IllegalStateException();
            }
        }

        public String getSyntax() {
            return syntax;
        }
    }
}
