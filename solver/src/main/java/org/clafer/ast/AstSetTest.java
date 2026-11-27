package org.clafer.ast;

import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public record AstSetTest(AstSetExpr left, Op op, AstSetExpr right) implements AstBoolExpr {

    public AstSetTest {
        left = Check.notNull(left);
        op = Check.notNull(op);
        right = Check.notNull(right);
    }

    @Override
    public String toString() {
        return left + " " + op.getSyntax() + " " + right;
    }

    public static enum Op {

        Equal("="),
        NotEqual("!=");
        private final String syntax;

        private Op(String syntax) {
            this.syntax = syntax;
        }

        public Op negate() {
            switch (this) {
                case Equal:
                    return NotEqual;
                case NotEqual:
                    return Equal;
                default:
                    throw new IllegalStateException();
            }
        }

        public String getSyntax() {
            return syntax;
        }
    }
}
