package org.clafer.ast;

import java.util.Arrays;
import org.clafer.common.Check;
import org.clafer.common.Util;

/**
 * foldl1 op operands.
 *
 * @author jimmy
 */
public record AstArithm(Op op, AstSetExpr[] operands) implements AstSetExpr {

    public AstArithm {
        op = Check.notNull(op);
        operands = Check.noNullsNotEmpty(operands);
    }

    @Override
    public boolean equals(Object obj) {
        return obj instanceof AstArithm other
                && op == other.op && Arrays.equals(operands, other.operands);
    }

    @Override
    public int hashCode() {
        // op.hashCode() can change between runs which makes the output change
        // every time.
        return op.ordinal() ^ Arrays.hashCode(operands);
    }

    @Override
    public String toString() {
        return "(" + Util.intercalate(") " + op.getSyntax() + " (", operands) + ")";
    }

    public static enum Op {

        Add("+"),
        Sub("-"),
        Mul("*"),
        Div("/"),
        /**
         * Multiplication of decimals, rounded half away from zero to the
         * decimal scale. Introduced by type analysis.
         */
        DecimalMul("*"),
        /**
         * Division of decimals, rounded half away from zero to the decimal
         * scale. Introduced by type analysis.
         */
        DecimalDiv("/");
        private final String syntax;

        private Op(String syntax) {
            this.syntax = syntax;
        }

        public String getSyntax() {
            return syntax;
        }
    }
}
