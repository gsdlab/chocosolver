package org.clafer.ast;

import java.util.Arrays;
import org.clafer.common.Check;
import org.clafer.common.Util;

/**
 * foldl1 op operands. Note that even implies is left-associative.
 *
 * @author jimmy
 */
public record AstBoolArithm(Op op, AstBoolExpr[] operands) implements AstBoolExpr {

    public AstBoolArithm {
        op = Check.notNull(op);
        operands = Check.noNullsNotEmpty(operands);
    }

    @Override
    public boolean equals(Object obj) {
        return obj instanceof AstBoolArithm other
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

        And("&&"),
        IfOnlyIf("<=>"),
        Implies("=>"),
        Or("||"),
        Xor("xor");
        private final String syntax;

        private Op(String syntax) {
            this.syntax = syntax;
        }

        public String getSyntax() {
            return syntax;
        }
    }
}
