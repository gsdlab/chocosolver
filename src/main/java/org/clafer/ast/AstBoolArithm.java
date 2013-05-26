package org.clafer.ast;

import java.util.Arrays;
import org.clafer.common.Check;
import org.clafer.common.Util;

/**
 * foldl1 op operands. Note that even implies is left-associative.
 *
 * @author jimmy
 */
public class AstBoolArithm implements AstBoolExpr {

    private final Op op;
    private final AstBoolExpr[] operands;

    AstBoolArithm(Op op, AstBoolExpr[] operands) {
        this.op = Check.notNull(op);
        this.operands = Check.noNullsNotEmpty(operands);
    }

    public Op getOp() {
        return op;
    }

    public AstBoolExpr[] getOperands() {
        return operands;
    }

    @Override
    public <A, B> B accept(AstExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof AstBoolArithm) {
            AstBoolArithm other = (AstBoolArithm) obj;
            return op.equals(other.op) && Arrays.equals(operands, other.operands);
        }
        return false;
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
