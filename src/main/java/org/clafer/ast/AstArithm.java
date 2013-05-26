package org.clafer.ast;

import java.util.Arrays;
import org.clafer.common.Check;
import org.clafer.common.Util;

/**
 * foldl1 op operands.
 *
 * @author jimmy
 */
public class AstArithm implements AstSetExpr {

    private final Op op;
    private final AstSetExpr[] operands;

    AstArithm(Op op, AstSetExpr[] operands) {
        this.op = Check.notNull(op);
        this.operands = Check.noNullsNotEmpty(operands);
    }

    public Op getOp() {
        return op;
    }

    public AstSetExpr[] getOperands() {
        return operands;
    }

    @Override
    public <A, B> B accept(AstExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof AstArithm) {
            AstArithm other = (AstArithm) obj;
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

        Add("+"),
        Sub("-"),
        Mul("*"),
        Div("/");
        private final String syntax;

        private Op(String syntax) {
            this.syntax = syntax;
        }

        public String getSyntax() {
            return syntax;
        }
    }
}
