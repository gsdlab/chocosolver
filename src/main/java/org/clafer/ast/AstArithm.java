package org.clafer.ast;

import java.util.Arrays;
import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class AstArithm implements AstSetExpr {

    private final Op op;
    private final AstSetExpr[] operands;

    AstArithm(Op op, AstSetExpr[] operands) {
        this.op = Check.notNull(op);
        this.operands = Check.notNull(operands);
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
        return op.hashCode() ^ Arrays.hashCode(operands);
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
