package org.clafer.ir;

import java.util.Arrays;
import org.clafer.common.Check;
import org.clafer.common.Util;

/**
 * Equivalent to: foldl1 op operands
 * 
 * @author jimmy
 */
public class IrArithm extends IrAbstractInt implements IrIntExpr {

    private final Op op;
    private final IrIntExpr[] operands;

    IrArithm(Op op, IrIntExpr[] operands, IrDomain domain) {
        super(domain);
        this.op = Check.notNull(op);
        this.operands = Check.noNulls(operands);
        if (operands.length < 2) {
            throw new IllegalArgumentException();
        }
    }

    public Op getOp() {
        return op;
    }

    public IrIntExpr[] getOperands() {
        return operands;
    }

    @Override
    public <A, B> B accept(IrIntExprVisitor< A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof IrArithm) {
            IrArithm other = (IrArithm) obj;
            return op.equals(other.op) && Arrays.equals(operands, other.operands) && super.equals(other);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return op.hashCode() ^ Arrays.hashCode(operands);
    }

    @Override
    public String toString() {
        return Util.intercalate(" " + op.getSyntax() + " ", operands);
    }

    public static enum Op {

        Add("+"),
        Sub("-"),
        Mul("*"),
        Div("/");
        private final String syntax;

        Op(String syntax) {
            this.syntax = syntax;
        }

        public String getSyntax() {
            return syntax;
        }
    }
}
