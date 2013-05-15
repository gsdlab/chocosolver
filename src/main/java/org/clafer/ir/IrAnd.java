package org.clafer.ir;

import org.clafer.Check;

/**
 *
 * @author jimmy
 */
public class IrAnd implements IrBoolExpr {

    private final IrBoolExpr[] operands;

    IrAnd(IrBoolExpr[] operands) {
        this.operands = Check.noNulls(operands);
    }

    public IrBoolExpr[] getOperands() {
        return operands;
    }

    @Override
    public IrBoolExpr opposite() {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public <A, B> B accept(IrBoolExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public String toString() {
        StringBuilder result = new StringBuilder();

        for (int i = 0; i < operands.length; i++) {
            if (i > 0) {
                result.append(" & ");
            }
            result.append('(').append(operands[i]).append(')');
        }

        return result.toString();
    }
}
