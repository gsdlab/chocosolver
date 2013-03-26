package org.clafer.ir;

import org.clafer.Check;

/**
 *
 * @author jimmy
 */
public class IrUnion implements IrSetExpr {

    private final IrSetExpr[] operands;

    IrUnion(IrSetExpr[] operands) {
        this.operands = Check.noNulls(operands);
    }

    public IrSetExpr[] getOperands() {
        return operands;
    }

    @Override
    public <A, B> B accept(IrSetExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public String toString() {
        StringBuilder result = new StringBuilder();
        for (int i = 0; i < operands.length; i++) {
            if (i > 0) {
                result.append(" ∪ ");
            }
            result.append('(').append(operands[i]).append(')');
        }
        return result.toString();
    }
}
