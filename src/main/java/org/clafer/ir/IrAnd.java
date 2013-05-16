package org.clafer.ir;

import org.clafer.Check;

/**
 *
 * @author jimmy
 */
public class IrAnd extends IrAbstractBool implements IrBoolExpr {

    private final IrBoolExpr[] operands;

    IrAnd(IrBoolExpr[] operands, IrBoolDomain domain) {
        super(domain);
        this.operands = Check.noNullsNotEmpty(operands);
    }

    public IrBoolExpr[] getOperands() {
        return operands;
    }

    @Override
    public IrBoolExpr negate() {
        IrBoolExpr[] negativeOperands = new IrBoolExpr[operands.length];
        for (int i = 0; i < negativeOperands.length; i++) {
            negativeOperands[i] = operands[i].negate();
        }
        return new IrOr(negativeOperands, getDomain().invert());
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
