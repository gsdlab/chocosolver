package org.clafer.ir;

import org.clafer.Check;

/**
 *
 * @author jimmy
 */
public class IrUnion extends IrAbstractSet implements IrSetExpr {

    private final IrSetExpr[] operands;

    IrUnion(IrSetExpr[] operands, IrDomain env, IrDomain ker, IrDomain card) {
        super(env, ker, card);
        this.operands = Check.noNullsNotEmpty(operands);
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
