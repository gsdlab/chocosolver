package org.clafer.ir;

import org.clafer.Check;

/**
 *
 * @author jimmy
 */
public class IrUnion implements IrSetExpr {

    private final IrSetExpr[] operands;

    IrUnion(IrSetExpr[] operands) {
        if (operands.length == 0) {
            throw new IllegalArgumentException();
        }
        this.operands = Check.noNulls(operands);
    }

    public IrSetExpr[] getOperands() {
        return operands;
    }

    @Override
    public IrDomain getEnv() {
        IrDomain env = operands[0].getEnv();
        for (int i = 1; i < operands.length; i++) {
            env = IrUtil.union(env, operands[i].getEnv());
        }
        return env;
    }

    @Override
    public IrDomain getKer() {
        IrDomain env = operands[0].getKer();
        for (int i = 1; i < operands.length; i++) {
            env = IrUtil.union(env, operands[i].getKer());
        }
        return env;
    }

    @Override
    public IrDomain getCard() {
        IrDomain card = operands[0].getCard();
        int low = card.getLowerBound();
        int high = card.getUpperBound();
        for (int i = 1; i < operands.length; i++) {
            card = operands[i].getCard();
            low = Math.max(low, card.getLowerBound());
            high += card.getUpperBound();
        }
        return Irs.boundDomain(
                Math.max(low, getKer().size()),
                Math.min(high, getEnv().size()));
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
