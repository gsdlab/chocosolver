package org.clafer.ir;

import org.clafer.Check;
import org.clafer.Util;
import org.clafer.ir.IrDomain.IrBoundDomain;

/**
 * Equivalent to: foldl1 op operands
 * 
 * @author jimmy
 */
public class IrArithm implements IrIntExpr {

    private final Op op;
    private final IrIntExpr[] operands;

    IrArithm(Op op, IrIntExpr... operands) {
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
    public IrDomain getDomain() {
        IrDomain domain = operands[0].getDomain();
        int low = domain.getLowerBound();
        int high = domain.getUpperBound();

        switch (op) {
            case Add:
                for (int i = 1; i < operands.length; i++) {
                    domain = operands[i].getDomain();
                    low += domain.getLowerBound();
                    high += domain.getUpperBound();
                }
                return new IrBoundDomain(low, high);
            case Sub:
                for (int i = 1; i < operands.length; i++) {
                    domain = operands[i].getDomain();
                    low -= domain.getUpperBound();
                    high -= domain.getLowerBound();
                }
                return new IrBoundDomain(low, high);
            case Mul:
                for (int i = 1; i < operands.length; i++) {
                    domain = operands[i].getDomain();
                    low = Util.min(low * domain.getLowerBound(), low * domain.getUpperBound(),
                            high * domain.getLowerBound(), high * domain.getUpperBound());
                    high = Util.max(low * domain.getLowerBound(), low * domain.getUpperBound(),
                            high * domain.getLowerBound(), high * domain.getUpperBound());
                }
                return new IrBoundDomain(low, high);
            case Div:
                for (int i = 1; i < operands.length; i++) {
                    domain = operands[i].getDomain();
                    low = Util.min(low / domain.getLowerBound(), low / domain.getUpperBound(),
                            high * domain.getLowerBound(), high * domain.getUpperBound());
                    high = Util.max(low / domain.getLowerBound(), low / domain.getUpperBound(),
                            high / domain.getLowerBound(), high / domain.getUpperBound());
                }
                return new IrBoundDomain(low, high);
            default:
                throw new IllegalStateException();
        }
    }

    @Override
    public <A, B> B accept(IrIntExprVisitor< A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public String toString() {
        StringBuilder result = new StringBuilder();
        result.append(operands[0]);
        for (int i = 1; i < operands.length; i++) {
            result.append(' ').append(op.getSyntax()).append(' ').append(operands[1]);
        }
        return result.toString();
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
