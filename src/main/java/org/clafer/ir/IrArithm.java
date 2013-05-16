package org.clafer.ir;

import org.clafer.Check;
import org.clafer.Util;
import org.clafer.ir.IrDomain.IrBoundDomain;

/**
 * Equivalent to: foldl1 op operands
 * 
 * @author jimmy
 */
public class IrArithm extends IrAbstractIntExpr implements IrIntExpr {

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
