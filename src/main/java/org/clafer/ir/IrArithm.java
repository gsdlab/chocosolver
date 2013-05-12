package org.clafer.ir;

import org.clafer.Check;
import org.clafer.ir.IrDomain.IrBoundDomain;

/**
 *
 * @author jimmy
 */
public class IrArithm implements IrIntExpr {
    
    private final IrIntExpr left;
    private final Op op;
    private final IrIntExpr right;
    
    IrArithm(IrIntExpr left, Op op, IrIntExpr right) {
        this.left = Check.notNull(left);
        this.op = Check.notNull(op);
        this.right = Check.notNull(right);
    }
    
    public IrIntExpr getLeft() {
        return left;
    }
    
    public IrIntExpr getRight() {
        return right;
    }
    
    public Op getOp() {
        return op;
    }
    
    @Override
    public IrDomain getDomain() {
        IrDomain leftDomain = left.getDomain();
        int leftLow = leftDomain.getLowerBound();
        int leftHigh = leftDomain.getUpperBound();
        IrDomain rightDomain = right.getDomain();
        int rightLow = rightDomain.getLowerBound();
        int rightHigh = rightDomain.getUpperBound();
        
        switch (op) {
            case Add:
                return new IrBoundDomain(leftLow + rightLow, leftHigh + rightHigh);
            case Sub:
                return new IrBoundDomain(leftLow - rightHigh, leftHigh - rightLow);
            case Mul:
                return new IrBoundDomain(leftLow * rightLow, leftHigh * rightHigh);
            case Div:
                // TODO: this bound can be tighter
                return new IrBoundDomain(leftLow, leftHigh);
            default:
                throw new IllegalStateException();
        }
    }
    
    @Override
    public <A, B> B accept(IrIntExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }
    
    @Override
    public String toString() {
        return left + " " + op.getSyntax() + " " + right;
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
