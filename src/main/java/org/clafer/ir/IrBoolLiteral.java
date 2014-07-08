//package org.clafer.ir;
//
//import org.clafer.common.Check;
//
///**
// *
// * @author jimmy
// */
//public class IrBoolLiteral extends IrAbstractBool {
//
//    private final IrBoolVar var;
//
//    IrBoolLiteral(IrBoolVar var, IrBoolDomain domain) {
//        super(domain);
//        this.var = Check.notNull(var);
//    }
//
//    public IrBoolVar getVar() {
//        return var;
//    }
//
//    @Override
//    public IrBoolExpr negate() {
//        return new IrNot(this, getDomain().invert());
//    }
//
//    @Override
//    public boolean isNegative() {
//        return false;
//    }
//
//    @Override
//    public <A, B> B accept(IrBoolExprVisitor<A, B> visitor, A a) {
//        return visitor.visit(this, a);
//    }
//
//    @Override
//    public boolean equals(Object obj) {
//        if (obj instanceof IrBoolLiteral) {
//            IrBoolLiteral other = (IrBoolLiteral) obj;
//            return var.equals(other.var) && super.equals(other);
//        }
//        return false;
//    }
//
//    @Override
//    public int hashCode() {
//        return 127 * var.hashCode();
//    }
//
//    @Override
//    public String toString() {
//        return var.toString();
//    }
//}
