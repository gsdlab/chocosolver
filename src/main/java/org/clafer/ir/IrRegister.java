package org.clafer.ir;

import org.clafer.domain.BoolDomain;

/**
 *
 * @author jimmy
 */
public class IrRegister extends IrAbstractBool {

    private final IrVar variable;

    public IrRegister(IrVar variable) {
        super(variable instanceof IrConstant
                ? BoolDomain.TrueDomain
                : BoolDomain.TrueFalseDomain);
        this.variable = variable;
    }

    public IrVar getVariable() {
        return variable;
    }

    @Override
    public boolean isNegative() {
        throw new UnsupportedOperationException();
    }

    @Override
    public IrBoolExpr negate() {
        throw new UnsupportedOperationException();
    }

    @Override
    public <A, B> B accept(IrBoolExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public <A, B> B accept(IrIntExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof IrRegister) {
            IrRegister other = (IrRegister) obj;
            return variable.equals(other.variable);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return 973 ^ variable.hashCode();
    }

    @Override
    public String toString() {
        return "register(" + variable + ")";
    }
}
