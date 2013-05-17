package org.clafer.ir;

import org.clafer.Check;

/**
 *
 * @author jimmy
 */
public class IrNot extends IrAbstractBool implements IrBoolExpr {

    private final IrBoolVar var;

    public IrNot(IrBoolVar var, IrBoolDomain domain) {
        super(domain);
        this.var = Check.notNull(var);
    }

    public IrBoolVar getVar() {
        return var;
    }

    @Override
    public IrBoolExpr negate() {
        return new IrBoolLiteral(var, getDomain().invert());
    }

    @Override
    public boolean isNegative() {
        return true;
    }

    @Override
    public <A, B> B accept(IrBoolExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof IrNot) {
            IrNot other = (IrNot) obj;
            return var.equals(other.var) && super.equals(other);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return ~var.hashCode();
    }

    @Override
    public String toString() {
        return "!" + var;
    }
}
