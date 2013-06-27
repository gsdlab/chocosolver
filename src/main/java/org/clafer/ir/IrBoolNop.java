package org.clafer.ir;

import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class IrBoolNop extends IrAbstractBool implements IrBoolExpr, IrNop {

    private final IrBoolVar var;

    public IrBoolNop(IrBoolVar var) {
        super(IrBoolDomain.BoolDomain);
        this.var = Check.notNull(var);
    }

    @Override
    public IrBoolVar getVar() {
        return var;
    }

    @Override
    public IrBoolExpr negate() {
        return this;
    }

    @Override
    public boolean isNegative() {
        return false;
    }

    @Override
    public <A, B> B accept(IrBoolExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof IrBoolNop) {
            IrBoolNop other = (IrBoolNop) obj;
            return var.equals(other.var) && super.equals(other);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return var.hashCode();
    }

    @Override
    public String toString() {
        return "nop(" + var + ")";
    }
}
