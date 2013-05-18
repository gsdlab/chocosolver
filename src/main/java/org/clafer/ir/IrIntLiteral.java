package org.clafer.ir;

import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class IrIntLiteral extends IrAbstractInt implements IrIntExpr {

    private final IrIntVar var;

    IrIntLiteral(IrIntVar var, IrDomain domain) {
        super(domain);
        this.var = Check.notNull(var);
    }

    public IrIntVar getVar() {
        return var;
    }

    @Override
    public <A, B> B accept(IrIntExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof IrIntLiteral) {
            IrIntLiteral other = (IrIntLiteral) obj;
            return var.equals(other.var) && super.equals(other);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return 127 * var.hashCode();
    }

    @Override
    public String toString() {
        return var.toString();
    }
}
