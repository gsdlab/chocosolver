package org.clafer.ir;

import org.clafer.Check;

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
    public String toString() {
        return var.toString();
    }
}
