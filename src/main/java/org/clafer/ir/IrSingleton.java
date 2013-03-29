package org.clafer.ir;

import org.clafer.Check;

/**
 *
 * @author jimmy
 */
public class IrSingleton implements IrSetExpr {

    private final IrIntExpr value;

    IrSingleton(IrIntExpr value) {
        this.value = Check.notNull(value);
    }

    public IrIntExpr getValue() {
        return value;
    }

    @Override
    public <A, B> B accept(IrSetExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }
}
