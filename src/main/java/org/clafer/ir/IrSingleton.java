package org.clafer.ir;

import org.clafer.Check;

/**
 *
 * @author jimmy
 */
public class IrSingleton extends IrAbstractSet implements IrSetExpr {

    private final IrIntExpr value;

    IrSingleton(IrIntExpr value, IrDomain env, IrDomain ker) {
        super(env, ker, Irs.OneDomain);
        this.value = Check.notNull(value);
        if (ker.size() > 1) {
            throw new IllegalArgumentException();
        }
    }

    public IrIntExpr getValue() {
        return value;
    }

    @Override
    public <A, B> B accept(IrSetExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }
}
