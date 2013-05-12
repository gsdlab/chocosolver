package org.clafer.ir;

import org.clafer.Check;

/**
 *
 * @author jimmy
 */
public class IrIntVar implements IrIntExpr {

    private final String name;
    private final IrDomain domain;

    IrIntVar(String name, IrDomain domain) {
        this.name = Check.notNull(name);
        this.domain = Check.notNull(domain);

        if (domain.isEmpty()) {
            throw new IllegalArgumentException();
        }
    }

    public String getName() {
        return name;
    }

    @Override
    public IrDomain getDomain() {
        return domain;
    }

    @Override
    public <A, B> B accept(IrIntExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public String toString() {
        return name;
    }
}
