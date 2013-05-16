package org.clafer.ir;

import org.clafer.Check;

/**
 *
 * @author jimmy
 */
public abstract class IrAbstractIntExpr implements IrIntExpr {

    private final IrDomain domain;

    public IrAbstractIntExpr(IrDomain domain) {
        this.domain = Check.notNull(domain);

        if (domain.isEmpty()) {
            throw new IllegalArgumentException();
        }
    }

    /** {@inheritDoc} */
    @Override
    public IrDomain getDomain() {
        return domain;
    }
}
