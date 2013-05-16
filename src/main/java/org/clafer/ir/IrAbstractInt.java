package org.clafer.ir;

import org.clafer.Check;

/**
 *
 * @author jimmy
 */
public abstract class IrAbstractInt implements IrInt {

    private final IrDomain domain;

    public IrAbstractInt(IrDomain domain) {
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
