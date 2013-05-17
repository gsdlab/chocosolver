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

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof IrAbstractInt) {
            IrAbstractInt other = (IrAbstractInt) obj;
            return domain.equals(other.domain);
        }
        return false;
    }

    @Override
    public int hashCode() {
        // Subclasses can choose not to callthis hashCode function since it can
        // be expensive.
        return domain.hashCode();
    }
}
