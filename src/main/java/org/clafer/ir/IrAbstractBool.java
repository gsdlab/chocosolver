package org.clafer.ir;

import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public abstract class IrAbstractBool implements IrBool {

    private final IrBoolDomain domain;

    public IrAbstractBool(IrBoolDomain domain) {
        this.domain = Check.notNull(domain);
    }

    /** {@inheritDoc} */
    @Override
    public IrBoolDomain getDomain() {
        return domain;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof IrAbstractBool) {
            IrAbstractBool other = (IrAbstractBool) obj;
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
