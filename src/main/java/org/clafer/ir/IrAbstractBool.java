package org.clafer.ir;

import org.clafer.Check;

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
}
