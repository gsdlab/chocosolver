package org.clafer.ir;

import org.clafer.Check;

/**
 *
 * @author jimmy
 */
public abstract class IrAbstractBoolExpr implements IrBoolExpr {

    private final IrBoolDomain domain;

    public IrAbstractBoolExpr(IrBoolDomain domain) {
        this.domain = Check.notNull(domain);
    }

    public IrBoolDomain getDomain() {
        return domain;
    }
}
