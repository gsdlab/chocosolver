package org.clafer.ir;

import org.clafer.domain.BoolDomain;
import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public abstract class IrAbstractBool implements IrBoolExpr {

    private final BoolDomain domain;

    public IrAbstractBool(BoolDomain domain) {
        this.domain = Check.notNull(domain);
    }

    @Override
    public BoolDomain getDomain() {
        return domain;
    }

    @Override
    public int getLowBound() {
        return domain.getLowBound();
    }

    @Override
    public int getHighBound() {
        return domain.getHighBound();
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
        // Subclasses can choose not to call this hashCode function since it can
        // be expensive.
        return domain.hashCode();
    }
}
