package org.clafer.ir;

import org.clafer.domain.Domain;
import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public abstract class IrAbstractInt implements IrIntExpr {

    private final Domain domain;

    public IrAbstractInt(Domain domain) {
        this.domain = Check.notNull(domain);

        if (domain.isEmpty()) {
            throw new IllegalIntException();
        }
    }

    @Override
    public Domain getDomain() {
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
        if (obj instanceof IrAbstractInt) {
            IrAbstractInt other = (IrAbstractInt) obj;
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
