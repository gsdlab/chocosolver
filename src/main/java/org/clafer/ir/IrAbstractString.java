package org.clafer.ir;

import java.util.Arrays;
import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public abstract class IrAbstractString implements IrStringExpr {

    private final IrDomain lengthDomain;
    private final IrDomain[] charDomains;

    IrAbstractString(IrDomain lengthDomain, IrDomain[] charDomains) {
        this.lengthDomain = Check.notNull(lengthDomain);
        this.charDomains = Check.noNulls(charDomains);

        if (lengthDomain.isEmpty()) {
            throw new IllegalArgumentException();
        }
        if (lengthDomain.getLowBound() < 0) {
            throw new IllegalArgumentException();
        }
        if (lengthDomain.getHighBound() > charDomains.length) {
            throw new IllegalArgumentException();
        }
        for (IrDomain c : charDomains) {
            if (c.getLowBound() < Character.MIN_VALUE) {
                throw new IllegalArgumentException();
            }
            if (c.getHighBound() > Character.MAX_VALUE) {
                throw new IllegalArgumentException();
            }
        }
    }

    @Override
    public IrDomain getLengthDomain() {
        return lengthDomain;
    }

    @Override
    public IrDomain[] getCharDomains() {
        return charDomains;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof IrAbstractString) {
            IrAbstractString other = (IrAbstractString) obj;
            return lengthDomain.equals(other.lengthDomain) && Arrays.equals(charDomains, other.charDomains);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return lengthDomain.hashCode() ^ Arrays.hashCode(charDomains);
    }
}
