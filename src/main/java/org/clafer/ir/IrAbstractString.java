package org.clafer.ir;

import java.util.Arrays;
import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public abstract class IrAbstractString implements IrStringExpr {

    private final IrDomain[] charDomains;
    private final IrDomain lengthDomain;

    IrAbstractString(IrDomain[] charDomains, IrDomain lengthDomain) {
        this.charDomains = Check.noNulls(charDomains);
        this.lengthDomain = Check.notNull(lengthDomain);

        for (IrDomain c : charDomains) {
            if (c.getLowBound() < Character.MIN_VALUE) {
                throw new IllegalArgumentException();
            }
            if (c.getHighBound() > Character.MAX_VALUE) {
                throw new IllegalArgumentException();
            }
        }
        if (lengthDomain.isEmpty()) {
            throw new IllegalArgumentException();
        }
        if (lengthDomain.getLowBound() < 0) {
            throw new IllegalArgumentException();
        }
        if (lengthDomain.getHighBound() > charDomains.length) {
            throw new IllegalArgumentException();
        }
    }

    @Override
    public IrDomain[] getCharDomains() {
        return charDomains;
    }

    @Override
    public IrDomain getLengthDomain() {
        return lengthDomain;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof IrAbstractString) {
            IrAbstractString other = (IrAbstractString) obj;
            return Arrays.equals(charDomains, other.charDomains)
                    && lengthDomain.equals(other.lengthDomain);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return Arrays.hashCode(charDomains) ^ lengthDomain.hashCode();
    }
}
