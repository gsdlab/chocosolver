package org.clafer.ir;

import org.clafer.domain.Domain;
import java.util.Arrays;
import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public abstract class IrAbstractString implements IrStringExpr {

    private final Domain[] charDomains;
    private final Domain lengthDomain;

    IrAbstractString(Domain[] charDomains, Domain lengthDomain) {
        this.charDomains = Check.noNulls(charDomains);
        this.lengthDomain = Check.notNull(lengthDomain);

        for (Domain c : charDomains) {
            if (c.getLowBound() < Character.MIN_VALUE) {
                throw new IllegalStringException();
            }
            if (c.getHighBound() > Character.MAX_VALUE) {
                throw new IllegalStringException();
            }
        }
        for (int i = 0; i < charDomains.length && i < lengthDomain.getLowBound(); i++) {
            if (charDomains[i].size() == 1 && charDomains[i].getLowBound() == 0) {
                throw new IllegalStringException();
            }
        }
        for (int i = lengthDomain.getHighBound(); i < charDomains.length; i++) {
            if (!charDomains[i].contains(0)) {
                throw new IllegalStringException();
            }
        }
        if (lengthDomain.isEmpty()) {
            throw new IllegalStringException();
        }
        if (lengthDomain.getLowBound() < 0) {
            throw new IllegalStringException();
        }
        if (lengthDomain.getHighBound() > charDomains.length) {
            throw new IllegalStringException();
        }
    }

    @Override
    public Domain[] getChars() {
        return charDomains;
    }

    @Override
    public Domain getLength() {
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
