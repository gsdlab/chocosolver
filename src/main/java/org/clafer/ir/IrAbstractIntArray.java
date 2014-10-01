package org.clafer.ir;

import java.util.Arrays;
import org.clafer.common.Check;
import org.clafer.domain.Domain;

/**
 *
 * @author jimmy
 */
public abstract class IrAbstractIntArray implements IrIntArrayExpr {

    private final Domain[] domains;

    IrAbstractIntArray(Domain[] domains) {
        this.domains = Check.noNulls(domains);
    }

    @Override
    public int length() {
        return domains.length;
    }

    @Override
    public Domain[] getDomains() {
        return domains;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof IrAbstractIntArray) {
            IrAbstractIntArray other = (IrAbstractIntArray) obj;
            return Arrays.equals(domains, other.domains);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return Arrays.hashCode(domains);
    }
}
