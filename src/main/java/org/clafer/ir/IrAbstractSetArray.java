package org.clafer.ir;

import java.util.Arrays;
import org.clafer.common.Check;
import org.clafer.domain.Domain;

/**
 *
 * @author jimmy
 */
public abstract class IrAbstractSetArray implements IrSetArrayExpr {

    private final Domain[] envDomains, kerDomains, cardDomains;

    IrAbstractSetArray(Domain[] envDomains, Domain[] kerDomains, Domain[] cardDomains) {
        if (envDomains.length != kerDomains.length) {
            throw new IllegalArgumentException();
        }
        if (envDomains.length != cardDomains.length) {
            throw new IllegalArgumentException();
        }
        this.envDomains = Check.noNulls(envDomains);
        this.kerDomains = Check.noNulls(kerDomains);
        this.cardDomains = Check.noNulls(cardDomains);
    }

    @Override
    public int length() {
        return envDomains.length;
    }

    @Override
    public Domain[] getEnvs() {
        return envDomains;
    }

    @Override
    public Domain[] getKers() {
        return kerDomains;
    }

    @Override
    public Domain[] getCards() {
        return cardDomains;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof IrAbstractSetArray) {
            IrAbstractSetArray other = (IrAbstractSetArray) obj;
            return Arrays.equals(envDomains, other.envDomains)
                    && Arrays.equals(kerDomains, other.kerDomains)
                    && Arrays.equals(cardDomains, other.cardDomains);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return Arrays.hashCode(envDomains) ^ Arrays.hashCode(kerDomains) ^ Arrays.hashCode(cardDomains);
    }
}
