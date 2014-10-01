package org.clafer.ir;

import java.util.Arrays;
import org.clafer.common.Check;
import org.clafer.domain.Domain;

/**
 *
 * @author jimmy
 */
public class IrSetArrayVar extends IrAbstractSetArray {

    private final IrSetExpr[] array;

    IrSetArrayVar(IrSetExpr[] array) {
        super(getEnvDomains(array), getKerDomains(array), getCardDomains(array));
        this.array = Check.noNulls(array);
    }

    private static Domain[] getEnvDomains(IrSetExpr[] array) {
        Domain[] envDomains = new Domain[array.length];
        for (int i = 0; i < envDomains.length; i++) {
            envDomains[i] = array[i].getEnv();
        }
        return envDomains;
    }

    private static Domain[] getKerDomains(IrSetExpr[] array) {
        Domain[] envDomains = new Domain[array.length];
        for (int i = 0; i < envDomains.length; i++) {
            envDomains[i] = array[i].getKer();
        }
        return envDomains;
    }

    private static Domain[] getCardDomains(IrSetExpr[] array) {
        Domain[] envDomains = new Domain[array.length];
        for (int i = 0; i < envDomains.length; i++) {
            envDomains[i] = array[i].getCard();
        }
        return envDomains;
    }

    public IrSetExpr[] getArray() {
        return array;
    }

    @Override
    public <A, B> B accept(IrSetArrayExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof IrSetArrayVar) {
            IrSetArrayVar other = (IrSetArrayVar) obj;
            return Arrays.equals(array, other.array);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return Arrays.hashCode(array);
    }

    @Override
    public String toString() {
        return Arrays.toString(array);
    }
}
