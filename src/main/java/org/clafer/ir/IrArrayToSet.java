package org.clafer.ir;

import gnu.trove.set.TIntSet;
import gnu.trove.set.hash.TIntHashSet;
import org.clafer.Check;

/**
 *
 * @author jimmy
 */
public class IrArrayToSet implements IrSetExpr {

    private final IrIntExpr[] array;

    public IrArrayToSet(IrIntExpr[] array) {
        if (array.length == 0) {
            throw new IllegalArgumentException();
        }
        this.array = Check.noNulls(array);
    }

    public IrIntExpr[] getArray() {
        return array;
    }

    @Override
    public IrDomain getEnv() {
        IrDomain domain = array[0].getDomain();
        for (int i = 1; i < array.length; i++) {
            domain = IrUtil.union(domain, array[i].getDomain());
        }
        return domain;
    }

    @Override
    public IrDomain getKer() {
        TIntSet values = new TIntHashSet();
        for (IrIntExpr i : array) {
            Integer constant = IrUtil.getConstant(i);
            if (constant != null) {
                values.add(constant.intValue());
            }
        }
        return Irs.enumDomain(values);
    }

    @Override
    public IrDomain getCard() {
        return Irs.boundDomain(1, array.length);
    }

    @Override
    public <A, B> B accept(IrSetExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }
}
