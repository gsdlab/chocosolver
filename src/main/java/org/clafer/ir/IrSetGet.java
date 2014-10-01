package org.clafer.ir;

import org.clafer.common.Check;
import org.clafer.domain.Domain;

/**
 *
 * @author jimmy
 */
public class IrSetGet extends IrAbstractSet {

    private final IrSetArrayExpr array;
    private final int index;

    public IrSetGet(IrSetArrayExpr array, int index, Domain env, Domain ker, Domain card) {
        super(env, ker, card);
        if (index < 0 || index >= array.length()) {
            throw new IllegalArgumentException(index + " is out of range");
        }
        this.array = Check.notNull(array);
        this.index = index;
    }

    public IrSetArrayExpr getArray() {
        return array;
    }

    public int getIndex() {
        return index;
    }

    @Override
    public <A, B> B accept(IrSetExprVisitor<A, B> visitor, A a) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof IrSetGet) {
            IrSetGet other = (IrSetGet) obj;
            return array.equals(other.array) && index == other.index;
        }
        return false;
    }

    @Override
    public int hashCode() {
        return 723 * array.hashCode() ^ index;
    }

    @Override
    public String toString() {
        return array + "[" + index + "]";
    }
}
