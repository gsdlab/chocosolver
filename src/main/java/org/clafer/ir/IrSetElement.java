package org.clafer.ir;

import org.clafer.common.Check;
import org.clafer.domain.Domain;

/**
 * Returns array[index].
 *
 * @author jimmy
 */
public class IrSetElement extends IrAbstractSet {

    private final IrSetArrayExpr array;
    private final IrIntExpr index;

    IrSetElement(IrSetArrayExpr array, IrIntExpr index, Domain env, Domain ker, Domain card) {
        super(env, ker, card);
        this.array = Check.notNull(array);
        this.index = Check.notNull(index);
    }

    public IrSetArrayExpr getArray() {
        return array;
    }

    public IrIntExpr getIndex() {
        return index;
    }

    @Override
    public <A, B> B accept(IrSetExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof IrSetElement) {
            IrSetElement other = (IrSetElement) obj;
            return array.equals(other.array) && index.equals(other.index);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return array.hashCode() ^ index.hashCode();
    }

    @Override
    public String toString() {
        return array + "[" + index + "]";
    }
}
