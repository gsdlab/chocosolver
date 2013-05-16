package org.clafer.ir;

import java.util.Arrays;
import org.clafer.Check;

/**
 * Returns array[index].
 * 
 * @author jimmy
 */
public class IrElement extends IrAbstractInt implements IrIntExpr {

    private final IrIntExpr[] array;
    private final IrIntExpr index;

    IrElement(IrIntExpr[] array, IrIntExpr index, IrDomain domain) {
        super(domain);
        this.array = Check.noNullsNotEmpty(array);
        this.index = Check.notNull(index);
    }

    public IrIntExpr getIndex() {
        return index;
    }

    public IrIntExpr[] getArray() {
        return array;
    }

    @Override
    public <A, B> B accept(IrIntExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public String toString() {
        return Arrays.toString(array) + "[" + index + "]";
    }
}
