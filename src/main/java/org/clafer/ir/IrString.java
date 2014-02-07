package org.clafer.ir;

import java.util.Arrays;

/**
 *
 * @author jimmy
 */
public class IrString extends IrAbstractString {

    IrString(IrIntExpr length, IrIntExpr[] chars) {
        super(length, chars);
    }

    @Override
    public <A, B> B accept(IrStringExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public String toString() {
        return Arrays.toString(getChars()) + "[length=" + getLength() + "]";
    }
}
