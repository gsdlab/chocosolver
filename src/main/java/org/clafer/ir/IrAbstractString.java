package org.clafer.ir;

import java.util.Arrays;
import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public abstract class IrAbstractString implements IrStringExpr {

    private final IrIntExpr length;
    private final IrIntExpr[] chars;

    IrAbstractString(IrIntExpr length, IrIntExpr[] chars) {
        this.length = Check.notNull(length);
        this.chars = Check.noNulls(chars);

        if (length.getDomain().isEmpty()) {
            throw new IllegalArgumentException();
        }
        if (length.getDomain().getLowBound() < 0) {
            throw new IllegalArgumentException();
        }
        if (length.getDomain().getHighBound() > chars.length) {
            throw new IllegalArgumentException();
        }
        for (IrIntExpr c : chars) {
            if (c.getDomain().getLowBound() < Character.MIN_VALUE) {
                throw new IllegalArgumentException();
            }
            if (c.getDomain().getHighBound() > Character.MAX_VALUE) {
                throw new IllegalArgumentException();
            }
        }
    }

    @Override
    public IrIntExpr getLength() {
        return length;
    }

    @Override
    public IrIntExpr[] getChars() {
        return chars;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof IrAbstractString) {
            IrAbstractString other = (IrAbstractString) obj;
            return length.equals(other.length) && Arrays.equals(chars, other.chars);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return length.hashCode() ^ Arrays.hashCode(chars);
    }
}
