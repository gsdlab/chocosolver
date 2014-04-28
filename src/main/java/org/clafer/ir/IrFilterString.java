package org.clafer.ir;

import org.clafer.domain.BoolDomain;
import java.util.Arrays;
import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class IrFilterString extends IrAbstractBool {

    private final IrSetExpr set;
    private final int offset;
    private final IrIntExpr[] string;
    private final IrIntExpr[] result;

    public IrFilterString(IrSetExpr set, int offset, IrIntExpr[] string, IrIntExpr[] result, BoolDomain domain) {
        super(domain);
        if (offset < 0) {
            throw new IllegalArgumentException();
        }
        this.set = set;
        this.offset = offset;
        this.string = Check.noNullsNotEmpty(string);
        this.result = Check.noNullsNotEmpty(result);
    }

    public IrSetExpr getSet() {
        return set;
    }

    public int getOffset() {
        return offset;
    }

    public IrIntExpr[] getString() {
        return string;
    }

    public IrIntExpr[] getResult() {
        return result;
    }

    @Override
    public IrBoolExpr negate() {
        return new IrNot(this, getDomain().invert());
    }

    @Override
    public boolean isNegative() {
        return false;
    }

    @Override
    public <A, B> B accept(IrBoolExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public <A, B> B accept(IrIntExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof IrFilterString) {
            IrFilterString other = (IrFilterString) obj;
            return set.equals(other.set)
                    && Arrays.equals(string, other.string)
                    && Arrays.equals(result, other.result)
                    && super.equals(other);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return set.hashCode() ^ Arrays.deepHashCode(string) ^ Arrays.hashCode(result);
    }

    @Override
    public String toString() {
        return "filterString(" + set + " >> " + offset + ", " + Arrays.toString(string) + ", " + Arrays.toString(result) + ")";
    }
}
