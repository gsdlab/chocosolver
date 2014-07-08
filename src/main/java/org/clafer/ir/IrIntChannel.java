package org.clafer.ir;

import org.clafer.domain.BoolDomain;
import java.util.Arrays;
import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class IrIntChannel extends IrAbstractBool {

    private final IrIntExpr[] ints;
    private final IrSetExpr[] sets;

    IrIntChannel(IrIntExpr[] ints, IrSetExpr[] sets, BoolDomain domain) {
        super(domain);
        this.ints = Check.noNullsNotEmpty(ints);
        this.sets = Check.noNullsNotEmpty(sets);
    }

    public IrIntExpr[] getInts() {
        return ints;
    }

    public IrSetExpr[] getSets() {
        return sets;
    }

    @Override
    public boolean isNegative() {
        return false;
    }

    @Override
    public IrBoolExpr negate() {
        return new IrNot(this, getDomain().invert());
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
        if (obj instanceof IrIntChannel) {
            IrIntChannel other = (IrIntChannel) obj;
            return Arrays.equals(ints, other.ints) && Arrays.equals(sets, other.sets) && super.equals(other);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return Arrays.hashCode(ints) ^ Arrays.hashCode(sets);
    }

    @Override
    public String toString() {
        return "intChannel(" + Arrays.toString(ints) + ", " + Arrays.toString(sets) + ")";
    }
}
