package org.clafer.ir;

import org.clafer.domain.BoolDomain;
import java.util.Arrays;
import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class IrSortSets extends IrAbstractBool {

    private final IrSetExpr[] sets;
    private final IrIntExpr[] bounds;

    public IrSortSets(IrSetExpr[] sets, IrIntExpr[] bounds, BoolDomain domain) {
        super(domain);
        if (sets.length != bounds.length) {
            throw new IllegalArgumentException();
        }
        this.sets = Check.noNullsNotEmpty(sets);
        this.bounds = Check.noNullsNotEmpty(bounds);
    }

    public IrSetExpr[] getSets() {
        return sets;
    }

    public IrIntExpr[] getBounds() {
        return bounds;
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
        if (obj instanceof IrSortSets) {
            IrSortSets other = (IrSortSets) obj;
            return Arrays.equals(sets, other.sets) && Arrays.equals(bounds, other.bounds);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return Arrays.deepHashCode(sets) ^ Arrays.deepHashCode(bounds);
    }
}
