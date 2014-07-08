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

    public IrSortSets(IrSetExpr[] sets, BoolDomain domain) {
        super(domain);
        this.sets = Check.noNullsNotEmpty(sets);
    }

    public IrSetExpr[] getSets() {
        return sets;
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
            return Arrays.deepEquals(sets, other.sets);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return Arrays.deepHashCode(sets);
    }
}
