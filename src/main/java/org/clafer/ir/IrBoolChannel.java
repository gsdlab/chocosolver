package org.clafer.ir;

import java.util.Arrays;
import org.clafer.Check;

/**
 *
 * @author jimmy
 */
public class IrBoolChannel implements IrConstraint {

    private final IrBoolExpr[] bools;
    private final IrSetExpr set;

    IrBoolChannel(IrBoolExpr[] bools, IrSetExpr set) {
        this.bools = Check.noNulls(bools);
        this.set = Check.notNull(set);
    }

    public IrBoolExpr[] getBools() {
        return bools;
    }

    public IrSetExpr getSet() {
        return set;
    }

    @Override
    public <A, B> B accept(IrConstraintVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public String toString() {
        return "boolChannel(" + Arrays.toString(bools) + ", " + set + ")";
    }
}
