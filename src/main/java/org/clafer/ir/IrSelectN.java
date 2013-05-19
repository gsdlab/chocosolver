package org.clafer.ir;

import java.util.Arrays;
import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class IrSelectN extends IrAbstractBool implements IrBoolExpr {

    private final IrBoolExpr[] bools;
    private final IrIntExpr n;

    IrSelectN(IrBoolExpr[] bools, IrIntExpr n, IrBoolDomain domain) {
        super(domain);
        this.bools = Check.noNulls(bools);
        this.n = Check.notNull(n);
    }

    public IrBoolExpr[] getBools() {
        return bools;
    }

    public IrIntExpr getN() {
        return n;
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
    public String toString() {
        return "selectN(" + Arrays.toString(bools) + ", " + n + ")";
    }
}
