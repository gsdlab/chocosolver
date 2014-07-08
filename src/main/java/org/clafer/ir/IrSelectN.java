package org.clafer.ir;

import org.clafer.domain.BoolDomain;
import java.util.Arrays;
import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class IrSelectN extends IrAbstractBool {

    private final IrBoolExpr[] bools;
    private final IrIntExpr n;

    IrSelectN(IrBoolExpr[] bools, IrIntExpr n, BoolDomain domain) {
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
    public <A, B> B accept(IrIntExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof IrSelectN) {
            IrSelectN other = (IrSelectN) obj;
            return Arrays.equals(bools, other.bools) && n.equals(other.n);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return Arrays.hashCode(bools) ^ n.hashCode();
    }

    @Override
    public String toString() {
        return "selectN(" + Arrays.toString(bools) + ", " + n + ")";
    }
}
