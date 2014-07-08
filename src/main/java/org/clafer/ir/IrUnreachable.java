package org.clafer.ir;

import org.clafer.domain.BoolDomain;
import java.util.Arrays;
import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class IrUnreachable extends IrAbstractBool {

    private final IrIntExpr[] edges;
    private final int from, to;

    public IrUnreachable(IrIntExpr[] edges, int from, int to, BoolDomain domain) {
        super(domain);
        this.edges = Check.noNullsNotEmpty(edges);
        this.from = from;
        this.to = to;
    }

    public IrIntExpr[] getEdges() {
        return edges;
    }

    public int getFrom() {
        return from;
    }

    public int getTo() {
        return to;
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
        if (obj instanceof IrUnreachable) {
            IrUnreachable other = (IrUnreachable) obj;
            return Arrays.equals(edges, other.edges)
                    && from == other.from && to == other.to
                    && super.equals(other);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return 17 * Arrays.hashCode(edges);
    }

    @Override
    public String toString() {
        return "unreachable(from=" + from + ", to=" + to + ", " + Arrays.toString(edges) + ")";
    }
}
