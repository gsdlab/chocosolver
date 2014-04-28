package org.clafer.ir;

import org.clafer.domain.BoolDomain;
import java.util.Arrays;
import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class IrAcyclic extends IrAbstractBool {

    private final IrIntExpr[] edges;

    public IrAcyclic(IrIntExpr[] edges, BoolDomain domain) {
        super(domain);
        this.edges = Check.noNullsNotEmpty(edges);
    }

    public IrIntExpr[] getEdges() {
        return edges;
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
        if (obj instanceof IrAcyclic) {
            IrAcyclic other = (IrAcyclic) obj;
            return Arrays.equals(edges, other.edges) && super.equals(other);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return 739 * Arrays.hashCode(edges);
    }

    @Override
    public String toString() {
        return "acyclic(" + Arrays.toString(edges) + ")";
    }
}
