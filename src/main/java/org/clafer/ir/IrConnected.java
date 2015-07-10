package org.clafer.ir;

import org.clafer.domain.BoolDomain;
import org.clafer.domain.Domain;

/**
 *
 * @author jimmy
 */
public class IrConnected extends IrAbstractBool {

    private final IrSetArrayExpr relation;
    private final IrSetExpr nodes;
    private final boolean directed;

    public IrConnected(IrSetExpr nodes, IrSetArrayExpr relation, boolean directed, BoolDomain domain) {
        super(domain);
        this.relation = relation;
        this.nodes = nodes;
        this.directed = directed;
    }

    public IrSetArrayExpr getRelation() {
        return relation;
    }

    public IrSetExpr getNodes() {
        return nodes;
    }

    public boolean isDirected() {
        return directed;
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
        if (obj instanceof IrConnected) {
            IrConnected other = (IrConnected) obj;
            return relation.equals(other.relation) && directed == other.directed;
        }
        return false;
    }

    @Override
    public int hashCode() {
        return 117 * nodes.hashCode() * relation.hashCode() ^ (directed ? 16 : 0);//51
    }

    @Override
    public String toString() {
        return directed ? "connectedDirected(" + nodes + " : " + relation + ")"
                : "connected(" + nodes + " : " + relation + ")";
    }
}
