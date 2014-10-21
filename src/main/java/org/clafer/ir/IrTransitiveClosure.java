package org.clafer.ir;

import org.clafer.domain.Domain;

/**
 *
 * @author jimmy
 */
public class IrTransitiveClosure extends IrAbstractSetArray {

    private final IrSetArrayExpr relation;
    private final boolean reflexive;

    public IrTransitiveClosure(IrSetArrayExpr relation, boolean reflexive, Domain[] envDomains, Domain[] kerDomains, Domain[] cardDomains) {
        super(envDomains, kerDomains, cardDomains);
        this.relation = relation;
        this.reflexive = reflexive;
    }

    public IrSetArrayExpr getRelation() {
        return relation;
    }

    public boolean isReflexive() {
        return reflexive;
    }

    @Override
    public <A, B> B accept(IrSetArrayExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof IrTransitiveClosure) {
            IrTransitiveClosure other = (IrTransitiveClosure) obj;
            return relation.equals(other.relation) && reflexive == other.reflexive;
        }
        return false;
    }

    @Override
    public int hashCode() {
        return 51 * relation.hashCode() ^ (reflexive ? 16 : 0);
    }

    @Override
    public String toString() {
        return reflexive ? "transitiveReflexiveClosure(" + relation + ")" : "transitiveClosure(" + relation + ")";
    }
}
