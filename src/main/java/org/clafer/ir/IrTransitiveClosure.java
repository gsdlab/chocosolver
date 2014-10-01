package org.clafer.ir;

import org.clafer.domain.Domain;

/**
 *
 * @author jimmy
 */
public class IrTransitiveClosure extends IrAbstractSetArray {

    private final IrSetArrayExpr relation;

    public IrTransitiveClosure(IrSetArrayExpr relation, Domain[] envDomains, Domain[] kerDomains, Domain[] cardDomains) {
        super(envDomains, kerDomains, cardDomains);
        this.relation = relation;
    }

    public IrSetArrayExpr getRelation() {
        return relation;
    }

    @Override
    public <A, B> B accept(IrSetArrayExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof IrTransitiveClosure) {
            IrTransitiveClosure other = (IrTransitiveClosure) obj;
            return relation.equals(other.relation);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return 51 * relation.hashCode();
    }

    @Override
    public String toString() {
        return "transitiveClosure(" + relation + ")";
    }
}
