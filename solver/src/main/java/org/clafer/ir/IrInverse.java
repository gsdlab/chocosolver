package org.clafer.ir;

import org.clafer.common.Check;
import org.clafer.domain.Domain;

/**
 *
 * @author jimmy
 */
public class IrInverse extends IrAbstractSetArray {

    private final IrSetArrayExpr relation;

    IrInverse(IrSetArrayExpr relation, Domain[] envDomains, Domain[] kerDomains, Domain[] cardDomains) {
        super(envDomains, kerDomains, cardDomains);
        this.relation = Check.notNull(relation);
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
        if (obj instanceof IrInverse) {
            IrInverse other = (IrInverse) obj;
            return relation.equals(other.relation);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return ~relation.hashCode();
    }

    @Override
    public String toString() {
        return "~" + relation;
    }
}
