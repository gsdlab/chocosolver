package org.clafer.ir;

import org.clafer.domain.Domain;
import org.clafer.common.Check;
import org.clafer.domain.Domains;

/**
 *
 * @author jimmy
 */
public class IrSingleton extends IrAbstractSet {

    private final IrIntExpr value;

    IrSingleton(IrIntExpr value, Domain env, Domain ker) {
        super(env, ker, Domains.OneDomain);
        this.value = Check.notNull(value);
        if (ker.size() > 1) {
            throw new IllegalArgumentException();
        }
    }

    public IrIntExpr getValue() {
        return value;
    }

    @Override
    public <A, B> B accept(IrSetExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof IrSingleton) {
            IrSingleton other = (IrSingleton) obj;
            return value.equals(other.value) && super.equals(other);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return 511 * value.hashCode();
    }

    @Override
    public String toString() {
        return "{" + value + "}";
    }
}
