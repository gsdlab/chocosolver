package org.clafer.ir;

import org.clafer.domain.Domain;
import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class IrSingletonFilter extends IrAbstractSet {

    private final IrIntExpr value;
    private final int filter;

    IrSingletonFilter(IrIntExpr value, int filter, Domain env, Domain ker, Domain card) {
        super(env, ker, card);
        this.value = Check.notNull(value);
        if (ker.size() > 1) {
            throw new IllegalArgumentException();
        }
        this.filter = filter;
    }

    public IrIntExpr getValue() {
        return value;
    }

    public int getFilter() {
        return filter;
    }

    @Override
    public <A, B> B accept(IrSetExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof IrSingleton) {
            IrSingletonFilter other = (IrSingletonFilter) obj;
            return value.equals(other.value) && filter == other.filter && super.equals(other);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return 511 * value.hashCode() ^ filter;
    }

    @Override
    public String toString() {
        return "filter" + filter + "{" + value + "}";
    }
}
