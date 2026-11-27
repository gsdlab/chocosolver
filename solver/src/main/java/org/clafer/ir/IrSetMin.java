package org.clafer.ir;

import org.clafer.common.Check;
import org.clafer.domain.Domain;

/**
 *
 * @author jimmy
 */
public class IrSetMin extends IrAbstractInt {

    private final IrSetExpr set;
    // The value if set is empty.
    private final int defaultValue;

    public IrSetMin(IrSetExpr set, int defaultValue, Domain domain) {
        super(domain);
        this.set = Check.notNull(set);
        this.defaultValue = defaultValue;
    }

    public IrSetExpr getSet() {
        return set;
    }

    public int getDefaultValue() {
        return defaultValue;
    }

    @Override
    public <A, B> B accept(IrIntExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof IrSetMin) {
            IrSetMin other = (IrSetMin) obj;
            return set.equals(other.set) && defaultValue == other.defaultValue && super.equals(other);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return 103 * set.hashCode() ^ defaultValue;
    }

    @Override
    public String toString() {
        return "min(" + set + ", default=" + defaultValue + ")";
    }
}
