package org.clafer.ir;

import org.clafer.common.Check;
import org.clafer.domain.Domain;
import org.clafer.domain.Domains;

/**
 *
 * @author jimmy
 */
public class IrIntVar extends IrAbstractInt implements IrVar {

    private final String name;

    IrIntVar(String name, Domain domain) {
        super(domain);
        this.name = Check.notNull(name);
    }

    IrIntVar(int constant) {
        this(Integer.toString(constant), Domains.constantDomain(constant));
    }

    @Override
    public String getName() {
        return name;
    }

    @Override
    public <A, B> B accept(IrIntExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj instanceof IrIntVar) {
            IrIntVar other = (IrIntVar) obj;
            if (isConstant() && other.isConstant()) {
                return getLowBound() == other.getLowBound();
            }
        }
        return false;
    }

    @Override
    public int hashCode() {
        return name.hashCode();
    }

    @Override
    public String toString() {
        // TODO: return only name
        return name + "{domain=" + getDomain() + "}";
    }
}
