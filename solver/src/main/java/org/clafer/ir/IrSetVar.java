package org.clafer.ir;

import org.clafer.common.Check;
import org.clafer.domain.Domain;

/**
 *
 * @author jimmy
 */
public class IrSetVar extends IrAbstractSet implements IrVar {

    private final String name;
    private final IrIntVar card;

    IrSetVar(String name, Domain env, Domain ker, IrIntVar card) {
        super(env, ker, card.getDomain());
        this.name = Check.notNull(name);
        this.card = card;
    }

    IrSetVar(Domain constant) {
        this(constant.toString(), constant, constant, Irs.constant(constant.size()));
    }

    @Override
    public String getName() {
        return name;
    }

    public IrIntVar getCardVar() {
        return card;
    }

    @Override
    public <A, B> B accept(IrSetExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj instanceof IrSetVar) {
            IrSetVar other = (IrSetVar) obj;
            if (isConstant() && other.isConstant()) {
                return getKer().equals(other.getKer());
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
        // TODO: should only print name
        return name + "{env=" + getEnv() + ", ker=" + getKer() + ", card=" + card + "}";
    }
}
