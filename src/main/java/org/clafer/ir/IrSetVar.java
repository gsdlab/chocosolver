package org.clafer.ir;

import org.clafer.domain.Domain;
import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class IrSetVar extends IrAbstractSet implements IrVar {

    private final String name;
    private final IrIntVar card;

    protected IrSetVar(String name, Domain env, Domain ker, IrIntVar card) {
        super(env, ker, card.getDomain());
        this.name = Check.notNull(name);
        this.card = card;
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
        return this == obj;
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
