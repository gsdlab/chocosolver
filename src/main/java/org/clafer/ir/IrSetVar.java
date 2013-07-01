package org.clafer.ir;

import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class IrSetVar extends IrAbstractSet implements IrSetExpr, IrVar {

    private final String name;
    private final boolean decision;

    IrSetVar(String name, IrDomain env, IrDomain ker, IrDomain card) {
        this(name, env, ker, card, true);
    }

    IrSetVar(String name, IrDomain env, IrDomain ker, IrDomain card, boolean decision) {
        super(env, ker, card);
        this.name = Check.notNull(name);
        this.decision = decision;
    }

    @Override
    public String getName() {
        return name;
    }

    @Override
    public IrSetVar asDecision() {
        return new IrSetVar(name, getEnv(), getKer(), getCard(), true);
    }

    @Override
    public IrSetVar asNoDecision() {
        return new IrSetVar(name, getEnv(), getKer(), getCard(), false);
    }

    @Override
    public boolean isDecision() {
        return decision;
    }

    public IrSetVar withCard(IrDomain card) {
        return new IrSetVar(name, getEnv(), getKer(), card);
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
        return name + "{env=" + getEnv() + ", ker=" + getKer() + ", card=" + getCard() + "}";
    }
}
