package org.clafer.ir;

import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class IrSetVar extends IrAbstractSet implements IrSetExpr, IrVar {

    private final String name;

    IrSetVar(String name, IrDomain env, IrDomain ker, IrDomain card) {
        super(env, ker, card);
        this.name = Check.notNull(name);
    }

    @Override
    public String getName() {
        return name;
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
