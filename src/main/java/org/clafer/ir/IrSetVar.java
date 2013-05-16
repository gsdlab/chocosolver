package org.clafer.ir;

import org.clafer.Check;

/**
 *
 * @author jimmy
 */
public class IrSetVar extends IrAbstractSetExpr implements IrVarExpr {

    private final String name;

    IrSetVar(String name, IrDomain env, IrDomain ker, IrDomain card) {
        super(env, ker, card);
        this.name = Check.notNull(name);
    }

    public String getName() {
        return name;
    }

    public boolean isConstant() {
        return getEnv().size() == getKer().size();
    }

    public int[] getValue() {
        assert isConstant();
        return getEnv().getValues();
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
        return name + "{env=" + getEnv() + ", ker=" + getKer() + "}";
    }
}
