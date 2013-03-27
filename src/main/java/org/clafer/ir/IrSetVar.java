package org.clafer.ir;

import org.clafer.Check;

/**
 *
 * @author jimmy
 */
public class IrSetVar implements IrSetExpr {

    private final String name;
    private final IrDomain env, ker;

    IrSetVar(String name, IrDomain env, IrDomain ker) {
        this.name = Check.notNull(name);
        // TODO: ker subseteq env
        this.env = Check.notNull(env);
        this.ker = Check.notNull(ker);
    }

    public String getName() {
        return name;
    }

    public IrDomain getEnv() {
        return env;
    }

    public IrDomain getKer() {
        return ker;
    }

    public boolean isConstant() {
        return env.size() == ker.size();
    }

    public int[] getValue() {
        return env.getValues();
    }

    @Override
    public <A, B> B accept(IrSetExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public String toString() {
        return name;
    }
}
