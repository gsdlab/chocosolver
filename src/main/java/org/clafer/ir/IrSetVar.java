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

    @Override
    public <A, B> B accept(IrSetExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public String toString() {
        return name;
    }
}
