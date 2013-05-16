package org.clafer.ir;

import org.clafer.Check;

/**
 *
 * @author jimmy
 */
public class IrSetLiteral extends IrAbstractSet implements IrSetExpr {

    private final IrSetVar var;

    IrSetLiteral(IrSetVar var, IrDomain env, IrDomain ker, IrDomain card) {
        super(env, ker, card);
        this.var = Check.notNull(var);
    }

    public IrSetVar getVar() {
        return var;
    }

    @Override
    public <A, B> B accept(IrSetExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public String toString() {
        return var.toString();
    }
}