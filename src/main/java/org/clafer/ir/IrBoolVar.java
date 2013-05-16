package org.clafer.ir;

import org.clafer.Check;

/**
 *
 * @author jimmy
 */
public class IrBoolVar extends IrAbstractBoolExpr implements IrVarExpr {

    private final String name;

    IrBoolVar(String name, IrBoolDomain domain) {
        super(domain);
        this.name = Check.notNull(name);
    }

    public String getName() {
        return name;
    }

    @Override
    public IrBoolExpr negate() {
        return new IrNot(this);
    }

    @Override
    public <A, B> B accept(IrBoolExprVisitor<A, B> visitor, A a) {
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
        return name;
    }
}
