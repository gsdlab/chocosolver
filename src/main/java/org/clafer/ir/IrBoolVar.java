package org.clafer.ir;

import org.clafer.Check;

/**
 *
 * @author jimmy
 */
public class IrBoolVar implements IrBoolExpr {

    private final String name;
    private final Boolean value;

    IrBoolVar(String name) {
        this(name, null);
    }

    IrBoolVar(String name, Boolean value) {
        this.name = Check.notNull(name);
        this.value = value;
    }

    public String getName() {
        return name;
    }

    public boolean isConstant() {
        return value != null;
    }

    public boolean isTrue() {
        return isConstant() && value.booleanValue();
    }

    public boolean isFalse() {
        return isConstant() && !value.booleanValue();
    }

    @Override
    public IrBoolExpr opposite() {
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
