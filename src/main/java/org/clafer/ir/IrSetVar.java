package org.clafer.ir;

import org.clafer.Check;

/**
 *
 * @author jimmy
 */
public class IrSetVar implements IrSetExpr {

    private final String name;
    private final IrDomain env, ker, card;

    IrSetVar(String name, IrDomain env, IrDomain ker, IrDomain card) {
        assert IrUtil.isSubsetOf(ker, env);
        this.name = Check.notNull(name);
        this.env = Check.notNull(env);
        this.ker = Check.notNull(ker);
        this.card = Check.notNull(card);

        if (card.isEmpty()) {
            throw new IllegalArgumentException();
        }
        if (card.getLowerBound() > env.size()) {
            throw new IllegalArgumentException(card.getLowerBound() + " > " + env.size());
        }
        if (card.getUpperBound() > env.size()) {
            throw new IllegalArgumentException(card.getUpperBound() + " > " + env.size());
        }
        if (card.getLowerBound() < ker.size()) {
            throw new IllegalArgumentException(card.getLowerBound() + " < " + ker.size());
        }
        if (card.getUpperBound() < ker.size()) {
            throw new IllegalArgumentException(card.getUpperBound() + " < " + ker.size());
        }
    }

    public String getName() {
        return name;
    }

    @Override
    public IrDomain getEnv() {
        return env;
    }

    @Override
    public IrDomain getKer() {
        return ker;
    }

    @Override
    public IrDomain getCard() {
        return card;
    }

    public boolean isConstant() {
        return env.size() == ker.size();
    }

    public int[] getValue() {
        assert isConstant();
        return env.getValues();
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
        return name;
    }
}
