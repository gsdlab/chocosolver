package org.clafer.ir;

import org.clafer.domain.Domain;

/**
 *
 * @author jimmy
 */
public abstract class IrAbstractSet implements IrSetExpr {

    private final Domain env, ker, card;
    private final boolean isConstant;

    IrAbstractSet(Domain env, Domain ker, Domain card) {
        if (!ker.isSubsetOf(env)) {
            throw new IllegalSetException();
        }
        if (card.isEmpty()) {
            throw new IllegalSetException();
        }
        if (card.getLowBound() > env.size()) {
            throw new IllegalSetException(card.getLowBound() + " > " + env.size());
        }
        if (card.getHighBound() < ker.size()) {
            throw new IllegalSetException(card.getHighBound() + " < " + ker.size());
        }

        if (ker.size() == env.size() || ker.size() == card.getHighBound()) {
            this.env = ker;
            this.ker = ker;
            this.card = card;
            this.isConstant = true;
        } else if (env.size() == card.getLowBound()) {
            this.env = env;
            this.ker = env;
            this.card = card;
            this.isConstant = true;
        } else {
            this.env = env;
            this.ker = ker;
            this.card = card;
            this.isConstant = false;
        }
    }

    @Override
    public Domain getEnv() {
        return env;
    }

    @Override
    public Domain getKer() {
        return ker;
    }

    @Override
    public Domain getCard() {
        return card;
    }

    @Override
    public boolean isConstant() {
        return isConstant;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof IrAbstractSet) {
            IrAbstractSet other = (IrAbstractSet) obj;
            return env.equals(other.env) && ker.equals(other.ker) && card.equals(other.card);
        }
        return false;
    }

    @Override
    public int hashCode() {
        // Subclasses can choose not to call this hashCode function since it can
        // be expensive.
        return env.hashCode() ^ ker.hashCode() ^ card.hashCode();
    }
}
