package org.clafer.ir;

import org.clafer.domain.Domain;
import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public abstract class IrAbstractSet implements IrSetExpr {

    private final Domain env, ker, card;

    public IrAbstractSet(Domain env, Domain ker, Domain card) {
        this.env = Check.notNull(env);
        this.ker = Check.notNull(ker);
        this.card = Check.notNull(card);

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
