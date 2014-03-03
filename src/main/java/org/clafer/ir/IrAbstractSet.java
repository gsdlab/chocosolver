package org.clafer.ir;

import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public abstract class IrAbstractSet implements IrSetExpr {

    private final IrDomain env, ker, card;

    public IrAbstractSet(IrDomain env, IrDomain ker, IrDomain card) {
        this.env = Check.notNull(env);
        this.ker = Check.notNull(ker);
        this.card = Check.notNull(card);

        if (!IrUtil.isSubsetOf(ker, env)) {
            throw new IllegalSetException();
        }
        if (card.isEmpty()) {
            throw new IllegalSetException();
        }
        if (card.getLowBound() > env.size()) {
            throw new IllegalSetException(card.getLowBound() + " > " + env.size());
        }
        if (card.getHighBound() > env.size()) {
            throw new IllegalSetException(card.getHighBound() + " > " + env.size());
        }
        if (card.getLowBound() < ker.size()) {
            throw new IllegalSetException(card.getLowBound() + " < " + ker.size());
        }
        if (card.getHighBound() < ker.size()) {
            throw new IllegalSetException(card.getHighBound() + " < " + ker.size());
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public IrDomain getEnv() {
        return env;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public IrDomain getKer() {
        return ker;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public IrDomain getCard() {
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
