package org.clafer.ir;

import org.clafer.Check;

/**
 *
 * @author jimmy
 */
public abstract class IrAbstractSet implements IrSet {

    private final IrDomain env, ker, card;

    public IrAbstractSet(IrDomain env, IrDomain ker, IrDomain card) {
        this.env = Check.notNull(env);
        this.ker = Check.notNull(ker);
        this.card = Check.notNull(card);

        assert IrUtil.isSubsetOf(ker, env);
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

    /** {@inheritDoc} */
    @Override
    public IrDomain getEnv() {
        return env;
    }

    /** {@inheritDoc} */
    @Override
    public IrDomain getKer() {
        return ker;
    }

    /** {@inheritDoc} */
    @Override
    public IrDomain getCard() {
        return card;
    }
}
