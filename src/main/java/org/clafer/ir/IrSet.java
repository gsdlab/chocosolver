package org.clafer.ir;

/**
 *
 * @author jimmy
 */
public interface IrSet {

    /**
     * Env or envelope is the union of all possible values the set can be.
     * 
     * @return the env domain
     */
    public IrDomain getEnv();

    /**
     * Ker or kernel is the intersection of all possible values the set can be
     * 
     * @return the ker domain
     */
    public IrDomain getKer();

    /**
     * Card or cardinality is all the possible size of the set.
     * 
     * @return the card domain
     */
    public IrDomain getCard();
}
