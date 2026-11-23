package org.clafer.ast;

/**
 * A factory for creating unique identifiers.
 *
 * @author jimmy
 */
public class AstIdFactory {

    protected int count = 0;

    /**
     * Create a new unique identifier.
     *
     * @return a unique identifier
     */
    public int newId() {
        return count++;
    }
}
