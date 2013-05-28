package org.clafer.ast;

/**
 * A unique identifier. Even as the AST gets transformed during optimization,
 * the identifier will still identify the original nodes.
 *
 * @param <T> the type of AST node this id identifies
 * @author jimmy
 */
public class AstId<T> {

    // Only predefined nodes are allowed to possess negative identifiers.
    private final int id;

    public AstId(int id) {
        this.id = id;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean equals(Object obj) {
        if (obj instanceof AstId<?>) {
            AstId<?> other = (AstId<?>) obj;
            return id == other.id;
        }
        return false;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int hashCode() {
        return id;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String toString() {
        return Integer.toString(id);
    }
}