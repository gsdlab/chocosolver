package org.clafer.ast;

/**
 * A factory for creating unique identifiers.
 *
 * @author jimmy
 */
public abstract class AstIdFactory {

    protected int count = 0;

    private AstIdFactory() {
    }

    /**
     * Create a new unique identifier.
     *
     * @param <T> the type of AST node to identify
     * @return a unique identifier
     */
    public abstract <T> AstId<T> newId();

    /**
     * Create a new general purpose identifier factory.
     *
     * @return a new identifier factory
     */
    public static AstIdFactory newIdFactory() {
        return new AstIdFactory() {
            @Override
            public <T> AstId<T> newId() {
                return new AstId<T>(count++);
            }
        };
    }
    /**
     * A special identifier factory for some predefined nodes.
     */
    static AstIdFactory PredefinedIdFactory = new AstIdFactory() {
        @Override
        public <T> AstId<T> newId() {
            return new AstId<T>(--count);
        }
    };
}
