package org.clafer.ast.scope;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import org.clafer.Check;
import org.clafer.ast.AstClafer;

/**
 * An immutable mapping from Clafers to their scope. Also contains the scope of
 * integers for solving, like the bit-width in Alloy, but more flexible because
 * the lowest and highest integers can be set indepedently and does not have to
 * be a power of 2.
 * 
 * The scope can be built with the constructor or through {@link builder()},
 * whichever is more convenient.
 * 
 * @author jimmy
 */
public class Scope {

    private final Map<AstClafer, Integer> scopes;
    private final int defaultScope;
    private final int intLow, intHigh;

    public Scope(int defaultScope) {
        this(defaultScope, -16, 16);
    }

    public Scope(int defaultScope, int intLow, int intHigh) {
        this(Collections.<AstClafer, Integer>emptyMap(), defaultScope, intLow, intHigh);
    }

    public Scope(Map<AstClafer, Integer> scopes) {
        this(scopes, 1, -16, 16);
    }

    public Scope(Map<AstClafer, Integer> scopes, int defaultScope) {
        this(scopes, defaultScope, -16, 16);
    }

    public Scope(Map<AstClafer, Integer> scopes, int defaultScope, int intLow, int intHigh) {
        if (defaultScope <= 0) {
            throw new IllegalArgumentException("Default scope must be positive");
        }
        for (Integer value : scopes.values()) {
            if (value <= 0) {
                throw new IllegalArgumentException("Scope must be positive");
            }
        }
        if (intLow > intHigh) {
            throw new IllegalArgumentException("intLow(" + intLow + " > intHigh(" + intHigh + ")");
        }
        this.scopes = new HashMap<AstClafer, Integer>(scopes);
        this.defaultScope = defaultScope;
        this.intLow = intLow;
        this.intHigh = intHigh;
    }

    /**
     * Returns the scope of the Clafer. If the Clafer is not specified a specific scope
     * during the construction of this object, then the default scope is returned.
     * 
     * @param clafer the Clafer
     * @return the scope of the Clafer
     */
    public int getScope(AstClafer clafer) {
        Integer scope = scopes.get(Check.notNull(clafer));
        if (scope == null) {
            return defaultScope;
        }
        return scope.intValue();
    }

    /**
     * The scope for unspecified Clafers.
     * 
     * @return the default scope
     */
    public int getDefaultScope() {
        return defaultScope;
    }

    /**
     * Returns the lowest (inclusive) integer used for solving.
     * 
     * @return the lowest integer
     */
    public int getIntLow() {
        return intLow;
    }

    /**
     * Returns the highest (inclusive) integer used for solving.
     * 
     * @return the highest integer
     */
    public int getIntHigh() {
        return intHigh;
    }

    public static ScopeBuilder builder() {
        return new ScopeBuilder();
    }

    /** {@inheritDoc} */
    @Override
    public String toString() {
        StringBuilder result = new StringBuilder().append('{');
        result.append("default:").append(defaultScope);
        for (Entry<AstClafer, Integer> entry : scopes.entrySet()) {
            result.append(", ").append(entry.getKey()).append(':').append(entry.getValue());
        }
        return result.append('}').toString();
    }
}
