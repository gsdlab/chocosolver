package org.clafer;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import org.clafer.ast.AstClafer;

/**
 * Immutable.
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

    public int getScope(AstClafer clafer) {
        Integer scope = scopes.get(Check.notNull(clafer));
        if (scope == null) {
            return defaultScope;
        }
        return scope.intValue();
    }

    public int getDefaultScope() {
        return defaultScope;
    }

    public int getIntLow() {
        return intLow;
    }

    public int getIntHigh() {
        return intHigh;
    }

    public static ScopeBuilder builder() {
        return new ScopeBuilder();
    }

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
