package org.clafer;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import org.clafer.ast.AstClafer;

/**
 *
 * @author jimmy
 */
public class Scope {

    private final int defaultScope;
    private final Map<AstClafer, Integer> scopes;

    public Scope(int defaultScope) {
        this(Collections.<AstClafer, Integer>emptyMap(), defaultScope);
    }

    public Scope(Map<AstClafer, Integer> scopes) {
        this(scopes, 1);
    }

    public Scope(Map<AstClafer, Integer> scopes, int defaultScope) {
        if (defaultScope <= 0) {
            throw new IllegalArgumentException("Default scope must be positive");
        }
        for(Integer value : scopes.values()) {
            if(value <= 0) {
                throw new IllegalArgumentException("Scope must be positive");
            }
        }
        this.scopes = new HashMap<AstClafer, Integer>(scopes);
        this.defaultScope = defaultScope;
    }

    public int getDefaultScope() {
        return defaultScope;
    }

    public int getScope(AstClafer clafer) {
        Integer scope = scopes.get(Check.notNull(clafer));
        if (scope == null) {
            return defaultScope;
        }
        return scope.intValue();
    }
}
