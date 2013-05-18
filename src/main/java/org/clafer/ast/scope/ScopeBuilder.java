package org.clafer.ast.scope;

import java.util.HashMap;
import java.util.Map;
import org.clafer.ast.AstClafer;

/**
 *
 * @author jimmy
 */
public class ScopeBuilder {

    private final Map<AstClafer, Integer> scope = new HashMap<AstClafer, Integer>();
    private int defaultScope = 1;
    private int intLow = -16;
    private int intHigh = 16;

    ScopeBuilder() {
    }

    public ScopeBuilder $(AstClafer clafer, int scope) {
        this.scope.put(clafer, scope);
        return this;
    }

    public ScopeBuilder defaultScope(int defaultScope) {
        this.defaultScope = defaultScope;
        return this;
    }

    public ScopeBuilder intLow(int intLow) {
        this.intLow = intLow;
        return this;
    }

    public ScopeBuilder intHigh(int intHigh) {
        this.intHigh = intHigh;
        return this;
    }

    public Scope toScope() {
        return new Scope(scope, defaultScope, intLow, intHigh);
    }
}
