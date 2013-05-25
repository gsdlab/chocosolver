package org.clafer.scope;

import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import org.clafer.common.Check;
import org.clafer.ast.AstClafer;

/**
 * <p>
 * An immutable mapping from Clafers to their scope. Also contains the scope of
 * integers for solving, like the bit-width in Alloy, but more flexible because
 * the lowest and highest integers can be set indepedently and does not have to
 * be a power of 2.
 * </p>
 * <p>
 * The scope is built through the constructor or {@link ScopeBuilder}, whichever
 * is mort convenient.
 * </p>
 * <p>
 * Example 1:
 * <pre>
 * Map&lt;AstClafer, Integer&gt; map = new HashMap&lt;AstClafer, Integer&gt();
 * map.put(claferA, 3);
 * map.put(claferB, 2);
 * Scope scope = new Scope(map, 3, -16, 16);
 * </pre>
 * </p>
 * <p>
 * Example 2:
 * <pre>
 * Scope scope = Scope.set(claferA, 3).set(claferB).defaultScope(3).intLow(-16).intHigh(16);
 * </pre>
 * </p>
 * <p>
 * Both examples construct the same scope.
 * </p>
 *
 * <p>
 * What is scope? Because of the expressiveness of Clafer, reasoning is too
 * difficult in general. The scope is a limitation of the solver so that
 * reasoning feasible.
 * <p>
 * Example 1:
 * <pre>
 * A : int
 * B : int
 * C : int
 * [A * A * A + B * B * B = C * C * C]
 * </pre>
 * </p>
 * <p>
 * Example 2:
 * <pre>
 * A *
 * B *
 * C *
 * [#A * #A * #A + #B * #B * #B = #C * #C * #C]
 * </pre>
 * </p>
 * Both examples are attempting to prove/disprove Fermat's last theorem for k=3,
 * although this easily generalizes for any k. Unfortunately this is too
 * difficult, so the solver only attempts to prove/disprove upto a certain
 * scope. In example 1, a scope of
 * {@code Scope.intLow(-16).intHigh(16).toScope()} would only attempt the proof
 * for integers between negative and positive 16. In example 2, a scope of
 * {@code Scope.defaultScope(16)} would only attemp the proof for integers
 * between 0 and positive 16.
 *
 * @author jimmy
 * @see ScopeBuilder
 */
public class Scope {

    private final Map<AstClafer, Integer> scopes;
    private final int defaultScope;
    private final int intLow, intHigh;

    /**
     * Construct a new scope. Altering the map has no affect once the scope is
     * constructed.
     *
     * @param scopes a map of Clafers to their scopes
     * @param defaultScope the scope for unspecified Clafers
     * @param intLow the lowest (inclusive) integer used for solving
     * @param intHigh the highest (inclusive) integer used for solving
     */
    public Scope(Map<AstClafer, Integer> scopes, int defaultScope, int intLow, int intHigh) {
        if (defaultScope <= 0) {
            throw new IllegalArgumentException("Default scope must be positive");
        }
        for (Entry<AstClafer, Integer> entry : scopes.entrySet()) {
            if (entry.getValue() <= 0) {
                throw new IllegalArgumentException(entry.getKey().getName() + " scope set to " + entry.getValue() + ". Scope must be positive.");
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
     * Returns the scope of the Clafer. If the Clafer is not specified a
     * specific scope during the construction of this object, then the default
     * scope is returned.
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

    /**
     * Construct the scope using the builder pattern.
     *
     * @see ScopeBuilder
     * @return a new builder
     */
    public static ScopeBuilder builder() {
        return new ScopeBuilder();
    }

    /**
     * Equivalent to {@code builder().set(clafer, scope)}.
     *
     * @param clafer the Clafer
     * @param scope the scope of the clafer
     * @return a new builder
     */
    public static ScopeBuilder set(AstClafer clafer, int scope) {
        return builder().set(clafer, scope);
    }

    /**
     * Equivalent to {@code builder().defaultScope(defaultScope)}.
     *
     * @param defaultScope the default scope
     * @return a new builder
     */
    public static ScopeBuilder defaultScope(int defaultScope) {
        return builder().defaultScope(defaultScope);
    }

    /**
     * Equivalent to {@code builder().intLow(intLow)}.
     *
     * @param intLow the lowest integer
     * @return a new builder
     */
    public static ScopeBuilder intLow(int intLow) {
        return builder().intLow(intLow);
    }

    /**
     * Equivalent to {@code builder().intHigh(intHigh)}.
     *
     * @param intHigh the highest integer
     * @return a new builder
     */
    public static ScopeBuilder intHigh(int intHigh) {
        return builder().intHigh(intHigh);
    }

    /**
     * {@inheritDoc}
     */
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
