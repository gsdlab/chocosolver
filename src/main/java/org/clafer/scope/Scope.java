package org.clafer.scope;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import org.clafer.ast.AstClafer;
import org.clafer.common.Check;

/**
 * <p>
 * An immutable mapping from Clafers to their scope. Also contains the scope of
 * integers for solving, like the bit-width in Alloy, but more flexible because
 * the lowest and highest integers can be set independently and does not have to
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
 * Scope scope = Scope.set(claferA, 3).set(claferB).defaultScope(3).intLow(-16).intHigh(16).toScope();
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
 * {@code Scope.defaultScope(16).toScope()} would only attempt the proof for
 * integers between 0 and positive 16 (the example uses encodes integers using
 * cardinality).
 *
 * @author jimmy
 * @see ScopeBuilder
 */
public class Scope implements Scopable {

    private final Map<AstClafer, Integer> scopes;
    private final int defaultScope;
    private final int intLow, intHigh;
    private final int mulLow, mulHigh;
    private final int stringLength;
    private final char charLow, charHigh;

    /**
     * Construct a new scope. Altering the map has no affect once the scope is
     * constructed.
     *
     * @param scopes a map of Clafers to their scopes
     * @param defaultScope the scope for unspecified Clafers
     * @param intLow the lowest (inclusive) integer used for solving
     * @param intHigh the highest (inclusive) integer used for solving
     * @param mulLow the lowest (inclusive) integer used for solving
     * multiplication
     * @param mulHigh the highest (inclusive) integer used for solving
     * multiplication
     * @param stringLength the longest (inclusive) string used for solving
     * @param charLow the lowest (inclusive) character used for solving
     * @param charHigh the highest (inclusive) character used for solving
     */
    public Scope(Map<AstClafer, Integer> scopes, int defaultScope, int intLow, int intHigh,
            int mulLow, int mulHigh, int stringLength, char charLow, char charHigh) {
        if (defaultScope <= 0) {
            throw new IllegalArgumentException("Default scope must be positive");
        }
        for (Entry<AstClafer, Integer> entry : scopes.entrySet()) {
            if (entry.getValue() < 0) {
                throw new IllegalArgumentException(entry.getKey().getName() + " scope set to " + entry.getValue() + ". Scope must be non-negative.");
            }
        }
        if (intLow > intHigh) {
            throw new IllegalArgumentException("intLow(" + intLow + ") > intHigh(" + intHigh + ")");
        }
        if (mulLow > mulHigh) {
            throw new IllegalArgumentException("mulLow(" + mulLow + ") > mulHigh(" + mulHigh + ")");
        }
        if (stringLength < 0) {
            throw new IllegalArgumentException("stringLength cannot be negative");
        }
        if (charLow > charHigh) {
            throw new IllegalArgumentException("charLow(" + charLow + ") > charHigh(" + charHigh + ")");
        }
        this.scopes = new HashMap<>(scopes);
        this.defaultScope = defaultScope;
        this.intLow = intLow;
        this.intHigh = intHigh;
        this.mulLow = mulLow;
        this.mulHigh = mulHigh;
        this.stringLength = stringLength;
        this.charLow = charLow;
        this.charHigh = charHigh;
    }

    /**
     * Returns the set of Clafers that have an explicit scope.
     *
     * @return the scoped Clafers
     */
    public Set<AstClafer> getScoped() {
        return Collections.unmodifiableSet(scopes.keySet());
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
        return scope == null ? defaultScope : scope.intValue();
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
     * Returns the lowest (inclusive) integer used for solving multiplication.
     *
     * @return the lowest integer
     */
    public int getMulLow() {
        return mulLow;
    }

    /**
     * Returns the highest (inclusive) integer used for solving multiplication.
     *
     * @return the highest integer
     */
    public int getMulHigh() {
        return mulHigh;
    }

    /**
     * Returns the longest (inclusive) string used for solving.
     *
     * @return the longest string
     */
    public int getStringLength() {
        return stringLength;
    }

    /**
     * Returns the lowest (inclusive) character used for solving.
     *
     * @return the lowest character
     */
    public char getCharLow() {
        return charLow;
    }

    /**
     * Returns the highest (inclusive) character used for solving.
     *
     * @return the highest character
     */
    public char getCharHigh() {
        return charHigh;
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
     * Equivalent to {@code builder().setScope(clafer, scope)}.
     *
     * @param clafer the Clafer
     * @param scope the scope of the clafer
     * @return a new builder
     */
    public static ScopeBuilder setScope(AstClafer clafer, int scope) {
        return builder().setScope(clafer, scope);
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
     * Equivalent to {@code builder().stringLength(stringLength)}.
     *
     * @param stringLength the longest string
     * @return a new builder
     */
    public static ScopeBuilder stringLength(int stringLength) {
        return builder().stringLength(stringLength);
    }

    /**
     * Equivalent to {@code builder().charLow(charLow)}.
     *
     * @param charLow the lowest character
     * @return a new builder
     */
    public static ScopeBuilder charLow(char charLow) {
        return builder().charLow(charLow);
    }

    /**
     * Equivalent to {@code builder().charHigh(charHigh)}.
     *
     * @param charHigh the highest character
     * @return a new builder
     */
    public static ScopeBuilder charHigh(char charHigh) {
        return builder().charHigh(charHigh);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Scope toScope() {
        return this;
    }

    /**
     * Construct the scope using the builder pattern. Use the current settings
     * in this Scope as the defaults in the builder.
     *
     * @return a new builder
     */
    public ScopeBuilder toBuilder() {
        return new ScopeBuilder(scopes, defaultScope, intLow, intHigh,
                mulLow, mulHigh, stringLength, charLow, charHigh);
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
