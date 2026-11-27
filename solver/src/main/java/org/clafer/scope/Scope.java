package org.clafer.scope;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;
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
 * is more convenient.
 * </p>
 * <p>
 * Example 1:
 * <pre>
 * Map&lt;AstClafer, Integer&gt; map = new HashMap&lt;&gt();
 * map.put(claferA, 3);
 * map.put(claferB, 2);
 * Scope scope = new Scope(map, 3, -16, 16, null, null, 10, (char) 0x20, (char) 0x7e);
 * </pre>
 * </p>
 * <p>
 * Example 2:
 * <pre>
 * Scope scope = Scope.setScope(claferA, 3).setScope(claferB, 2).defaultScope(3).intLow(-16).intHigh(16).toScope();
 * </pre>
 * </p>
 * <p>
 * Both examples construct the same scope.
 * </p>
 *
 * <p>
 * What is scope? Because of the expressiveness of Clafer, reasoning is too
 * difficult in general. The scope is a limitation of the solver so that
 * reasoning is feasible.
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
 * A 2..*
 * B 2..*
 * C 2..*
 * [#A * #A * #A + #B * #B * #B = #C * #C * #C]
 * </pre>
 * </p>
 * Both examples are attempting to prove/disprove Fermat's last theorem for k=3,
 * although this easily generalizes for any k. Unfortunately this is too
 * difficult, so the solver only attempts to prove/disprove upto a certain
 * scope. In example 1, a scope of {@code Scope.intLow(2).intHigh(16).toScope()}
 * would only attempt the proof for integers between 2 and 16. In example 2, a
 * scope of {@code Scope.defaultScope(16).toScope()} would also only attempt the
 * proof for integers between 2 and 16 (the example uses encodes integers using
 * cardinality).
 *
 * @author jimmy
 * @see ScopeBuilder
 */
public class Scope implements Scopable {

    /**
     * The largest decimal scale (number of fractional digits).
     */
    public static final int MaxDecimalScale = 9;
    /**
     * The largest magnitude of a scaled decimal: one below Choco's bound for
     * integer variables ({@code IntVar.MAX_INT_BOUND = Integer.MAX_VALUE / 100}),
     * which leaves a value for the uninitialized reference.
     */
    public static final int MaxScaledDecimal = Integer.MAX_VALUE / 100 - 1;

    private final Map<AstClafer, Integer> scopes;
    private final int defaultScope;
    private final int intLow, intHigh;
    private final Integer mulLow, mulHigh;
    private final int stringLength;
    private final char charLow, charHigh;
    private final int decimalScale;
    private final int decimalLow, decimalHigh;
    // The decimal range as given, null for the largest that fits.
    private final BigDecimal decimalLowBound, decimalHighBound;

    /**
     * Construct a new scope without decimals. Altering the map has no affect
     * once the scope is constructed.
     *
     * @param scopes a map of Clafers to their scopes
     * @param defaultScope the scope for unspecified Clafers
     * @param intLow the lowest (inclusive) integer used for solving
     * @param intHigh the highest (inclusive) integer used for solving
     * @param mulLow the lowest (inclusive) integer used for solving
     * multiplication or null to use {@code intLow}
     * @param mulHigh the highest (inclusive) integer used for solving
     * multiplication or null to use {@code intHigh}
     * @param stringLength the longest (inclusive) string used for solving
     * @param charLow the lowest (inclusive) character used for solving
     * @param charHigh the highest (inclusive) character used for solving
     */
    public Scope(Map<AstClafer, Integer> scopes, int defaultScope, int intLow, int intHigh,
            Integer mulLow, Integer mulHigh, int stringLength, char charLow, char charHigh) {
        this(scopes, defaultScope, intLow, intHigh, mulLow, mulHigh, stringLength, charLow, charHigh,
                0, null, null);
    }

    /**
     * Construct a new scope. Altering the map has no affect once the scope is
     * constructed.
     *
     * @param scopes a map of Clafers to their scopes
     * @param defaultScope the scope for unspecified Clafers
     * @param intLow the lowest (inclusive) integer used for solving
     * @param intHigh the highest (inclusive) integer used for solving
     * @param mulLow the lowest (inclusive) integer used for solving
     * multiplication or null to use {@code intLow}
     * @param mulHigh the highest (inclusive) integer used for solving
     * multiplication or null to use {@code intHigh}
     * @param stringLength the longest (inclusive) string used for solving
     * @param charLow the lowest (inclusive) character used for solving
     * @param charHigh the highest (inclusive) character used for solving
     * @param decimalScale the number of fractional digits of decimals
     * @param decimalLow the lowest (inclusive) decimal used for solving, or
     * null for the lowest that fits
     * @param decimalHigh the highest (inclusive) decimal used for solving, or
     * null for the highest that fits
     */
    public Scope(Map<AstClafer, Integer> scopes, int defaultScope, int intLow, int intHigh,
            Integer mulLow, Integer mulHigh, int stringLength, char charLow, char charHigh,
            int decimalScale, BigDecimal decimalLow, BigDecimal decimalHigh) {
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
        if ((mulLow == null ? intLow : mulLow) > (mulHigh == null ? intHigh : mulHigh)) {
            throw new IllegalArgumentException(
                    "mulLow(" + (mulLow == null ? intLow : mulLow) + ") > mulHigh(" + (mulHigh == null ? intHigh : mulHigh) + ")");
        }
        if (stringLength < 0) {
            throw new IllegalArgumentException("stringLength cannot be negative");
        }
        if (charLow > charHigh) {
            throw new IllegalArgumentException("charLow(" + charLow + ") > charHigh(" + charHigh + ")");
        }
        if (decimalScale < 0 || decimalScale > MaxDecimalScale) {
            throw new IllegalArgumentException("decimalScale(" + decimalScale + ") must be between 0 and " + MaxDecimalScale);
        }
        this.decimalScale = decimalScale;
        this.decimalLowBound = decimalLow;
        this.decimalHighBound = decimalHigh;
        this.decimalLow =decimalLow == null ? -MaxScaledDecimal : scaleDecimal(decimalLow, RoundingMode.CEILING);
        this.decimalHigh = decimalHigh == null ? MaxScaledDecimal : scaleDecimal(decimalHigh, RoundingMode.FLOOR);
        if (this.decimalLow > this.decimalHigh) {
            throw new IllegalArgumentException("decimalLow(" + decimalLow + ") > decimalHigh(" + decimalHigh + ")");
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
        return scope == null ? defaultScope : scope;
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
        return mulLow == null ? intLow : mulLow;
    }

    /**
     * Returns the highest (inclusive) integer used for solving multiplication.
     *
     * @return the highest integer
     */
    public int getMulHigh() {
        return mulHigh == null ? intHigh : mulHigh;
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
     * Returns the number of fractional digits of decimals.
     *
     * @return the decimal scale
     */
    public int getDecimalScale() {
        return decimalScale;
    }

    /**
     * Returns {@code 10^decimalScale}, the integer that represents the decimal
     * 1.
     *
     * @return the decimal one
     */
    public int getDecimalOne() {
        int one = 1;
        for (int i = 0; i < decimalScale; i++) {
            one *= 10;
        }
        return one;
    }

    /**
     * Returns the lowest (inclusive) decimal used for solving, scaled by
     * {@link #getDecimalOne()}.
     *
     * @return the lowest scaled decimal
     */
    public int getDecimalLow() {
        return decimalLow;
    }

    /**
     * Returns the highest (inclusive) decimal used for solving, scaled by
     * {@link #getDecimalOne()}.
     *
     * @return the highest scaled decimal
     */
    public int getDecimalHigh() {
        return decimalHigh;
    }

    /**
     * Scale a decimal to an integer at the decimal scale.
     *
     * @param value the decimal
     * @param rounding how to round digits beyond the decimal scale
     * @return the scaled decimal
     * @throws IllegalArgumentException if the scaled decimal is out of the
     * range Choco supports
     * @throws ArithmeticException if rounding is
     * {@link RoundingMode#UNNECESSARY} and the decimal has more fractional
     * digits than the decimal scale
     */
    public int scaleDecimal(BigDecimal value, RoundingMode rounding) {
        BigDecimal scaled = value.setScale(decimalScale, rounding);
        if (scaled.unscaledValue().abs().compareTo(BigInteger.valueOf(MaxScaledDecimal)) > 0) {
            throw new IllegalArgumentException(value.toPlainString() + " is out of the range of decimals with "
                    + decimalScale + " fractional digits, "
                    + unscaleDecimal(-MaxScaledDecimal).toPlainString() + ".."
                    + unscaleDecimal(MaxScaledDecimal).toPlainString());
        }
        return scaled.unscaledValue().intValue();
    }

    /**
     * The decimal an integer at the decimal scale represents.
     *
     * @param scaled the scaled decimal
     * @return the decimal
     */
    public BigDecimal unscaleDecimal(int scaled) {
        return BigDecimal.valueOf(scaled, decimalScale);
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
     * Equivalent to {@code builder().mulLow(mulLow)}.
     *
     * @param mulLow the lowest integer for multiplication
     * @return a new builder
     */
    public static ScopeBuilder mulLow(int mulLow) {
        return builder().mulLow(mulLow);
    }

    /**
     * Equivalent to {@code builder().mulHigh(mulHigh)}.
     *
     * @param mulHigh the highest integer for multiplication
     * @return a new builder
     */
    public static ScopeBuilder mulHigh(int mulHigh) {
        return builder().mulHigh(mulHigh);
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
     * Equivalent to {@code builder().decimalScale(decimalScale)}.
     *
     * @param decimalScale the number of fractional digits of decimals
     * @return a new builder
     */
    public static ScopeBuilder decimalScale(int decimalScale) {
        return builder().decimalScale(decimalScale);
    }

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
                mulLow, mulHigh, stringLength, charLow, charHigh,
                decimalScale, decimalLowBound, decimalHighBound);
    }

    /**
     * A scope with the same settings but other Clafer scopes.
     *
     * @param scopes a map of Clafers to their scopes
     * @return the new scope
     */
    public Scope withScopes(Map<AstClafer, Integer> scopes) {
        return new Scope(scopes, defaultScope, intLow, intHigh,
                mulLow, mulHigh, stringLength, charLow, charHigh,
                decimalScale, decimalLowBound, decimalHighBound);
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
