package org.clafer.frontend;

/**
 * The kinds of tokens.
 */
public enum TokenKind {
    /**
     * A name or a keyword, e.g., {@code c0_cost}, {@code abstract},
     * {@code int}: keywords are recognized by the parser where they are
     * expected, so a clafer may be named like one.
     */
    WORD,
    /**
     * A natural number, e.g., {@code 16}. A sign is a separate {@code -}.
     */
    INT,
    /**
     * A decimal number with digits on both sides of the point, e.g.,
     * {@code 19.99}.
     */
    DECIMAL,
    /**
     * A double, a decimal number with an exponent, e.g., {@code 1.2e2} or
     * {@code 1.0e-2}, as the compiler prints a {@code double} literal.
     */
    DOUBLE,
    /**
     * A string literal with its quotes and escapes as written, e.g.,
     * {@code "a\"b"}; see {@link Lexer#unescape}.
     */
    STRING,
    /**
     * An operator or punctuation, e.g., {@code ->>}, {@code ..} or a brace.
     */
    SYMBOL,
    /**
     * A line or block comment. Comments are set aside, not passed to the
     * parser.
     */
    COMMENT,
    /**
     * Text that is not a token, e.g., an unexpected character or an
     * unterminated string. The lexer reports a diagnostic for it and goes on.
     */
    ERROR,
    /**
     * A reserved word of the full language, e.g., {@code abstract},
     * {@code some} or {@code X} ({@link ClaferLexer}): it cannot name a
     * clafer. The explicit languages have none ({@link #WORD}).
     */
    KEYWORD,
    /**
     * An escape to a backend, {@code [alloy| ... |]} or
     * {@code [choco| ... |]}, set aside like a comment ({@link ClaferLexer}).
     */
    ESCAPE,
    /**
     * The start of a block of indented lines, an empty span: the opening
     * brace that {@link Layout} inserts.
     */
    INDENT,
    /**
     * The end of a block of indented lines, an empty span: the closing
     * brace that {@link Layout} inserts.
     */
    DEDENT,
    /**
     * The separator that {@link Layout} inserts between two lines at the
     * same indentation, an empty span, so that, e.g., a group cardinality
     * at the start of a line is not the cardinality of the clafer on the line
     * before.
     */
    SEPARATOR,
    /**
     * The end of the input, an empty span after the last character.
     */
    EOF
}
