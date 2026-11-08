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
     * The end of the input, an empty span after the last character.
     */
    EOF
}
