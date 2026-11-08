package org.clafer.frontend;

import org.clafer.ast.SourceSpan;

/**
 * A token: its kind, its text exactly as written, and where it is.
 *
 * @param kind the kind
 * @param text the source text of the token
 * @param span where the token is
 */
public record Token(TokenKind kind, String text, SourceSpan span) {

    /**
     * @param symbol an operator or punctuation
     * @return whether this token is that symbol
     */
    public boolean isSymbol(String symbol) {
        return kind == TokenKind.SYMBOL && text.equals(symbol);
    }

    /**
     * @param word a keyword or name
     * @return whether this token is that word
     */
    public boolean isWord(String word) {
        return kind == TokenKind.WORD && text.equals(word);
    }

    @Override
    public String toString() {
        return kind + " " + text + " at " + span;
    }
}
