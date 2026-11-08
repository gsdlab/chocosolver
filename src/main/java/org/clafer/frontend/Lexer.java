package org.clafer.frontend;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import org.clafer.ast.SourceSpan;

/**
 * Splits the explicit form of a Clafer model, as {@link org.clafer.ast.AstPrinter}
 * prints it, into tokens.
 * <p>
 * Whitespace and line breaks only separate tokens: nesting is in braces, and
 * indentation carries no meaning. Comments ({@code // ...} to the end of the
 * line and {@code /* ... *}{@code /}) are set aside with their spans, for
 * tools that show or keep them.
 * <p>
 * Lines and columns start at 1. A column counts UTF-16 code units, as the
 * Language Server Protocol does by default, so a tab is one column. A span
 * ends after its last character.
 * <p>
 * The lexer does not stop at an error: an unexpected character or an
 * unterminated string becomes an {@link TokenKind#ERROR} token with a
 * diagnostic, and lexing goes on after it.
 */
public final class Lexer {

    /**
     * The operators and punctuation, longest first so that the longest match
     * wins, e.g., {@code ->>} over {@code ->}.
     */
    private static final String[] Symbols = {
        "<=>", "->>",
        "<<", ">>", "->", "..", "::", "<=", ">=", "!=", "&&", "||", "=>",
        "++", "--", "**", "<:", ":>", "*.", "/.",
        ".", ":", "[", "]", "(", ")", "{", "}", ",", ";", "|",
        "<", ">", "=", "!", "#", "+", "-", "*", "/", "%", "~", "^"
    };

    /**
     * The tokens of a source text.
     *
     * @param tokens the tokens for the parser, ending with
     * {@link TokenKind#EOF}
     * @param comments the comments, in order
     * @param diagnostics the errors, in order
     */
    public record Result(List<Token> tokens, List<Token> comments, List<Diagnostic> diagnostics) {
    }

    private final String source;
    private int pos = 0;
    private int line = 1;
    private int column = 1;
    private final List<Token> tokens = new ArrayList<>();
    private final List<Token> comments = new ArrayList<>();
    private final List<Diagnostic> diagnostics = new ArrayList<>();

    private Lexer(String source) {
        this.source = source;
    }

    /**
     * Splits a source text into tokens.
     *
     * @param source the source text
     * @return the tokens, the comments and the errors
     */
    public static Result lex(String source) {
        Lexer lexer = new Lexer(source);
        lexer.run();
        return new Result(
                Collections.unmodifiableList(lexer.tokens),
                Collections.unmodifiableList(lexer.comments),
                Collections.unmodifiableList(lexer.diagnostics));
    }

    private void run() {
        while (pos < source.length()) {
            char c = source.charAt(pos);
            if (c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\f') {
                advance();
            } else if (source.startsWith("//", pos)) {
                lineComment();
            } else if (source.startsWith("/*", pos)) {
                blockComment();
            } else if (isWordStart(c)) {
                word();
            } else if (isDigit(c)) {
                number();
            } else if (c == '"') {
                string();
            } else {
                symbol();
            }
        }
        tokens.add(new Token(TokenKind.EOF, "", new SourceSpan(line, column, line, column)));
    }

    private static boolean isWordStart(char c) {
        return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
    }

    private static boolean isWordPart(char c) {
        return isWordStart(c) || isDigit(c) || c == '\'';
    }

    private static boolean isDigit(char c) {
        return c >= '0' && c <= '9';
    }

    private char peek(int offset) {
        int i = pos + offset;
        return i < source.length() ? source.charAt(i) : '\0';
    }

    /**
     * Moves past one character, counting a line break (LF, CRLF or CR) once.
     */
    private void advance() {
        char c = source.charAt(pos++);
        if (c == '\n' || (c == '\r' && peek(0) != '\n')) {
            line++;
            column = 1;
        } else if (c != '\r') {
            column++;
        }
    }

    private Token emit(List<Token> into, TokenKind kind, int startPos, int startLine, int startColumn) {
        Token token = new Token(kind, source.substring(startPos, pos),
                new SourceSpan(startLine, startColumn, line, column));
        into.add(token);
        return token;
    }

    private void lineComment() {
        int startPos = pos, startLine = line, startColumn = column;
        while (pos < source.length() && source.charAt(pos) != '\n' && source.charAt(pos) != '\r') {
            advance();
        }
        emit(comments, TokenKind.COMMENT, startPos, startLine, startColumn);
    }

    private void blockComment() {
        int startPos = pos, startLine = line, startColumn = column;
        advance();
        advance();
        while (pos < source.length() && !source.startsWith("*/", pos)) {
            advance();
        }
        if (pos < source.length()) {
            advance();
            advance();
            emit(comments, TokenKind.COMMENT, startPos, startLine, startColumn);
        } else {
            Token error = emit(tokens, TokenKind.ERROR, startPos, startLine, startColumn);
            diagnostics.add(new Diagnostic(error.span(), "Unterminated comment"));
        }
    }

    private void word() {
        int startPos = pos, startLine = line, startColumn = column;
        while (pos < source.length() && isWordPart(source.charAt(pos))) {
            advance();
        }
        emit(tokens, TokenKind.WORD, startPos, startLine, startColumn);
    }

    /**
     * An integer, or a decimal if a point and a digit follow the digits, so
     * that {@code 1..2} is a range of integers.
     */
    private void number() {
        int startPos = pos, startLine = line, startColumn = column;
        while (pos < source.length() && isDigit(source.charAt(pos))) {
            advance();
        }
        TokenKind kind = TokenKind.INT;
        if (peek(0) == '.' && isDigit(peek(1))) {
            kind = TokenKind.DECIMAL;
            advance();
            while (pos < source.length() && isDigit(source.charAt(pos))) {
                advance();
            }
        }
        emit(tokens, kind, startPos, startLine, startColumn);
    }

    /**
     * A string on one line. The escapes are checked here, and
     * {@link #unescape} decodes them.
     */
    private void string() {
        int startPos = pos, startLine = line, startColumn = column;
        advance();
        while (pos < source.length()) {
            char c = source.charAt(pos);
            if (c == '"') {
                advance();
                emit(tokens, TokenKind.STRING, startPos, startLine, startColumn);
                return;
            }
            if (c == '\n' || c == '\r') {
                break;
            }
            if (c == '\\') {
                int escapeLine = line, escapeColumn = column;
                advance();
                int length = escapeLength(peek(0), pos);
                if (length == 0) {
                    // Report the backslash and the character after it, unless
                    // that ends the string or the line, and go on.
                    char next = peek(0);
                    if (pos < source.length() && next != '"' && next != '\n' && next != '\r') {
                        advance();
                    }
                    diagnostics.add(new Diagnostic(new SourceSpan(escapeLine, escapeColumn, line, column),
                            "Unknown escape in a string; use \\\", \\\\, \\n, \\t or \\uXXXX"));
                }
                for (int i = 0; i < length; i++) {
                    advance();
                }
            } else {
                advance();
            }
        }
        Token error = emit(tokens, TokenKind.ERROR, startPos, startLine, startColumn);
        diagnostics.add(new Diagnostic(error.span(), "Unterminated string"));
    }

    /**
     * @return how many characters after the backslash the escape takes, or 0
     * if it is not one
     */
    private int escapeLength(char c, int at) {
        switch (c) {
            case '"', '\\', 'n', 't' -> {
                return 1;
            }
            case 'u' -> {
                for (int i = 1; i <= 4; i++) {
                    int j = at + i;
                    if (j >= source.length() || Character.digit(source.charAt(j), 16) < 0) {
                        return 0;
                    }
                }
                return 5;
            }
            default -> {
                return 0;
            }
        }
    }

    private void symbol() {
        int startPos = pos, startLine = line, startColumn = column;
        for (String symbol : Symbols) {
            if (source.startsWith(symbol, pos)) {
                for (int i = 0; i < symbol.length(); i++) {
                    advance();
                }
                emit(tokens, TokenKind.SYMBOL, startPos, startLine, startColumn);
                return;
            }
        }
        advance();
        Token error = emit(tokens, TokenKind.ERROR, startPos, startLine, startColumn);
        diagnostics.add(new Diagnostic(error.span(), "Unexpected character " + describe(error.text().charAt(0))));
    }

    private static String describe(char c) {
        return c >= ' ' && c <= '~' ? "'" + c + "'" : String.format("U+%04X", (int) c);
    }

    /**
     * Decodes a string token: drops the quotes and replaces the escapes.
     *
     * @param token the text of a {@link TokenKind#STRING} token
     * @return the string it denotes
     */
    public static String unescape(String token) {
        StringBuilder out = new StringBuilder();
        for (int i = 1; i < token.length() - 1; i++) {
            char c = token.charAt(i);
            if (c != '\\') {
                out.append(c);
                continue;
            }
            char e = token.charAt(++i);
            switch (e) {
                case 'n' -> out.append('\n');
                case 't' -> out.append('\t');
                case 'u' -> {
                    out.append((char) Integer.parseInt(token.substring(i + 1, i + 5), 16));
                    i += 4;
                }
                default -> out.append(e);
            }
        }
        return out.toString();
    }
}
