package org.clafer.frontend;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Deque;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Turns indentation into blocks, as the compiler's layout resolver does
 * ({@code LayoutResolver.resolveLayout}): a line indented more than the line
 * before opens a block ({@link TokenKind#INDENT}), a line indented less
 * closes the blocks it leaves ({@link TokenKind#DEDENT}), and a line at the
 * indentation of a sibling is separated from the line before
 * ({@link TokenKind#SEPARATOR}).
 * <p>
 * The rules are the compiler's, quirks included, so that the parser sees the
 * tokens that the compiler's parser sees:
 * <ul>
 * <li>layout is off on a line that starts inside {@code [ ]} or
 * {@code { }};</li>
 * <li>a line that starts with {@code {} or {@code }} has no layout;</li>
 * <li>a line that starts with a comment or an escape is indented at the
 * column of the first of them, even one that starts on an earlier line;</li>
 * <li>a model whose brackets are open by more than one at its end has
 * {@code ']' bracket missing}.</li>
 * </ul>
 * An inserted token has an empty span where the compiler puts it: after the
 * token before it, past one space.
 */
public final class Layout {

    /**
     * The tokens with the layout made explicit.
     *
     * @param tokens the tokens for the parser, ending with
     * {@link TokenKind#EOF}
     * @param diagnostics the errors
     */
    public record Result(List<Token> tokens, List<Diagnostic> diagnostics) {
    }

    private Layout() {
    }

    /**
     * Where a line starts: the column that gives its indentation, and how
     * deep in brackets it starts.
     */
    private record NewLine(int column, int depth) {
    }

    /**
     * Resolves the layout of the tokens of a model.
     *
     * @param lexed the tokens, comments and escapes of {@link ClaferLexer}
     * @return the tokens with the blocks and separators of the layout
     */
    public static Result resolve(ClaferLexer.Result lexed) {
        List<Token> tokens = lexed.tokens().subList(0, lexed.tokens().size() - 1);
        Token eof = lexed.tokens().get(lexed.tokens().size() - 1);
        List<Diagnostic> diagnostics = new ArrayList<>();
        List<Object> marked = addNewLines(tokens, lexed.others(), diagnostics);
        List<Token> out = new ArrayList<>();
        Deque<Integer> levels = new ArrayDeque<>();
        levels.push(1);
        NewLine lastNewLine = null;
        for (Object item : marked) {
            if (item instanceof NewLine newLine) {
                lastNewLine = newLine;
                continue;
            }
            Token t = (Token) item;
            if (lastNewLine != null && lastNewLine.depth() <= 0) {
                int level = lastNewLine.column();
                if (level > levels.peek()) {
                    levels.push(level);
                    out.add(inserted(TokenKind.INDENT, "{", out));
                } else if (level == levels.peek()) {
                    out.add(inserted(TokenKind.SEPARATOR, "", out));
                } else {
                    while (level < levels.peek()) {
                        levels.pop();
                        out.add(inserted(TokenKind.DEDENT, "}", out));
                    }
                    out.add(inserted(TokenKind.SEPARATOR, "", out));
                }
                lastNewLine = null;
            }
            out.add(t);
        }
        while (levels.size() > 1) {
            levels.pop();
            out.add(inserted(TokenKind.DEDENT, "}", out));
        }
        out.add(eof);
        return new Result(Collections.unmodifiableList(out), Collections.unmodifiableList(diagnostics));
    }

    /**
     * A token that the layout inserts, after the token before it past one
     * space ({@code LayoutResolver.nextPos}).
     */
    private static Token inserted(TokenKind kind, String text, List<Token> out) {
        if (out.isEmpty()) {
            return new Token(kind, text, SourceSpan.at(1, 1));
        }
        Token before = out.get(out.size() - 1);
        int column = before.span().column() + before.text().length() + 1;
        return new Token(kind, text, SourceSpan.at(before.span().line(), column));
    }

    /**
     * The tokens, with a {@link NewLine} before each token that starts a
     * line, except a line that starts with {@code {} or {@code }}
     * ({@code LayoutResolver.addNewLines}).
     */
    private static List<Object> addNewLines(List<Token> tokens, List<Token> others, List<Diagnostic> diagnostics) {
        List<Object> out = new ArrayList<>();
        if (tokens.isEmpty()) {
            return out;
        }
        Map<Integer, int[]> firstOther = firstOtherOnLine(others);
        int depth = tokens.get(0).isSymbol("[") ? 1 : 0;
        for (int i = 0; i < tokens.size(); i++) {
            Token t0 = tokens.get(i);
            out.add(t0);
            if (i + 1 == tokens.size()) {
                if (depth != 0 && depth != 1) {
                    diagnostics.add(new Diagnostic(SourceSpan.at(t0.span().line(), t0.span().column()),
                            "']' bracket missing"));
                }
                break;
            }
            Token t1 = tokens.get(i + 1);
            boolean newLine = t0.span().line() < t1.span().line();
            if (newLine && t1.isSymbol("[")) {
                out.add(new NewLine(layoutColumn(firstOther, t1), depth));
                depth++;
            } else if (newLine && t1.isSymbol("]")) {
                out.add(new NewLine(layoutColumn(firstOther, t1), depth));
                depth--;
            } else if (t1.isSymbol("{") || t1.isSymbol("[")) {
                depth++;
            } else if (t1.isSymbol("}") || t1.isSymbol("]")) {
                depth--;
            } else if (newLine) {
                out.add(new NewLine(layoutColumn(firstOther, t1), depth));
            }
        }
        return out;
    }

    /**
     * For each line, the start of the first comment or escape that ends on
     * it.
     */
    private static Map<Integer, int[]> firstOtherOnLine(List<Token> others) {
        Map<Integer, int[]> first = new HashMap<>();
        for (Token o : others) {
            int endLine = o.span().line() + (int) o.text().chars().filter(c -> c == '\n').count();
            int[] start = {o.span().line(), o.span().column()};
            first.merge(endLine, start, (a, b) -> compare(a, b) <= 0 ? a : b);
        }
        return first;
    }

    private static int compare(int[] a, int[] b) {
        return a[0] != b[0] ? Integer.compare(a[0], b[0]) : Integer.compare(a[1], b[1]);
    }

    /**
     * The column that gives the indentation of a line that starts with a
     * token: that of the first comment or escape that ends on the line before
     * the token, if any ({@code LayoutResolver.layoutColumn}).
     */
    private static int layoutColumn(Map<Integer, int[]> firstOther, Token t) {
        int[] start = firstOther.get(t.span().line());
        if (start != null && compare(start, new int[]{t.span().line(), t.span().column()}) < 0) {
            return start[1];
        }
        return t.span().column();
    }
}
