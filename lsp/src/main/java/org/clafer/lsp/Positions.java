package org.clafer.lsp;

import java.util.ArrayList;
import java.util.List;
import org.clafer.frontend.SourceSpan;
import org.eclipse.lsp4j.Position;
import org.eclipse.lsp4j.Range;

/**
 * Converts between the frontend's positions and LSP's, on the text of a
 * document.
 * <p>
 * The frontend's lines and columns start at 1, and its columns are the
 * compiler's: a tab moves to the column after the next multiple of 8, and
 * every other character, a carriage return included, is one column. LSP's
 * lines and characters start at 0, and a character is a UTF-16 code unit, as
 * a Java {@code char} is.
 */
public final class Positions {

    private final List<String> lines;

    /**
     * @param text the text of the document
     */
    public Positions(String text) {
        List<String> ls = new ArrayList<>();
        int start = 0;
        for (int i = 0; i < text.length(); i++) {
            if (text.charAt(i) == '\n') {
                ls.add(text.substring(start, i));
                start = i + 1;
            }
        }
        ls.add(text.substring(start));
        this.lines = List.copyOf(ls);
    }

    /**
     * @param line a line, from 0
     * @return its text without the newline, or empty if there is no such line
     */
    public String line(int line) {
        return line >= 0 && line < lines.size() ? lines.get(line) : "";
    }

    /**
     * @return the number of lines, the last one after the last newline
     * included
     */
    public int lineCount() {
        return lines.size();
    }

    /**
     * @param line a line of the frontend, from 1
     * @param column a column of the frontend, from 1
     * @return the LSP position: past the end of its line if the column is,
     * and at the end of the document if the line is past it
     */
    public Position toLsp(int line, int column) {
        if (line > lines.size()) {
            int last = lines.size() - 1;
            return new Position(last, lines.get(last).length());
        }
        String text = lines.get(Math.max(line, 1) - 1);
        int col = 1;
        for (int i = 0; i < text.length(); i++) {
            int after = next(col, text.charAt(i));
            // a column inside a tab is the tab
            if (col >= column || after > column) {
                return new Position(line - 1, i);
            }
            col = after;
        }
        return new Position(Math.max(line, 1) - 1, text.length());
    }

    /**
     * @param position an LSP position
     * @return the frontend's line and column there, as {line, column}
     */
    public int[] fromLsp(Position position) {
        String text = line(position.getLine());
        int col = 1;
        for (int i = 0; i < Math.min(position.getCharacter(), text.length()); i++) {
            col = next(col, text.charAt(i));
        }
        return new int[]{position.getLine() + 1, col};
    }

    /**
     * The column after a character, as the compiler counts it.
     */
    private static int next(int column, char c) {
        return c == '\t' ? ((column + 7) / 8) * 8 + 1 : column + 1;
    }

    /**
     * @param span a span of the frontend
     * @return its LSP range
     */
    public Range toLsp(SourceSpan span) {
        return new Range(toLsp(span.line(), span.column()), toLsp(span.endLine(), span.endColumn()));
    }

    /**
     * @param span a span of the frontend
     * @return its LSP range, widened to the character at its start if it is
     * empty and there is one, so that every editor shows it
     */
    public Range toVisibleLsp(SourceSpan span) {
        Range r = toLsp(span);
        if (r.getStart().equals(r.getEnd())) {
            Position s = r.getStart();
            if (s.getCharacter() < line(s.getLine()).length()) {
                return new Range(s, new Position(s.getLine(), s.getCharacter() + 1));
            }
        }
        return r;
    }

    /**
     * @param range a range in this document
     * @return its text
     */
    public String text(Range range) {
        Position s = range.getStart(), e = range.getEnd();
        if (s.getLine() == e.getLine()) {
            String l = line(s.getLine());
            return l.substring(Math.min(s.getCharacter(), l.length()), Math.min(e.getCharacter(), l.length()));
        }
        StringBuilder sb = new StringBuilder();
        String first = line(s.getLine());
        sb.append(first.substring(Math.min(s.getCharacter(), first.length())));
        for (int i = s.getLine() + 1; i < e.getLine(); i++) {
            sb.append('\n').append(line(i));
        }
        String last = line(e.getLine());
        sb.append('\n').append(last, 0, Math.min(e.getCharacter(), last.length()));
        return sb.toString();
    }
}
