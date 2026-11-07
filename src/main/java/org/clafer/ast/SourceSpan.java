package org.clafer.ast;

/**
 * Where a clafer, constraint, assertion or objective is in the source model:
 * from line:column up to, not including, endLine:endColumn. Lines and columns
 * start at 1, as in the Clafer compiler's error messages.
 *
 * @param line the first line
 * @param column the first column
 * @param endLine the last line
 * @param endColumn the column after the last character
 */
public record SourceSpan(int line, int column, int endLine, int endColumn) {

    public SourceSpan {
        if (line < 1 || column < 1 || endLine < line || (endLine == line && endColumn < column)) {
            throw new IllegalArgumentException("Not a span: " + line + ":" + column + "-" + endLine + ":" + endColumn);
        }
    }

    /**
     * @param line the line
     * @param column the column
     * @return the empty span at a position, for when only the start is known
     */
    public static SourceSpan at(int line, int column) {
        return new SourceSpan(line, column, line, column);
    }

    /**
     * Prefixes a message with the start of this span, as the compiler's
     * messages are.
     *
     * @param message the message
     * @return "line:column: message"
     */
    public String format(String message) {
        return line + ":" + column + ": " + message;
    }

    @Override
    public String toString() {
        return line + ":" + column + "-" + endLine + ":" + endColumn;
    }
}
