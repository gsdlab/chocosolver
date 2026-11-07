package org.clafer.frontend;

import org.clafer.ast.SourceSpan;

/**
 * An error in the source, with where it is.
 *
 * @param span where the error is
 * @param message what is wrong
 */
public record Diagnostic(SourceSpan span, String message) {

    @Override
    public String toString() {
        return span.format(message);
    }
}
