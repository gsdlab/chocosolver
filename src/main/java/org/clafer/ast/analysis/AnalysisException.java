package org.clafer.ast.analysis;

/**
 *
 * @author jimmy
 */
public class AnalysisException extends RuntimeException {

    public AnalysisException(Throwable cause) {
        super(cause);
    }

    public AnalysisException(String message, Throwable cause) {
        super(message, cause);
    }

    public AnalysisException(String message) {
        super(message);
    }

    public AnalysisException() {
    }
}
