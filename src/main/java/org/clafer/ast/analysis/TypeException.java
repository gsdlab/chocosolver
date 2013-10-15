package org.clafer.ast.analysis;

/**
 *
 * @author jimmy
 */
public class TypeException extends AnalysisException {

    public TypeException(Throwable cause) {
        super(cause);
    }

    public TypeException(String message, Throwable cause) {
        super(message, cause);
    }

    public TypeException(String message) {
        super(message);
    }

    public TypeException() {
    }
}
