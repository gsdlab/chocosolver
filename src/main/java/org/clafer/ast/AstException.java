package org.clafer.ast;

/**
 *
 * @author jimmy
 */
public class AstException extends RuntimeException {

    public AstException(String message, Throwable cause) {
        super(message, cause);
    }

    public AstException(String message) {
        super(message);
    }

    public AstException(Throwable cause) {
        super(cause);
    }

    public AstException() {
    }
}
