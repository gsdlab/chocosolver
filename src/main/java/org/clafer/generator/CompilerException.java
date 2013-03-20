package org.clafer.generator;

/**
 *
 * @author jimmy
 */
public class CompilerException extends RuntimeException {

    public CompilerException(Throwable cause) {
        super(cause);
    }

    public CompilerException(String message, Throwable cause) {
        super(message, cause);
    }

    public CompilerException(String message) {
        super(message);
    }

    public CompilerException() {
    }
}
