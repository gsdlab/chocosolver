package org.clafer.common;

/**
 * The current system of constraints is unsatisfiable.
 *
 * @author jimmy
 */
public class UnsatisfiableException extends RuntimeException {

    private static final long serialVersionUID = 1L;

    public UnsatisfiableException() {
    }

    public UnsatisfiableException(String message) {
        super(message);
    }

    public UnsatisfiableException(String message, Throwable cause) {
        super(message, cause);
    }

    public UnsatisfiableException(Throwable cause) {
        super(cause);
    }
}
