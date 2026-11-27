package org.clafer.ir;

/**
 *
 * @author jimmy
 */
public class IllegalIntException extends RuntimeException {

    private static final long serialVersionUID = 1L;

    public IllegalIntException() {
    }

    public IllegalIntException(String message) {
        super(message);
    }

    public IllegalIntException(String message, Throwable cause) {
        super(message, cause);
    }

    public IllegalIntException(Throwable cause) {
        super(cause);
    }
}
