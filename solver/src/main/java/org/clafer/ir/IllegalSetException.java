package org.clafer.ir;

/**
 *
 * @author jimmy
 */
public class IllegalSetException extends RuntimeException {

    private static final long serialVersionUID = 1L;

    public IllegalSetException() {
    }

    public IllegalSetException(String message) {
        super(message);
    }

    public IllegalSetException(String message, Throwable cause) {
        super(message, cause);
    }

    public IllegalSetException(Throwable cause) {
        super(cause);
    }
}
