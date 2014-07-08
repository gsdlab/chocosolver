package org.clafer.ir;

/**
 *
 * @author jimmy
 */
public class IllegalStringException extends RuntimeException {

    private static final long serialVersionUID = 1L;

    public IllegalStringException() {
    }

    public IllegalStringException(String message) {
        super(message);
    }

    public IllegalStringException(String message, Throwable cause) {
        super(message, cause);
    }

    public IllegalStringException(Throwable cause) {
        super(cause);
    }
}
