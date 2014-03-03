package org.clafer.ir.analysis;

/**
 *
 * @author jimmy
 */
public class CoalesceException extends RuntimeException {

    private static final long serialVersionUID = 1L;

    public CoalesceException() {
    }

    public CoalesceException(String message) {
        super(message);
    }

    public CoalesceException(String message, Throwable cause) {
        super(message, cause);
    }

    public CoalesceException(Throwable cause) {
        super(cause);
    }
}
