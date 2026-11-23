package org.clafer.ir;

/**
 *
 * @author jimmy
 */
public class IrException extends RuntimeException {

    public IrException(String message, Throwable cause) {
        super(message, cause);
    }

    public IrException(String message) {
        super(message);
    }

    public IrException(Throwable cause) {
        super(cause);
    }
}
