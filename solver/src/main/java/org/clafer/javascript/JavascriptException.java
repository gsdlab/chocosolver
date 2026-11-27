package org.clafer.javascript;

/**
 *
 * @author jimmy
 */
public class JavascriptException extends RuntimeException {

    public JavascriptException() {
    }

    public JavascriptException(String message) {
        super(message);
    }

    public JavascriptException(Throwable cause) {
        super(cause);
    }

    public JavascriptException(String message, Throwable cause) {
        super(message, cause);
    }
}
