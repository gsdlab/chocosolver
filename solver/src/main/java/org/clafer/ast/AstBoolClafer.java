package org.clafer.ast;

/**
 * The primitive boolean Clafer.
 *
 * @author jimmy
 */
public class AstBoolClafer extends AstPrimClafer {

    public static final AstBoolClafer Singleton = new AstBoolClafer();

    private AstBoolClafer() {
        super("bool");
    }
}
