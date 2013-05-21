package org.clafer.ast;

/**
 *
 * @author jimmy
 */
public class AstIntClafer extends AstPrimClafer {

    public static final AstIntClafer Singleton = new AstIntClafer();

    private AstIntClafer() {
        super("int");
    }
}
