package org.clafer.ast;

/**
 *
 * @author jimmy
 */
public class AstStringClafer extends AstPrimClafer {

    public static final AstStringClafer Singleton = new AstStringClafer();

    private AstStringClafer() {
        super("string");
    }
}
