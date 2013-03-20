package org.clafer.ast;

/**
 *
 * @author jimmy
 */
public class AstBoolClafer extends AstClafer {

    public static final AstBoolClafer Singleton = new AstBoolClafer();

    private AstBoolClafer() {
        super("bool");
    }

    @Override
    public AstClafer extending(AstAbstractClafer superClafer) {
        throw new UnsupportedOperationException("Cannot extend from bool primitive");
    }

    @Override
    public AstConcreteClafer addChild(String name) {
        throw new UnsupportedOperationException("Cannot add a child under bool primitive");
    }

    @Override
    public String toString() {
        return "bool";
    }
}
