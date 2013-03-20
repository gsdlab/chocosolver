package org.clafer.ast;

/**
 *
 * @author jimmy
 */
public class AstIntClafer extends AstClafer {

    public static final AstIntClafer Singleton = new AstIntClafer();
    
    private AstIntClafer() {
        super("int");
    }

    @Override
    public AstClafer extending(AstAbstractClafer superClafer) {
        throw new UnsupportedOperationException("Cannot extend from int primitive");
    }

    @Override
    public AstConcreteClafer addChild(String name) {
        throw new UnsupportedOperationException("Cannot add a child under int primitive");
    }

    @Override
    public String toString() {
        return "int";
    }
}
