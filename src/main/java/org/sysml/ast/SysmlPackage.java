package org.sysml.ast;



import java.io.IOException;

/**
 * TODO: build out the DiagramElement taxonomy better
 */
public class SysmlPackage implements SysmlBlockDefElement {
    private final SysmlBlockDefElement[] elements;
    private final String name;

    public SysmlPackage(String name, SysmlBlockDefElement[] elements){
        this.elements = elements;
        this.name = name;
    }

    public String getName() {
        return name;
    }

    public SysmlBlockDefElement[] getElements(){
        return elements;
    }

    @Override
    public <A, B> B accept(SysmlExprVisitor<A, B> visitor, A a) throws IOException {
        return visitor.visit(this, a);
    }
}
