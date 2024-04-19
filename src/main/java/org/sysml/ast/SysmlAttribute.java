package org.sysml.ast;

import java.io.IOException;

public class SysmlAttribute implements SysmlExpr, SysmlId {
    private String name;

    private Object ref;

    public SysmlAttribute(String name, Object ref) {
        this.name = name;
        this.ref = ref;
    }

    @Override
    public String getName() {
        return name;
    }

    public String getRef() {
        return ref.toString();
    }
    @Override
    public <A, B> B accept(SysmlExprVisitor<A, B> visitor, A a) throws IOException {
        return visitor.visit(this, a);
    }
}
