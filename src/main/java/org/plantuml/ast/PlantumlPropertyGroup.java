package org.plantuml.ast;

import java.io.IOException;

public class PlantumlPropertyGroup implements PlantumlId, PlantumlExpr {

    private final String name;
    private PlantumlProperty[] properties;

    public PlantumlPropertyGroup(String name, PlantumlProperty[] properties) {
        this.properties = properties;
        this.name = name;
    }

    public PlantumlProperty[] getProperties() {
        return this.properties;
    }

    @Override
    public String getName() {
        return name;
    }

    @Override
    public <A, B> B accept(PlantumlExprVisitor<A, B> visitor, A a) throws IOException {
        return visitor.visit(this, a);
    }
}
