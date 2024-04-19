package org.plantuml.ast;

import java.io.IOException;

public class PlantumlObject implements PlantumlExpr, PlantumlId {
    private final String name;
    private final String alias;
    private final String parent;

    private final PlantumlPropertyGroup[] propertyGroups;

    public PlantumlObject(String name, String alias, String parent, PlantumlPropertyGroup[] propertyGroups) {
        this.name = name;
        this.alias = alias;
        this.parent = parent;
        this.propertyGroups = propertyGroups;
    }

    public PlantumlPropertyGroup[] getPropertyGroups() {
        return propertyGroups;
    }

    @Override
    public String getName() {
        return name;
    }

    public String getAlias() {
        return alias;
    }

    public String getParent() {
        return parent;
    }

    @Override
    public <A, B> B accept(PlantumlExprVisitor<A, B> visitor, A a) throws IOException {
        return visitor.visit(this, a);
    }
}
