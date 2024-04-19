package org.plantuml.ast;

import java.io.IOException;

public class PlantumlProperty implements PlantumlExpr {
    private final String prop;

    public PlantumlProperty(String prop) {
        // NOTE: do you want to make this configurable though the TOML file? That might be a stretch...
        prop = prop.replace("c0_", ""); // Remove "c0_" prefix (instances)
        prop = prop.replace("this . ", ""); // Remove "this ." prefix (self attributes)
        prop = prop.replace(" . ref", ""); // Remove "ref ." prefix (type defs)
        prop = prop.replace("  ", " "); // Only single space
        prop = prop.replace(" . ", "."); // Remove spaces between names
        prop = prop.replace("[", ""); // Remove square brackets
        prop = prop.replace("]", ""); // Remove square brackets
        prop = prop.replace("parent.", ""); // Remove `parent.` from the fully qualified name
        this.prop = prop;
    }

    public String getProp(){
        return prop;
    }

    @Override
    public <A, B> B accept(PlantumlExprVisitor<A, B> visitor, A a) throws IOException {
        return visitor.visit(this, a);
    }
}
