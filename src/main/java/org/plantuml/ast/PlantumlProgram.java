package org.plantuml.ast;

import org.sysml.ast.SysmlExpr;
import org.sysml.ast.SysmlExprVisitor;

import java.io.IOException;

/**
 * Main PlantUML Program Element
 *
 * This is likely quite wrong. For our use case, we consider a PlantUML program as a collection
 * of objects and connections.
 */
public class PlantumlProgram implements PlantumlExpr {
    private PlantumlObject[] objects;
    private PlantumlConnection[] connections;

    public PlantumlProgram() {
        this.objects = new PlantumlObject[0];
        this.connections = new PlantumlConnection[0];
    }

    public PlantumlProgram(PlantumlObject[] objects, PlantumlConnection[] connections) {
        this.objects = objects;
        this.connections = connections;

    }

    public PlantumlObject[] getObjects(){
        return objects;
    }

    public PlantumlConnection[] getConnections(){
        return connections;
    }

    @Override
    public <A, B> B accept(PlantumlExprVisitor<A, B> visitor, A a) throws IOException {
        return visitor.visit(this, a);
    }
}
