package org.plantuml.ast;

import java.io.IOException;

public class PlantumlConnection implements PlantumlExpr  {
    private final String fromObj;
    private final String toObj;
    private final char fromConn;
    private final char toConn;

    private final char lineChar;

    private final String label;

    public PlantumlConnection(String fromObj, String toObj, char fromConn, char toConn, String label){
        this.fromObj = fromObj;
        this.toObj = toObj;
        this.fromConn = fromConn;
        this.toConn = toConn;
        this.label = label;
        this.lineChar = '-';
    }

    public PlantumlConnection(String fromObj, String toObj, char fromConn, char toConn, String label, char lineChar){
        this.fromObj = fromObj;
        this.toObj = toObj;
        this.fromConn = fromConn;
        this.toConn = toConn;
        this.label = label;
        this.lineChar = lineChar;
    }

    public String getFromObj(){
        return fromObj;
    }

    public String getToObj(){
        return toObj;
    }

    public char getToConn(){
        return toConn;
    }

    public char getFromConn(){
        return fromConn;
    }

    public String getLabel(){
        return label;
    }

    public char getLineChar(){
        return lineChar;
    }

    @Override
    public <A, B> B accept(PlantumlExprVisitor<A, B> visitor, A a) throws IOException {
        return visitor.visit(this, a);
    }
}
