package org.sysml.pprinter;

import org.sysml.ast.*;

import java.io.IOException;
import java.lang.Void;

public class SysmlPrinter implements SysmlExprVisitor<String, Void> {

    private final String indent_base;
    private final Appendable out;

    public SysmlPrinter(Appendable out) {
        this.out = out;
        this.indent_base = "  ";
    }


    public void print(String indent, SysmlPackage spackage, Appendable out) throws IOException {
        out.append(indent).append("package ").append(spackage.getName()).append(" {").append("\n");
        for (SysmlBlockDefElement elem : spackage.getElements()) {
            print(indent + indent_base, elem, out);
            out.append('\n');
        }
        out.append(indent).append("}");
    }

    public void print(String indent, SysmlBlockDefElement sprop, Appendable out) throws IOException {
    }

    public Void visit(SysmlAttribute ast, String indent) throws IOException {
        this.out.append(indent).append(":>> ").append(ast.getName()).append(" = ").append(ast.getRef()).append(";\n");
        return null;
    }

    @Override
    public Void visit(SysmlProperty ast, String indent) throws IOException {
        this.out
                .append(indent)
                .append(ast.getPropertyType().getName())
                .append(" ")
                .append(ast.getName())
        ;
        if (ast.getMultiplicity() > 1) {
            this.out.append("[").append(String.valueOf(ast.getMultiplicity())).append("]");
        }
        for (String s: ast.getSupers()){
            this.out.append(" : ").append(s);
        }
        if (ast.getElements().length > 0 || ast.getAnnotations().length > 0) {
            this.out.append(" {\n");
            for (SysmlAttribute annot: ast.getAnnotations()){
                annot.accept(this, indent + indent_base);
            }
            for (SysmlBlockDefElement elem : ast.getElements()) {
                elem.accept(this, indent + indent_base);
            }
            this.out.append(indent).append("}\n");
        } else {
            this.out.append(";\n");
        }

        return null;
    }

    @Override
    public Void visit(SysmlPropertyDef ast, String indent) throws IOException {
        this.out
                .append(indent)
                .append(ast.getPropertyType().getName())
                .append(" def ")
                .append(ast.getName())
        ;
        for (String s: ast.getSupers()){
            this.out.append(" :> ").append(s);
        }
        if (ast.getElements().length > 0 || ast.getAnnotations().length > 0) {
            this.out.append(" {\n");
            for (SysmlAttribute annot: ast.getAnnotations()){
                this.out.append(indent + indent_base).append("attribute ").append(annot.getName()).append(": ").append(annot.getRef()).append(";\n");
                ///annot.accept(this, indent + indent_base);
            }
            for (SysmlBlockDefElement elem : ast.getElements()) {
                elem.accept(this, indent + indent_base);
            }
            this.out.append(indent).append("}\n");
        } else {
            this.out.append(";\n");
        }
        return null;
    }

    @Override
    public Void visit(SysmlPackage ast, String indent) throws IOException {
        this.out.append(indent).append("package ").append(ast.getName()).append(" {").append("\n");
        for (SysmlBlockDefElement elem : ast.getElements()) {
            elem.accept(this, indent + indent_base);
        }
        this.out.append(indent).append("}\n");
        return null;
    }
}
