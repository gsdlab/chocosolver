package org.sysml.ast;

import java.io.IOException;

public interface SysmlExprVisitor <A, B>{
    B visit(SysmlPackage ast, A a) throws IOException;

    B visit(SysmlProperty ast, A a) throws IOException;

    B visit(SysmlAttribute ast, A a) throws IOException;

    B visit(SysmlPropertyDef sysmlPropertyDef, A a) throws IOException;
}
