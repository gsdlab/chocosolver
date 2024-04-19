package org.plantuml.ast;

import java.io.IOException;

/**
 * AST Visitor
 *
 * We make AST visitors capable of throwing IOExecptions as it's convenient for pretty printers
 * However, we could likely get rid of this throw some type of interface conversion.
 */
public interface PlantumlExprVisitor<A, B> {
    B visit(PlantumlProgram plantumlProgram, A a) throws IOException;

    B visit(PlantumlObject plantumlObject, A a) throws IOException;

    B visit(PlantumlPropertyGroup plantumlPropertyGroup, A a) throws IOException;

    B visit(PlantumlProperty plantumlProperty, A a) throws IOException;

    B visit(PlantumlConnection plantumlConnection, A a) throws IOException;
}
