package org.clafer.ast;

import org.clafer.common.Check;
import org.clafer.common.Util;

/**
 *
 * @author jimmy
 */
public class AstDecl {

    private final boolean disjoint;
    private final AstLocal[] locals;
    private final AstSetExpr body;

    public AstDecl(boolean disjoint, AstLocal[] locals, AstSetExpr body) {
        this.disjoint = disjoint;
        this.locals = Check.noNullsNotEmpty(locals);
        this.body = Check.notNull(body);
    }

    public boolean isDisjoint() {
        return disjoint;
    }

    public AstLocal[] getLocals() {
        return locals;
    }

    public AstSetExpr getBody() {
        return body;
    }

    public AstDecl withBody(AstSetExpr body) {
        return new AstDecl(disjoint, locals, body);
    }

    @Override
    public String toString() {
        return (isDisjoint() ? "disj " : "") + Util.intercalate(";", locals) + ":" + body;
    }
}
