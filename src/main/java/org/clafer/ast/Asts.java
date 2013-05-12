package org.clafer.ast;

import org.clafer.ast.AstQuantify.Quantifier;

/**
 *
 * @author jimmy
 */
public class Asts {

    public static final AstIntClafer IntType = AstIntClafer.Singleton;
    public static final AstBoolClafer BoolType = AstBoolClafer.Singleton;

    public static AstModel newModel() {
        return new AstModel();
    }

    public static AstThis $this() {
        return new AstThis();
    }

    public static AstGlobal global(AstClafer type) {
        return new AstGlobal(type);
    }

    public static AstConstant constant(int value) {
        return new AstConstant(value);
    }

    public static AstJoin join(AstSetExpr left, AstConcreteClafer right) {
        return new AstJoin(left, right);
    }

    public static AstJoinParent joinParent(AstSetExpr children) {
        return new AstJoinParent(children);
    }

    public static AstJoinRef joinRef(AstSetExpr deref) {
        return new AstJoinRef(deref);
    }

    public static AstCard card(AstSetExpr set) {
        return new AstCard(set);
    }

    public static AstEqual equal(AstSetExpr left, AstEqual.Op op, AstSetExpr right) {
        return new AstEqual(left, op, right);
    }

    public static AstEqual equal(AstSetExpr left, AstSetExpr right) {
        return equal(left, AstEqual.Op.Equal, right);
    }

    public static AstEqual notEqual(AstSetExpr left, AstSetExpr right) {
        return equal(left, AstEqual.Op.NotEqual, right);
    }

    public static AstCompare compare(AstSetExpr left, AstCompare.Op op, AstSetExpr right) {
        return new AstCompare(left, op, right);
    }

    public static AstCompare lessThan(AstSetExpr left, AstSetExpr right) {
        return compare(left, AstCompare.Op.LessThan, right);
    }

    public static AstCompare lessThanEqual(AstSetExpr left, AstSetExpr right) {
        return compare(left, AstCompare.Op.LessThanEqual, right);
    }

    public static AstCompare greaterThan(AstSetExpr left, AstSetExpr right) {
        return compare(left, AstCompare.Op.GreaterThan, right);
    }

    public static AstCompare greaterThanEqual(AstSetExpr left, AstSetExpr right) {
        return compare(left, AstCompare.Op.GreaterThanEqual, right);
    }

    public static AstUpcast upcast(AstSetExpr base, AstAbstractClafer target) {
        return new AstUpcast(base, target);
    }

    public static AstNone none(AstSetExpr set) {
        return new AstNone(set);
    }

    public static AstLocal local() {
        return new AstLocal();
    }

    public static AstDecl decl(AstLocal[] locals, AstSetExpr body) {
        return new AstDecl(false, locals, body);
    }

    public static AstDecl disjointDecl(AstLocal[] locals, AstSetExpr body) {
        return new AstDecl(true, locals, body);
    }

    public static AstQuantify quantify(Quantifier quantifier, AstDecl[] decls, AstBoolExpr body) {
        return new AstQuantify(quantifier, decls, body);
    }

    public static AstQuantify some(AstDecl[] decls, AstBoolExpr body) {
        return new AstQuantify(Quantifier.Some, decls, body);
    }

    public static AstQuantify all(AstDecl[] decls, AstBoolExpr body) {
        return new AstQuantify(Quantifier.All, decls, body);
    }
}
