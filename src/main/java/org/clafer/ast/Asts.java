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

    public static AstConstantInt constantInt(int value) {
        return new AstConstantInt(value);
    }

    public static AstJoin join(AstSetExpression left, AstConcreteClafer right) {
        return new AstJoin(left, right);
    }

    public static AstJoinParent joinParent(AstSetExpression children) {
        return new AstJoinParent(children);
    }

    public static AstJoinRef joinRef(AstSetExpression deref) {
        return new AstJoinRef(deref);
    }

    public static AstCard card(AstSetExpression set) {
        return new AstCard(set);
    }

    public static AstCompare compare(AstSetExpression left, AstCompare.Op op, AstSetExpression right) {
        return new AstCompare(left, op, right);
    }

    public static AstCompare equal(AstSetExpression left, AstSetExpression right) {
        return compare(left, AstCompare.Op.Equal, right);
    }

    public static AstCompare notEqual(AstSetExpression left, AstSetExpression right) {
        return compare(left, AstCompare.Op.NotEqual, right);
    }

    public static AstCompare lessThan(AstSetExpression left, AstSetExpression right) {
        return compare(left, AstCompare.Op.LessThan, right);
    }

    public static AstCompare lessThanEqual(AstSetExpression left, AstSetExpression right) {
        return compare(left, AstCompare.Op.LessThanEqual, right);
    }

    public static AstCompare greaterThan(AstSetExpression left, AstSetExpression right) {
        return compare(left, AstCompare.Op.GreaterThan, right);
    }

    public static AstCompare greaterThanEqual(AstSetExpression left, AstSetExpression right) {
        return compare(left, AstCompare.Op.GreaterThanEqual, right);
    }

    public static AstUpcast upcast(AstSetExpression base, AstAbstractClafer target) {
        return new AstUpcast(base, target);
    }

    public static AstNone none(AstSetExpression set) {
        return new AstNone(set);
    }

    public static AstLocal local() {
        return new AstLocal();
    }

    public static AstDecl decl(AstLocal[] locals, AstSetExpression body) {
        return new AstDecl(false, locals, body);
    }

    public static AstDecl disjointDecl(AstLocal[] locals, AstSetExpression body) {
        return new AstDecl(true, locals, body);
    }

    public static AstQuantify quantify(Quantifier quantifier, AstDecl[] decls, AstBoolExpression body) {
        return new AstQuantify(quantifier, decls, body);
    }

    public static AstQuantify some(AstDecl[] decls, AstBoolExpression body) {
        return new AstQuantify(Quantifier.Some, decls, body);
    }

    public static AstQuantify all(AstDecl[] decls, AstBoolExpression body) {
        return new AstQuantify(Quantifier.All, decls, body);
    }
}
