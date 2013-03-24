package org.clafer.ast;

/**
 *
 * @author jimmy
 */
public class Ast {

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

    public static AstCompare compare(AstExpression left, AstCompare.Op op, AstExpression right) {
        return new AstCompare(left, op, right);
    }

    public static AstCompare equal(AstExpression left, AstExpression right) {
        return compare(left, AstCompare.Op.Equal, right);
    }

    public static AstCompare notEqual(AstExpression left, AstExpression right) {
        return compare(left, AstCompare.Op.NotEqual, right);
    }

    public static AstCompare lessThan(AstExpression left, AstExpression right) {
        return compare(left, AstCompare.Op.LessThan, right);
    }

    public static AstCompare lessThanEqual(AstExpression left, AstExpression right) {
        return compare(left, AstCompare.Op.LessThanEqual, right);
    }

    public static AstCompare greaterThan(AstExpression left, AstExpression right) {
        return compare(left, AstCompare.Op.GreaterThan, right);
    }

    public static AstCompare greaterThanEqual(AstExpression left, AstExpression right) {
        return compare(left, AstCompare.Op.GreaterThanEqual, right);
    }

    public static AstUpcast upcast(AstSetExpression base, AstAbstractClafer target) {
        return new AstUpcast(base, target);
    }

    public static AstNone none(AstSetExpression set) {
        return new AstNone(set);
    }
}
