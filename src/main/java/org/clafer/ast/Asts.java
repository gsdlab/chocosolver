package org.clafer.ast;

import org.clafer.ast.AstQuantify.Quantifier;

/**
 *
 * @author jimmy
 */
public class Asts {

    private Asts() {
    }
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

    public static AstSetExpr join(AstSetExpr left, AstConcreteClafer right) {
        return new AstJoin(left, right);
    }

    public static AstSetExpr joinParent(AstSetExpr children) {
        return new AstJoinParent(children);
    }

    public static AstSetExpr joinRef(AstSetExpr deref) {
        return new AstJoinRef(deref);
    }

    public static AstSetExpr card(AstSetExpr set) {
        return new AstCard(set);
    }

    public static AstBoolExpr equal(AstSetExpr left, AstEqual.Op op, AstSetExpr right) {
        return new AstEqual(left, op, right);
    }

    public static AstBoolExpr equal(AstSetExpr left, AstSetExpr right) {
        return equal(left, AstEqual.Op.Equal, right);
    }

    public static AstBoolExpr notEqual(AstSetExpr left, AstSetExpr right) {
        return equal(left, AstEqual.Op.NotEqual, right);
    }

    public static AstBoolExpr compare(AstSetExpr left, AstCompare.Op op, AstSetExpr right) {
        return new AstCompare(left, op, right);
    }

    public static AstBoolExpr lessThan(AstSetExpr left, AstSetExpr right) {
        return compare(left, AstCompare.Op.LessThan, right);
    }

    public static AstBoolExpr lessThanEqual(AstSetExpr left, AstSetExpr right) {
        return compare(left, AstCompare.Op.LessThanEqual, right);
    }

    public static AstBoolExpr greaterThan(AstSetExpr left, AstSetExpr right) {
        return compare(left, AstCompare.Op.GreaterThan, right);
    }

    public static AstBoolExpr greaterThanEqual(AstSetExpr left, AstSetExpr right) {
        return compare(left, AstCompare.Op.GreaterThanEqual, right);
    }

    public static AstSetExpr arithm(AstArithm.Op op, AstSetExpr... operands) {
        if (operands.length == 1) {
            return operands[0];
        }
        return new AstArithm(op, operands);
    }

    public static AstSetExpr add(AstSetExpr... addends) {
        return arithm(AstArithm.Op.Add, addends);
    }

    public static AstSetExpr sub(AstSetExpr... subtrahends) {
        return arithm(AstArithm.Op.Sub, subtrahends);
    }

    public static AstSetExpr mul(AstSetExpr... multipliers) {
        return arithm(AstArithm.Op.Mul, multipliers);
    }

    public static AstSetExpr div(AstSetExpr... divisors) {
        return arithm(AstArithm.Op.Div, divisors);
    }

    public static AstSetExpr setArithm(AstSetArithm.Op op, AstSetExpr... operands) {
        if (operands.length == 1) {
            return operands[0];
        }
        return new AstSetArithm(op, operands);
    }

    public static AstSetExpr setUnion(AstSetExpr... addends) {
        return setArithm(AstSetArithm.Op.Union, addends);
    }

    public static AstSetExpr setDiff(AstSetExpr... addends) {
        return setArithm(AstSetArithm.Op.Difference, addends);
    }

    public static AstSetExpr setInter(AstSetExpr... addends) {
        return setArithm(AstSetArithm.Op.Intersection, addends);
    }

    public static AstSetExpr upcast(AstSetExpr base, AstAbstractClafer target) {
        return new AstUpcast(base, target);
    }

    public static AstBoolExpr none(AstSetExpr set) {
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
