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

    public static AstBoolExpr not(AstBoolExpr expr) {
        return new AstNot(expr);
    }

    public static AstSetExpr minus(AstSetExpr expr) {
        return new AstMinus(expr);
    }

    public static AstSetExpr card(AstSetExpr set) {
        return new AstCard(set);
    }

    public static AstBoolExpr test(AstSetExpr left, AstSetTest.Op op, AstSetExpr right) {
        return new AstSetTest(left, op, right);
    }

    public static AstBoolExpr equal(AstSetExpr left, AstSetExpr right) {
        return test(left, AstSetTest.Op.Equal, right);
    }

    public static AstBoolExpr notEqual(AstSetExpr left, AstSetExpr right) {
        return test(left, AstSetTest.Op.NotEqual, right);
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

    public static AstBoolExpr arithm(AstBoolArithm.Op op, AstBoolExpr... operands) {
        if (operands.length == 1) {
            return operands[0];
        }
        return new AstBoolArithm(op, operands);
    }

    public static AstBoolExpr and(AstBoolExpr... operands) {
        return arithm(AstBoolArithm.Op.And, operands);
    }

    public static AstBoolExpr ifOnlyIf(AstBoolExpr... operands) {
        return arithm(AstBoolArithm.Op.IfOnlyIf, operands);
    }

    public static AstBoolExpr implies(AstBoolExpr... operands) {
        return arithm(AstBoolArithm.Op.Implies, operands);
    }

    public static AstBoolExpr or(AstBoolExpr... operands) {
        return arithm(AstBoolArithm.Op.Or, operands);
    }

    public static AstBoolExpr xor(AstBoolExpr... operands) {
        return arithm(AstBoolArithm.Op.Xor, operands);
    }

    public static AstSetExpr arithm(AstSetArithm.Op op, AstSetExpr... operands) {
        if (operands.length == 1) {
            return operands[0];
        }
        return new AstSetArithm(op, operands);
    }

    public static AstSetExpr union(AstSetExpr... addends) {
        return arithm(AstSetArithm.Op.Union, addends);
    }

    public static AstSetExpr diff(AstSetExpr... addends) {
        return arithm(AstSetArithm.Op.Difference, addends);
    }

    public static AstSetExpr inter(AstSetExpr... addends) {
        return arithm(AstSetArithm.Op.Intersection, addends);
    }

    public static AstBoolExpr membership(AstSetExpr member, AstMembership.Op op, AstSetExpr set) {
        return new AstMembership(member, op, set);
    }

    public static AstBoolExpr in(AstSetExpr member, AstSetExpr set) {
        return membership(member, AstMembership.Op.In, set);
    }

    public static AstBoolExpr notIn(AstSetExpr member, AstSetExpr set) {
        return membership(member, AstMembership.Op.NotIn, set);
    }

    public static AstSetExpr ifThenElse(AstBoolExpr antecedent, AstSetExpr consequent, AstSetExpr alternative) {
        return new AstTernary(antecedent, consequent, alternative);
    }

    public static AstBoolExpr ifThenElse(AstBoolExpr antecedent, AstBoolExpr consequent, AstBoolExpr alternative) {
        return new AstIfThenElse(antecedent, consequent, alternative);
    }

    public static AstSetExpr upcast(AstSetExpr base, AstAbstractClafer target) {
        return new AstUpcast(base, target);
    }

    public static AstBoolExpr lone(AstClafer clafer) {
        // Syntactic sugar.
        return lone(global(clafer));
    }

    public static AstBoolExpr lone(AstSetExpr set) {
        // Syntactic sugar.
        return lessThanEqual(card(set), constant(1));
    }

    public static AstBoolExpr none(AstClafer clafer) {
        // Syntactic sugar.
        return none(global(clafer));
    }

    public static AstBoolExpr none(AstSetExpr set) {
        // Syntactic sugar.
        return equal(card(set), constant(0));
    }

    public static AstBoolExpr one(AstClafer clafer) {
        // Syntactic sugar.
        return one(global(clafer));
    }

    public static AstBoolExpr one(AstSetExpr set) {
        // Syntactic sugar.
        return equal(card(set), constant(1));
    }

    public static AstBoolExpr some(AstClafer clafer) {
        // Syntactic sugar.
        return some(global(clafer));
    }

    public static AstBoolExpr some(AstSetExpr set) {
        // Syntactic sugar.
        return greaterThanEqual(card(set), constant(1));
    }

    public static AstLocal local(String name) {
        return new AstLocal(name);
    }

    public static AstDecl decl(AstLocal[] locals, AstSetExpr body) {
        return new AstDecl(false, locals, body);
    }

    public static AstDecl decl(AstLocal local, AstSetExpr body) {
        return decl(new AstLocal[]{local}, body);
    }

    public static AstDecl disjDecl(AstLocal[] locals, AstSetExpr body) {
        return new AstDecl(true, locals, body);
    }

    public static AstBoolExpr quantify(Quantifier quantifier, AstDecl[] decls, AstBoolExpr body) {
        return new AstQuantify(quantifier, decls, body);
    }

    public static AstBoolExpr quantify(Quantifier quantifier, AstDecl decl, AstBoolExpr body) {
        return quantify(quantifier, new AstDecl[]{decl}, body);
    }

    public static AstBoolExpr all(AstDecl[] decls, AstBoolExpr body) {
        return quantify(Quantifier.All, decls, body);
    }

    public static AstBoolExpr all(AstDecl decl, AstBoolExpr body) {
        return quantify(Quantifier.All, decl, body);
    }

    public static AstBoolExpr lone(AstDecl[] decls, AstBoolExpr body) {
        return quantify(Quantifier.Lone, decls, body);
    }

    public static AstBoolExpr lone(AstDecl decl, AstBoolExpr body) {
        return quantify(Quantifier.Lone, decl, body);
    }

    public static AstBoolExpr none(AstDecl[] decls, AstBoolExpr body) {
        return quantify(Quantifier.None, decls, body);
    }

    public static AstBoolExpr none(AstDecl decl, AstBoolExpr body) {
        return quantify(Quantifier.None, decl, body);
    }

    public static AstBoolExpr one(AstDecl[] decls, AstBoolExpr body) {
        return quantify(Quantifier.One, decls, body);
    }

    public static AstBoolExpr one(AstDecl decl, AstBoolExpr body) {
        return quantify(Quantifier.One, decl, body);
    }

    public static AstBoolExpr some(AstDecl[] decls, AstBoolExpr body) {
        return quantify(Quantifier.Some, decls, body);
    }

    public static AstBoolExpr some(AstDecl decl, AstBoolExpr body) {
        return quantify(Quantifier.Some, decl, body);
    }
}
