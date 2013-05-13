package org.clafer.ir;

import gnu.trove.TCollections;
import gnu.trove.TIntCollection;
import gnu.trove.set.hash.TIntHashSet;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.clafer.ir.IrDomain.IrBoundDomain;
import org.clafer.ir.IrDomain.IrEmptyDomain;
import org.clafer.ir.IrDomain.IrEnumDomain;

/**
 * 
 * @author jimmy
 */
public class Irs {

    /********************
     * 
     * Boolean
     * 
     ********************/
    public static final IrBoolVar True = new IrBoolVar("True", Boolean.TRUE);
    public static final IrBoolVar False = new IrBoolVar("False", Boolean.FALSE);

    public static IrBoolVar constant(boolean value) {
        return value ? True : False;
    }

    public static IrBoolVar bool(String name) {
        return new IrBoolVar(name);
    }

    public static IrBoolExpr not(IrBoolExpr proposition) {
        if (IrUtil.isTrue(proposition)) {
            return False;
        }
        if (IrUtil.isFalse(proposition)) {
            return True;
        }
        if (proposition instanceof IrDualExpr) {
            return ((IrDualExpr) proposition).opposite();
        }
        return new IrNot(proposition);
    }

    public static IrBoolExpr and(IrBoolExpr... operands) {
        List<IrBoolExpr> filter = new ArrayList<IrBoolExpr>(operands.length);
        for (IrBoolExpr operand : operands) {
            if (IrUtil.isFalse(operand)) {
                return False;
            }
            if (!IrUtil.isTrue(operand)) {
                filter.add(operand);
            }
        }
        switch (filter.size()) {
            case 0:
                return True;
            case 1:
                return filter.get(0);
            default:
                return new IrAnd(filter.toArray(new IrBoolExpr[filter.size()]));
        }
    }

    public static IrBoolExpr implies(IrBoolExpr antecedent, IrBoolExpr consequent) {
        if (IrUtil.isTrue(antecedent)) {
            return consequent;
        }
        if (IrUtil.isFalse(antecedent)) {
            return True;
        }
        if (IrUtil.isTrue(consequent)) {
            return True;
        }
        if (IrUtil.isFalse(consequent)) {
            return not(antecedent);
        }
        return new IrImplies(antecedent, consequent);
    }

    public static IrConstraint implies(IrBoolExpr antecedent, IrConstraint consequent) {
        if (IrUtil.isTrue(antecedent)) {
            return consequent;
        }
        if (IrUtil.isFalse(antecedent)) {
            return Tautalogy;
        }
        if (IrUtil.isTrue(consequent)) {
            return Tautalogy;
        }
        if (IrUtil.isFalse(consequent)) {
            return boolConstraint(not(antecedent));
        }
        return new IrHalfReification(antecedent, consequent);
    }

    public static IrBoolExpr ifOnlyIf(IrBoolExpr left, IrBoolExpr right) {
        if (IrUtil.isTrue(left)) {
            return right;
        }
        if (IrUtil.isFalse(left)) {
            return not(right);
        }
        if (IrUtil.isTrue(right)) {
            return left;
        }
        if (IrUtil.isFalse(right)) {
            return not(left);
        }
        return new IrIfOnlyIf(left, right);
    }

    public static IrBoolExpr member(IrIntExpr var, int low, int high) {
        return new IrMember(var, low, high);
    }

    public static IrBoolExpr notMember(IrIntExpr var, int low, int high) {
        return new IrNotMember(var, low, high);
    }

    public static IrCompare compare(IrIntExpr left, IrCompare.Op op, IrIntExpr right) {
        return new IrCompare(left, op, right);
    }

    public static IrBoolExpr equal(IrIntExpr left, int right) {
        return equal(left, constant(right));
    }

    public static IrBoolExpr equal(IrIntExpr left, IrIntExpr right) {
        if (left.equals(right)) {
            return True;
        }
        return new IrCompare(left, IrCompare.Op.Equal, right);
    }

    public static IrSetEquality equal(IrSetExpr left, IrSetEquality.Op op, IrSetExpr right) {
        return new IrSetEquality(left, op, right);
    }

    public static IrBoolExpr equal(IrSetExpr left, IrSetExpr right) {
        if (left.equals(right)) {
            return True;
        }
        return new IrSetEquality(left, IrSetEquality.Op.Equal, right);
    }

    public static IrBoolExpr notEqual(IrIntExpr left, int right) {
        return notEqual(left, constant(right));
    }

    public static IrBoolExpr notEqual(IrIntExpr left, IrIntExpr right) {
        if (left.equals(right)) {
            return False;
        }
        return new IrCompare(left, IrCompare.Op.NotEqual, right);
    }

    public static IrBoolExpr notEqual(IrSetExpr left, IrSetExpr right) {
        if (left.equals(right)) {
            return False;
        }
        return new IrSetEquality(left, IrSetEquality.Op.NotEqual, right);
    }

    public static IrCompare lessThan(IrIntExpr left, int right) {
        return lessThan(left, constant(right));
    }

    public static IrCompare lessThan(IrIntExpr left, IrIntExpr right) {
        return new IrCompare(left, IrCompare.Op.LessThan, right);
    }

    public static IrCompare lessThanEqual(IrIntExpr left, int right) {
        return lessThanEqual(left, constant(right));
    }

    public static IrCompare lessThanEqual(IrIntExpr left, IrIntExpr right) {
        return new IrCompare(left, IrCompare.Op.LessThanEqual, right);
    }

    public static IrCompare greaterThan(IrIntExpr left, int right) {
        return greaterThan(left, constant(right));
    }

    public static IrCompare greaterThan(IrIntExpr left, IrIntExpr right) {
        return new IrCompare(left, IrCompare.Op.GreaterThan, right);
    }

    public static IrCompare greaterThanEqual(IrIntExpr left, int right) {
        return greaterThanEqual(left, constant(right));
    }

    public static IrCompare greaterThanEqual(IrIntExpr left, IrIntExpr right) {
        return new IrCompare(left, IrCompare.Op.GreaterThanEqual, right);
    }

    /********************
     * 
     * Integers
     * 
     ********************/
    public static IrIntVar constant(int value) {
        return boundInt(Integer.toString(value), value, value);
    }

    public static IrIntVar boundInt(String name, int low, int high) {
        return new IrIntVar(name, boundDomain(low, high));
    }

    public static IrIntVar enumInt(String name, int[] values) {
        return new IrIntVar(name, enumDomain(values));
    }

    public static IrIntExpr card(IrSetExpr set) {
        return new IrCard(set);
    }

    public static IrIntExpr add(IrIntExpr left, int right) {
        return add(left, constant(right));
    }

    public static IrIntExpr add(IrIntExpr left, IrIntExpr right) {
        Integer leftConstant = IrUtil.getConstant(left);
        Integer rightConstant = IrUtil.getConstant(right);
        if (leftConstant != null && rightConstant != null) {
            return constant(leftConstant.intValue() + rightConstant.intValue());
        }
        if (leftConstant != null) {
            if (leftConstant.intValue() == 0) {
                return right;
            }
        }
        if (rightConstant != null) {
            if (rightConstant.intValue() == 0) {
                return left;
            }
        }
        return new IrArithm(left, IrArithm.Op.Add, right);
    }

    public static IrIntExpr sub(IrIntExpr left, int right) {
        return sub(left, constant(right));
    }

    public static IrIntExpr sub(IrIntExpr left, IrIntExpr right) {
        Integer leftConstant = IrUtil.getConstant(left);
        Integer rightConstant = IrUtil.getConstant(right);
        if (leftConstant != null && rightConstant != null) {
            return constant(leftConstant.intValue() - rightConstant.intValue());
        }
        if (rightConstant != null && rightConstant.intValue() == 0) {
            return left;
        }
        return new IrArithm(left, IrArithm.Op.Sub, right);
    }

    public static IrIntExpr mul(IrIntExpr left, int right) {
        return mul(left, constant(right));

    }

    public static IrIntExpr mul(IrIntExpr left, IrIntExpr right) {
        Integer leftConstant = IrUtil.getConstant(left);
        Integer rightConstant = IrUtil.getConstant(right);
        if (leftConstant != null && rightConstant != null) {
            return constant(leftConstant.intValue() * rightConstant.intValue());
        }
        if (leftConstant != null) {
            if (leftConstant.intValue() == 0) {
                return constant(0);
            }
            if (leftConstant.intValue() == 1) {
                return right;
            }
        }
        if (rightConstant != null) {
            if (rightConstant.intValue() == 0) {
                return constant(0);
            }
            if (rightConstant.intValue() == 1) {
                return left;
            }
        }
        return new IrArithm(left, IrArithm.Op.Mul, right);
    }

    public static IrIntExpr div(IrIntExpr left, int right) {
        return div(left, constant(right));
    }

    public static IrIntExpr div(IrIntExpr left, IrIntExpr right) {
        Integer leftConstant = IrUtil.getConstant(left);
        Integer rightConstant = IrUtil.getConstant(right);
        if (leftConstant != null && rightConstant != null) {
            return constant(leftConstant.intValue() / rightConstant.intValue());
        }
        if (rightConstant != null && rightConstant.intValue() == 1) {
            return left;
        }
        return new IrArithm(left, IrArithm.Op.Div, right);
    }

    public static IrIntExpr sum(IrIntExpr... addends) {
        // TODO: optimize
        return new IrSum(addends);
    }

    public static IrIntExpr element(IrIntExpr[] array, IrIntExpr index) {
        Integer constant = IrUtil.getConstant(index);
        if (constant != null) {
            return array[constant.intValue()];
        }
        return new IrElement(array, index);
    }
    /********************
     * 
     * Set
     * 
     ********************/
    public static final IrDomain EmptyDomain = new IrEmptyDomain();
    public static final IrDomain ZeroDomain = new IrEnumDomain(new int[]{0});
    public static final IrDomain OneDomain = new IrEnumDomain(new int[]{1});
    public static final IrSetVar EmptySet = new IrSetVar("{}", EmptyDomain, EmptyDomain, ZeroDomain);

    public static IrDomain boundDomain(int low, int high) {
        return new IrBoundDomain(low, high);
    }

    public static IrDomain enumDomain(int... values) {
        return enumDomain(new TIntHashSet(values));
    }

    public static IrDomain enumDomain(TIntCollection values) {
        return enumDomain(new TIntHashSet(values));
    }

    // TODO: collect enum over an interval into a bound domain
    public static IrDomain enumDomain(TIntHashSet values) {
        switch (values.size()) {
            case 0:
                return EmptyDomain;
            case 1:
                int value = values.iterator().next();
                return boundDomain(value, value);
            default:
                int[] array = values.toArray();
                Arrays.sort(array);
                return new IrEnumDomain(array);
        }
    }

    public static IrSetVar constant(int[] value) {
        IrDomain domain = enumDomain(value);
        return new IrSetVar(Arrays.toString(value), domain, domain, enumDomain(value.length));
    }

    public static IrSetVar set(String name, int lowEnv, int highEnv) {
        return set(name, boundDomain(lowEnv, highEnv), EmptyDomain);
    }

    public static IrSetVar set(String name, int lowEnv, int highEnv, int lowKer, int highKer) {
        return set(name, boundDomain(lowEnv, highEnv), boundDomain(lowKer, highKer));
    }

    public static IrSetVar set(String name, int lowEnv, int highEnv, int[] ker) {
        return set(name, boundDomain(lowEnv, highEnv), enumDomain(ker));
    }

    public static IrSetVar set(String name, int[] env) {
        return set(name, enumDomain(env), EmptyDomain);
    }

    public static IrSetVar set(String name, int[] env, int lowKer, int highKer) {
        return set(name, enumDomain(env), boundDomain(lowKer, highKer));
    }

    public static IrSetVar set(String name, int[] env, int[] ker) {
        return set(name, enumDomain(env), enumDomain(ker));
    }

    public static IrSetVar set(String name, IrDomain env, IrDomain ker) {
        return new IrSetVar(name, env, ker, boundDomain(ker.size(), env.size()));
    }

    public static IrSetVar set(String name, IrDomain env, IrDomain ker, IrDomain card) {
        return new IrSetVar(name, env, ker, card);
    }

    public static IrSetExpr singleton(IrIntExpr value) {
        Integer constant = IrUtil.getConstant(value);
        if (constant != null) {
            return constant(new int[]{constant.intValue()});
        }
        return new IrSingleton(value);
    }

    public static IrSetExpr arrayToSet(IrIntExpr[] array) {
        switch (array.length) {
            case 0:
                return EmptySet;
            case 1:
                return singleton(array[0]);
            default:
                return new IrArrayToSet(array);
        }
    }

    public static IrSetExpr join(IrSetExpr take, IrSetExpr[] children) {
        int[] constant = IrUtil.getConstant(take);
        if (constant != null) {
            IrSetExpr[] to = new IrSetExpr[constant.length];
            for (int i = 0; i < to.length; i++) {
                to[i] = children[constant[i]];
            }
            return union(to);
        }
        return new IrJoin(take, children);
    }

    public static IrSetExpr joinRef(IrSetExpr take, IrIntExpr[] refs) {
        int[] constant = IrUtil.getConstant(take);
        if (constant != null) {
            IrIntExpr[] to = new IrIntExpr[constant.length];
            for (int i = 0; i < to.length; i++) {
                to[i] = refs[constant[i]];
            }
            return arrayToSet(to);
        }
        return new IrJoinRef(take, refs);
    }

    // TODO: Partially union set of constants
    public static IrSetExpr union(IrSetExpr[] operands) {
        switch (operands.length) {
            case 0:
                return EmptySet;
            case 1:
                return operands[0];
            default:
                return new IrUnion(operands);
        }
    }
    /**
     * Constraints
     */
    public static final IrBoolConstraint Tautalogy = new IrBoolConstraint(True);
    public static final IrBoolConstraint FalseTautalogy = new IrBoolConstraint(False);

    public static IrBoolConstraint boolConstraint(IrBoolExpr expr) {
        if (IrUtil.isTrue(expr)) {
            return Tautalogy;
        }
        if (IrUtil.isFalse(expr)) {
            return FalseTautalogy;
        }
        return new IrBoolConstraint(expr);
    }

    public static IrBoolChannel boolChannel(IrBoolExpr[] bools, IrSetExpr set) {
        return new IrBoolChannel(bools, set);
    }

    public static IrIntChannel intChannel(IrIntExpr[] ints, IrSetExpr[] sets) {
        return new IrIntChannel(ints, sets);
    }

    public static IrConstraint sort(IrIntExpr[] array) {
        if (array.length < 2) {
            return Tautalogy;
        }
        return new IrSortInts(array);
    }

    public static IrConstraint sort(IrIntExpr[]... strings) {
        if (strings.length < 2) {
            return Tautalogy;
        }
        return new IrSortStrings(strings);
    }

    public static IrConstraint allDifferent(IrIntExpr[] ints) {
        if (ints.length < 2) {
            return Tautalogy;
        }
        return new IrAllDifferent(ints);
    }

    public static IrConstraint selectN(IrBoolExpr[] bools, IrIntExpr n) {
        return new IrSelectN(bools, n);
    }
}
