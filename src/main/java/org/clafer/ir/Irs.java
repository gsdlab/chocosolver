package org.clafer.ir;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.clafer.ir.IrDomain.IrBoundDomain;
import org.clafer.ir.IrDomain.IrEnumDomain;

/**
 *
 * @author jimmy
 */
public class Irs {

    /**
     * Constraints
     */
    public static IrBoolConstraint boolConstraint(IrBoolExpr expr) {
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
            return boolConstraint(True);
        }
        return new IrSortInts(array);
    }

    public static IrConstraint sort(IrIntExpr[][] strings) {
        if (strings.length < 2) {
            return boolConstraint(True);
        }
        return new IrSortStrings(strings);
    }

    public static IrConstraint allDifferent(IrIntExpr[] ints) {
        if (ints.length < 2) {
            return boolConstraint(True);
        }
        return new IrAllDifferent(ints);
    }

    public static IrConstraint selectN(IrBoolExpr[] bools, IrIntExpr n) {
        return new IrSelectN(bools, n);
    }
    /********************
     * 
     * Boolean
     * 
     ********************/
    public static IrBoolVar True = new IrBoolVar("True", Boolean.TRUE);
    public static IrBoolVar False = new IrBoolVar("False", Boolean.FALSE);

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
        return new IrNot(proposition);
    }

    public static IrBoolExpr and(IrBoolExpr... operands) {
        return and(Arrays.asList(operands));
    }

    public static IrBoolExpr and(List<IrBoolExpr> operands) {
        List<IrBoolExpr> filter = new ArrayList<IrBoolExpr>();
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

    public static IrIntCompare equal(IrIntExpr left, int right) {
        return new IrIntCompare(left, IrIntCompare.Op.Equal, constant(right));
    }

    public static IrIntCompare equal(IrIntExpr left, IrIntExpr right) {
        return new IrIntCompare(left, IrIntCompare.Op.Equal, right);
    }

    public static IrSetCompare equal(IrSetExpr left, IrSetExpr right) {
        return new IrSetCompare(left, IrSetCompare.Op.Equal, right);
    }

    public static IrIntCompare notEqual(IrIntExpr left, int right) {
        return new IrIntCompare(left, IrIntCompare.Op.NotEqual, constant(right));
    }

    public static IrIntCompare notEqual(IrIntExpr left, IrIntExpr right) {
        return new IrIntCompare(left, IrIntCompare.Op.NotEqual, right);
    }

    public static IrSetCompare notEqual(IrSetExpr left, IrSetExpr right) {
        return new IrSetCompare(left, IrSetCompare.Op.NotEqual, right);
    }

    public static IrIntCompare lessThan(IrIntExpr left, int right) {
        return new IrIntCompare(left, IrIntCompare.Op.LessThan, constant(right));
    }

    public static IrIntCompare lessThan(IrIntExpr left, IrIntExpr right) {
        return new IrIntCompare(left, IrIntCompare.Op.LessThan, right);
    }

    public static IrIntCompare lessThanEqual(IrIntExpr left, int right) {
        return new IrIntCompare(left, IrIntCompare.Op.LessThanEqual, constant(right));
    }

    public static IrIntCompare lessThanEqual(IrIntExpr left, IrIntExpr right) {
        return new IrIntCompare(left, IrIntCompare.Op.LessThanEqual, right);
    }

    public static IrIntCompare greaterThan(IrIntExpr left, int right) {
        return new IrIntCompare(left, IrIntCompare.Op.GreaterThan, constant(right));
    }

    public static IrIntCompare greaterThan(IrIntExpr left, IrIntExpr right) {
        return new IrIntCompare(left, IrIntCompare.Op.GreaterThan, right);
    }

    public static IrIntCompare greaterThanEqual(IrIntExpr left, int right) {
        return new IrIntCompare(left, IrIntCompare.Op.GreaterThanEqual, constant(right));
    }

    public static IrIntCompare greaterThanEqual(IrIntExpr left, IrIntExpr right) {
        return new IrIntCompare(left, IrIntCompare.Op.GreaterThanEqual, right);
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
        return new IrIntVar(name, new IrBoundDomain(low, high));
    }

    public static IrIntVar enumInt(String name, int[] values) {
        return new IrIntVar(name, new IrEnumDomain(values));
    }

    public static IrIntExpr setCard(IrSetExpr set) {
        return new IrSetCard(set);
    }
    /********************
     * 
     * Set
     * 
     ********************/
    private static final IrDomain EmptyDomain = new IrEnumDomain(new int[0]);
    public static final IrSetVar EmptySet = new IrSetVar("{}", EmptyDomain, EmptyDomain);

    public static IrSetVar constant(int[] value) {
        return new IrSetVar(Arrays.toString(value), new IrEnumDomain(value), new IrEnumDomain(value));
    }

    public static IrSetVar set(String name, int low, int high) {
        return new IrSetVar(name, new IrBoundDomain(low, high), EmptyDomain);
    }

    public static IrSetVar set(String name, int[] values) {
        return new IrSetVar(name, new IrEnumDomain(values), EmptyDomain);
    }

    public static IrSetExpr union(IrSetExpr[] operands) {
        switch (operands.length) {
            case 0:
                throw new IllegalArgumentException();
            case 1:
                return operands[0];
            default:
                return new IrUnion(operands);
        }
    }
}
