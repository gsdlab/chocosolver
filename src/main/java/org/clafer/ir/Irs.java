package org.clafer.ir;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.clafer.ir.IrDomain.IrBoundDomain;
import org.clafer.ir.IrDomain.IrEnumDomain;
import solver.Solver;
import solver.constraints.IntConstraintFactory;
import solver.variables.IntVar;
import solver.variables.VariableFactory;

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

    public static IrCompare equal(IrIntExpr left, int right) {
        return new IrCompare(left, IrCompare.Op.Equal, constant(right));
    }

    public static IrCompare equal(IrIntExpr left, IrIntExpr right) {
        return new IrCompare(left, IrCompare.Op.Equal, right);
    }

    public static IrSetCompare equal(IrSetExpr left, IrSetExpr right) {
        return new IrSetCompare(left, IrSetCompare.Op.Equal, right);
    }

    public static IrCompare notEqual(IrIntExpr left, int right) {
        return new IrCompare(left, IrCompare.Op.NotEqual, constant(right));
    }

    public static IrCompare notEqual(IrIntExpr left, IrIntExpr right) {
        return new IrCompare(left, IrCompare.Op.NotEqual, right);
    }

    public static IrSetCompare notEqual(IrSetExpr left, IrSetExpr right) {
        return new IrSetCompare(left, IrSetCompare.Op.NotEqual, right);
    }

    public static IrCompare lessThan(IrIntExpr left, int right) {
        return new IrCompare(left, IrCompare.Op.LessThan, constant(right));
    }

    public static IrCompare lessThan(IrIntExpr left, IrIntExpr right) {
        return new IrCompare(left, IrCompare.Op.LessThan, right);
    }

    public static IrCompare lessThanEqual(IrIntExpr left, int right) {
        return new IrCompare(left, IrCompare.Op.LessThanEqual, constant(right));
    }

    public static IrCompare lessThanEqual(IrIntExpr left, IrIntExpr right) {
        return new IrCompare(left, IrCompare.Op.LessThanEqual, right);
    }

    public static IrCompare greaterThan(IrIntExpr left, int right) {
        return new IrCompare(left, IrCompare.Op.GreaterThan, constant(right));
    }

    public static IrCompare greaterThan(IrIntExpr left, IrIntExpr right) {
        return new IrCompare(left, IrCompare.Op.GreaterThan, right);
    }

    public static IrCompare greaterThanEqual(IrIntExpr left, int right) {
        return new IrCompare(left, IrCompare.Op.GreaterThanEqual, constant(right));
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
        return new IrIntVar(name, new IrBoundDomain(low, high));
    }

    public static IrIntVar enumInt(String name, int[] values) {
        return new IrIntVar(name, new IrEnumDomain(values));
    }

    public static IrIntExpr setCard(IrSetExpr set) {
        return new IrSetCard(set);
    }

    public static IrIntExpr div(IrIntExpr numerator, IrIntExpr denominator) {
        Integer numeratorConstant = IrUtil.getConstant(numerator);
        Integer denominatorConstant = IrUtil.getConstant(denominator);
        if (numeratorConstant != null && denominatorConstant != null) {
            return constant(numeratorConstant.intValue() / denominatorConstant.intValue());
        }
        if (denominatorConstant != null && denominatorConstant.intValue() == 1) {
            return numerator;
        }
        return new IrDiv(numerator, denominator);
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

    public static IrSetExpr singleton(IrIntExpr value) {
        Integer constant = IrUtil.getConstant(value);
        if (constant != null) {
            return constant(new int[]{constant.intValue()});
        }
        return new IrSingleton(value);
    }

    public static IrSetExpr join(IrSetExpr take, IrSetExpr[] children) {
        int[] constant = IrUtil.getConstant(take);
        if (constant != null) {
            IrSetExpr[] to = new IrSetExpr[constant.length];
            for (int i = 0; i < to.length; i++) {
                to[i] = children[constant[i]];
                return union(to);
            }
        }
        return new IrJoin(take, children);
    }

    public static IrSetExpr joinRef(IrSetExpr take, IrIntExpr[] refs) {
        int[] constant = IrUtil.getConstant(take);
        if (constant != null) {
            IrIntExpr[] to = new IrIntExpr[constant.length];
            for (int i = 0; i < to.length; i++) {
                to[i] = refs[constant[i]];
                return tupleToSet(to);
            }
        }
        return new IrJoinRef(take, refs);
    }

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

    public static IrSetExpr tupleToSet(IrIntExpr[] tuple) {
        switch (tuple.length) {
            case 0:
                return EmptySet;
            case 1:
                return singleton(tuple[0]);
            default:
                throw new UnsupportedOperationException();
        }
    }

    public static void main(String[] args) {
        Solver solver = new Solver();

        IntVar x = VariableFactory.enumerated("x", 0, 10, solver);
        IntVar y = VariableFactory.enumerated("y", 0, 10, solver);
        IntVar z = VariableFactory.enumerated("z", 0, 10, solver);
        solver.post(IntConstraintFactory.eucl_div(x, y, z));

        if (solver.findSolution()) {
            do {
                System.out.println(solver);
            } while (solver.nextSolution());
        }
    }
}
