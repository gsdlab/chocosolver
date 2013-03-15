package org.clafer;

import choco.Choco;
import choco.kernel.model.constraints.Constraint;
import choco.kernel.model.variables.integer.IntegerExpressionVariable;
import choco.kernel.model.variables.integer.IntegerVariable;
import choco.kernel.model.variables.set.SetVariable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

/**
 *
 * @author jimmy
 */
public class ChocoUtil {

    public static Constraint increasingSum(List<IntegerVariable> variables, IntegerVariable sum) {
        return Choco.increasingSum(variables.toArray(new IntegerVariable[variables.size()]), sum);
    }

    public static Constraint decreasingSum(IntegerVariable[] variables, IntegerVariable sum) {
        IntegerVariable[] decreasing = Arrays.copyOf(variables, variables.length);
        Util.reverse(decreasing);
        return Choco.increasingSum(decreasing, sum);
    }

    public static Constraint decreasingSum(List<IntegerVariable> variables, IntegerVariable sum) {
        List<IntegerVariable> decreasing = new ArrayList<IntegerVariable>(variables);
        Collections.reverse(decreasing);
        return increasingSum(decreasing, sum);
    }

    public static IntegerExpressionVariable plus(IntegerExpressionVariable t, int v) {
        if (v == 0) {
            return t;
        }
        return Choco.plus(t, v);
    }

    public static Constraint betweenCard(SetVariable set, int low, int high) {
        if (low == high) {
            return Choco.eqCard(set, low);
        }
        List<Constraint> constraints = new ArrayList<Constraint>();
        if (low != 0) {
            constraints.add(Choco.geqCard(set, low));
        }
        if (high != Integer.MAX_VALUE) {
            constraints.add(Choco.leqCard(set, high));
        }
        return and(constraints);
    }

    public static Constraint or(List<Constraint> constraints) {
        return Choco.or(constraints.toArray(new Constraint[constraints.size()]));
    }

    public static Constraint and(List<Constraint> constraints) {
        return Choco.and(constraints.toArray(new Constraint[constraints.size()]));
    }
}
