package org.clafer.math;

import gnu.trove.iterator.TObjectIntIterator;
import gnu.trove.list.TIntList;
import gnu.trove.list.array.TIntArrayList;
import gnu.trove.map.TObjectIntMap;
import gnu.trove.map.hash.TObjectIntHashMap;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.clafer.common.Check;

/**
 * @author jimmy
 */
public class LinearFunction implements LinearFunctionable {

    private final int[] coefficients;
    private final Variable[] variables;
    private final int constant;
    public final TObjectIntMap<Variable> coefficientMap;

    public LinearFunction(int constant) {
        this.coefficients = new int[0];
        this.variables = new Variable[0];
        this.constant = constant;
        this.coefficientMap = new TObjectIntHashMap<>(0);
    }

    public LinearFunction(int[] coefficients, Variable[] variables, int constant) {
        Check.notNull(coefficients);
        Check.noNulls(variables);
        if (coefficients.length != variables.length) {
            throw new IllegalArgumentException();
        }
        TObjectIntMap<Variable> map = new TObjectIntHashMap<>(coefficients.length);
        for (int i = 0; i < coefficients.length; i++) {
            map.adjustOrPutValue(variables[i], coefficients[i], coefficients[i]);
        }
        TIntList cs = new TIntArrayList(coefficients.length);
        List<Variable> vs = new ArrayList<>(variables.length);

        TObjectIntIterator<Variable> iter = map.iterator();
        for (int i = map.size(); i-- > 0;) {
            iter.advance();
            if (iter.value() == 0) {
                iter.remove();
            } else {
                cs.add(iter.value());
                vs.add(iter.key());
            }
        }

        this.coefficients = cs.toArray();
        this.variables = vs.toArray(new Variable[vs.size()]);
        this.constant = constant;
        this.coefficientMap = map;
    }

    public int getCoefficient(Variable variable) {
        if (!coefficientMap.containsKey(variable)) {
            throw new IllegalArgumentException();
        }
        return coefficientMap.get(variable);
    }

    public int[] getCoefficients() {
        return coefficients;
    }

    public Variable[] getVariables() {
        return variables;
    }

    public boolean hasConstant() {
        return constant != 0;
    }

    public int getConstant() {
        return constant;
    }

    public int getLowBound() {
        int low = constant;
        for (int i = 0; i < coefficients.length; i++) {
            low += coefficients[i] * (coefficients[i] > 0
                    ? variables[i].getLowBound()
                    : variables[i].getHighBound());
        }
        return low;
    }

    public int getHighBound() {
        int high = constant;
        for (int i = 0; i < coefficients.length; i++) {
            high += coefficients[i] * (coefficients[i] > 0
                    ? variables[i].getHighBound()
                    : variables[i].getLowBound());
        }
        return high;
    }

    public LinearFunction add(LinearFunction addend) {
        int[] newCoefficients = Arrays.copyOf(coefficients, coefficients.length + addend.coefficients.length);
        System.arraycopy(addend.coefficients, 0, newCoefficients, coefficients.length, addend.coefficients.length);
        Variable[] newVariables = Arrays.copyOf(variables, variables.length + addend.variables.length);
        System.arraycopy(addend.variables, 0, newVariables, variables.length, addend.variables.length);
        int newConstant = constant + addend.constant;
        return new LinearFunction(newCoefficients, newVariables, newConstant);
    }

    public LinearFunction add(int addend) {
        if (addend == 0) {
            return this;
        }
        return new LinearFunction(coefficients, variables, constant + addend);
    }

    public LinearFunction sub(LinearFunction subtrahend) {
        int[] newCoefficients = Arrays.copyOf(coefficients, coefficients.length + subtrahend.coefficients.length);
        for (int i = 0; i < subtrahend.coefficients.length; i++) {
            newCoefficients[i + coefficients.length] = -subtrahend.coefficients[i];
        }
        Variable[] newVariables = Arrays.copyOf(variables, variables.length + subtrahend.variables.length);
        System.arraycopy(subtrahend.variables, 0, newVariables, variables.length, subtrahend.variables.length);
        int newConstant = constant - subtrahend.constant;
        return new LinearFunction(newCoefficients, newVariables, newConstant);
    }

    public LinearFunction sub(int subtrahend) {
        if (subtrahend == 0) {
            return this;
        }
        return new LinearFunction(coefficients, variables, constant - subtrahend);
    }

    public LinearFunction scale(int scale) {
        int[] newCoefficients = new int[coefficients.length];
        for (int i = 0; i < newCoefficients.length; i++) {
            newCoefficients[i] = scale * coefficients[i];
        }
        return new LinearFunction(newCoefficients, variables, scale * constant);
    }

    public LinearFunction replace(Variable variable, LinearFunction value) {
        TIntArrayList newCoefficients = new TIntArrayList(coefficients.length + value.coefficients.length);
        List<Variable> newVariables = new ArrayList<>(newCoefficients.size());
        int newConstant = constant;
        for (int i = 0; i < coefficients.length; i++) {
            if (variable.equals(variables[i])) {
                for (int j = 0; j < value.coefficients.length; j++) {
                    newCoefficients.add(coefficients[i] * value.coefficients[j]);
                    newVariables.add(value.variables[i]);
                }
                newConstant += coefficients[i] * value.constant;
            } else {
                newCoefficients.add(coefficients[i]);
                newVariables.add(variables[i]);
            }
        }
        return new LinearFunction(
                newCoefficients.toArray(),
                newVariables.toArray(new Variable[newVariables.size()]),
                newConstant);
    }

    @Override
    public LinearFunction toFunction() {
        return this;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == this) {
            return true;
        }
        if (obj instanceof LinearFunction) {
            LinearFunction other = (LinearFunction) obj;
            return coefficientMap.equals(other.coefficientMap)
                    && constant == other.constant;
        }
        return false;
    }

    @Override
    public int hashCode() {
        return coefficientMap.hashCode() ^ constant;
    }

    @Override
    public String toString() {
        if (coefficients.length == 0) {
            return Integer.toString(constant);
        }
        StringBuilder result = new StringBuilder();
        for (int i = 0; i < coefficients.length; i++) {
            if (i > 0) {
                result.append(" + ");
            }
            result.append(coefficients[i] > 0 ? coefficients[i] : "(" + coefficients[i] + ")");
            result.append("*");
            result.append(variables[i]);
        }
        if (constant != 0) {
            result.append(" + ");
            result.append(constant);
        }
        return result.toString();
    }
}
