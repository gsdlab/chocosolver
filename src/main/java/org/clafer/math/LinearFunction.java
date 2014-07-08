package org.clafer.math;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import org.clafer.common.Check;

/**
 * @author jimmy
 */
public class LinearFunction implements LinearFunctionable {

    private final Rational[] coefficients;
    private final Variable[] variables;
    private final Rational constant;
    public final Map<Variable, Rational> coefficientMap;

    public LinearFunction(long coefficient, Variable variable) {
        this(new Rational(coefficient), variable);
    }

    public LinearFunction(Rational coefficient, Variable variable) {
        this(coefficient, variable, Rational.Zero);
    }

    public LinearFunction(long constant) {
        this(new Rational(constant));
    }

    public LinearFunction(Rational constant) {
        this.coefficients = new Rational[0];
        this.variables = new Variable[0];
        this.constant = Check.notNull(constant);
        this.coefficientMap = new HashMap<>(0);
    }

    public LinearFunction(long coefficient, Variable variable, long constant) {
        this(new Rational(coefficient), variable, new Rational(constant));
    }

    public LinearFunction(Rational coefficient, Variable variable, Rational constant) {
        this.coefficients = new Rational[]{Check.notNull(coefficient)};
        this.variables = new Variable[]{Check.notNull(variable)};
        this.constant = Check.notNull(constant);
        this.coefficientMap = new HashMap<>(1);
        this.coefficientMap.put(variable, coefficient);
    }

    public LinearFunction(long[] coefficients, Variable[] variables, long constant) {
        this(convert(coefficients), variables, new Rational(constant));
    }

    private static Rational[] convert(long[] is) {
        Rational[] rs = new Rational[is.length];
        for (int i = 0; i < rs.length; i++) {
            rs[i] = new Rational(is[i]);
        }
        return rs;
    }

    public LinearFunction(Rational[] coefficients, Variable[] variables, Rational constant) {
        Check.notNull(coefficients);
        Check.noNulls(variables);
        if (coefficients.length != variables.length) {
            throw new IllegalArgumentException();
        }
        Map<Variable, Rational> map = new HashMap<>(coefficients.length);
        for (int i = 0; i < coefficients.length; i++) {
            Rational old = map.put(variables[i], coefficients[i]);
            if (old != null) {
                map.put(variables[i], coefficients[i].add(old));
            }
        }
        List<Rational> cs = new ArrayList<>(coefficients.length);
        List<Variable> vs = new ArrayList<>(variables.length);

        Iterator<Entry<Variable, Rational>> iter = map.entrySet().iterator();
        while (iter.hasNext()) {
            Entry<Variable, Rational> entry = iter.next();
            if (entry.getValue().isZero()) {
                iter.remove();
            } else {
                cs.add(entry.getValue());
                vs.add(entry.getKey());
            }
        }

        this.coefficients = cs.toArray(new Rational[cs.size()]);
        this.variables = vs.toArray(new Variable[vs.size()]);
        this.constant = constant;
        this.coefficientMap = map;
    }

    public Rational getCoefficient(Variable variable) {
        return coefficientMap.get(variable);
    }

    public Rational[] getCoefficients() {
        return coefficients;
    }

    public Variable[] getVariables() {
        return variables;
    }

    public boolean hasConstant() {
        return !constant.isZero();
    }

    public Rational getConstant() {
        return constant;
    }

    public Rational getLowBound() {
        Rational low = constant;
        for (int i = 0; i < coefficients.length; i++) {
            low = low.add(coefficients[i].mul(coefficients[i].isPositive()
                    ? variables[i].getLowBound()
                    : variables[i].getHighBound()));
        }
        return low;
    }

    public Rational getHighBound() {
        Rational high = constant;
        for (int i = 0; i < coefficients.length; i++) {
            high = high.add(coefficients[i].mul(coefficients[i].isPositive()
                    ? variables[i].getHighBound()
                    : variables[i].getLowBound()));
        }
        return high;
    }

    public LinearFunction minus() {
        Rational[] newCoefficients = new Rational[coefficients.length];
        for (int i = 0; i < newCoefficients.length; i++) {
            newCoefficients[i] = coefficients[i].minus();
        }
        return new LinearFunction(newCoefficients, variables, constant.minus());
    }

    public LinearFunction add(LinearFunction addend) {
        Rational[] newCoefficients = Arrays.copyOf(coefficients, coefficients.length + addend.coefficients.length);
        System.arraycopy(addend.coefficients, 0, newCoefficients, coefficients.length, addend.coefficients.length);
        Variable[] newVariables = Arrays.copyOf(variables, variables.length + addend.variables.length);
        System.arraycopy(addend.variables, 0, newVariables, variables.length, addend.variables.length);
        Rational newConstant = constant.add(addend.constant);
        return new LinearFunction(newCoefficients, newVariables, newConstant);
    }

    public LinearFunction add(long addend) {
        if (addend == 0) {
            return this;
        }
        return add(new Rational(addend));
    }

    public LinearFunction add(Rational addend) {
        if (addend.isZero()) {
            return this;
        }
        return new LinearFunction(coefficients, variables, constant.add(addend));
    }

    public LinearFunction sub(LinearFunction subtrahend) {
        Rational[] newCoefficients = Arrays.copyOf(coefficients, coefficients.length + subtrahend.coefficients.length);
        for (int i = 0; i < subtrahend.coefficients.length; i++) {
            newCoefficients[i + coefficients.length] = subtrahend.coefficients[i].minus();
        }
        Variable[] newVariables = Arrays.copyOf(variables, variables.length + subtrahend.variables.length);
        System.arraycopy(subtrahend.variables, 0, newVariables, variables.length, subtrahend.variables.length);
        Rational newConstant = constant.sub(subtrahend.constant);
        return new LinearFunction(newCoefficients, newVariables, newConstant);
    }

    public LinearFunction sub(long subtrahend) {
        if (subtrahend == 0) {
            return this;
        }
        return sub(new Rational(subtrahend));
    }

    public LinearFunction sub(Rational subtrahend) {
        if (subtrahend.isZero()) {
            return this;
        }
        return new LinearFunction(coefficients, variables, constant.sub(subtrahend));
    }

    public LinearFunction mul(long multiplier) {
        if (multiplier == 1) {
            return this;
        }
        return mul(new Rational(multiplier));
    }

    public LinearFunction mul(Rational multiplier) {
        if (multiplier.isOne()) {
            return this;
        }
        Rational[] newCoefficients = new Rational[coefficients.length];
        for (int i = 0; i < newCoefficients.length; i++) {
            newCoefficients[i] = coefficients[i].mul(multiplier);
        }
        return new LinearFunction(newCoefficients, variables, constant.mul(multiplier));
    }

    public LinearFunction div(long divisor) {
        if (divisor == 0) {
            throw new ArithmeticException("/ by zero");
        } else if (divisor == 1) {
            return this;
        }
        return div(new Rational(divisor));
    }

    public LinearFunction div(Rational divisor) {
        if (divisor.isZero()) {
            throw new ArithmeticException("/ by zero");
        } else if (divisor.isOne()) {
            return this;
        }
        Rational[] newCoefficients = new Rational[coefficients.length];
        for (int i = 0; i < newCoefficients.length; i++) {
            newCoefficients[i] = coefficients[i].div(divisor);
        }
        return new LinearFunction(newCoefficients, variables, constant.div(divisor));
    }

    public LinearFunction replace(Variable variable, LinearFunction value) {
        if (!coefficientMap.containsKey(variable)) {
            return this;
        }
        List<Rational> newCoefficients = new ArrayList<>(coefficients.length + value.coefficients.length);
        List<Variable> newVariables = new ArrayList<>(newCoefficients.size());
        Rational newConstant = constant;
        for (int i = 0; i < coefficients.length; i++) {
            if (variable.equals(variables[i])) {
                for (int j = 0; j < value.coefficients.length; j++) {
                    newCoefficients.add(coefficients[i].mul(value.coefficients[j]));
                    newVariables.add(value.variables[j]);
                }
                newConstant = newConstant.add(coefficients[i].mul(value.constant));
            } else {
                newCoefficients.add(coefficients[i]);
                newVariables.add(variables[i]);
            }
        }
        return new LinearFunction(
                newCoefficients.toArray(new Rational[newCoefficients.size()]),
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
                    && constant.equals(other.constant);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return coefficientMap.hashCode() ^ constant.hashCode();
    }

    @Override
    public String toString() {
        if (coefficients.length == 0) {
            return constant.toString();
        }
        StringBuilder result = new StringBuilder();
        for (int i = 0; i < coefficients.length; i++) {
            if (i > 0) {
                result.append(" + ");
            }
            result.append(coefficients[i].isPositive() ? coefficients[i] : "(" + coefficients[i] + ")");
            result.append("*");
            result.append(variables[i]);
        }
        if (!constant.isZero()) {
            result.append(" + ");
            result.append(constant);
        }
        return result.toString();
    }
}
