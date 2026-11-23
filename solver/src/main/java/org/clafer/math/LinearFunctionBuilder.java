package org.clafer.math;

import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author jimmy
 */
public class LinearFunctionBuilder implements LinearFunctionable {

    private final List<Rational> coefficients = new ArrayList<>();
    private final List<Variable> variables = new ArrayList<>();
    private Rational constant = Rational.Zero;

    public static LinearFunctionBuilder term(Variable variable) {
        return term(Rational.One, variable);
    }

    public static LinearFunctionBuilder term(Rational coefficient, Variable variable) {
        return new LinearFunctionBuilder().plusTerm(coefficient, variable);
    }

    public static LinearFunctionBuilder constant(long c) {
        return constant(new Rational(c));
    }

    public static LinearFunctionBuilder constant(Rational c) {
        return new LinearFunctionBuilder().plusConstant(c);
    }

    public static LinearFunctionBuilder function(LinearFunction function) {
        return new LinearFunctionBuilder().plusFunction(function);
    }

    public LinearFunctionBuilder plusTerm(Variable variable) {
        return plusTerm(Rational.One, variable);
    }

    public LinearFunctionBuilder plusTerm(Rational coefficient, Variable variable) {
        coefficients.add(coefficient);
        variables.add(variable);
        return this;
    }

    public LinearFunctionBuilder plusConstant(long constant) {
        this.constant = this.constant.add(constant);
        return this;
    }

    public LinearFunctionBuilder plusConstant(Rational constant) {
        this.constant = this.constant.add(constant);
        return this;
    }

    public LinearFunctionBuilder plusFunction(LinearFunction function) {
        Rational[] cs = function.getCoefficients();
        Variable[] vs = function.getVariables();
        for (int i = 0; i < cs.length; i++) {
            plusTerm(cs[i], vs[i]);
        }
        plusConstant(function.getConstant());
        return this;
    }

    @Override
    public LinearFunction toFunction() {
        return new LinearFunction(
                coefficients.toArray(new Rational[coefficients.size()]),
                variables.toArray(new Variable[variables.size()]),
                constant);
    }
}
