package org.clafer.math;

import gnu.trove.iterator.TIntObjectIterator;
import gnu.trove.list.TIntList;
import gnu.trove.list.array.TIntArrayList;
import gnu.trove.map.TIntObjectMap;
import gnu.trove.map.TObjectIntMap;
import gnu.trove.map.hash.TObjectIntHashMap;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import org.clafer.collection.Pair;
import org.clafer.common.Check;
import org.clafer.math.LinearEquation.Op;
import static org.clafer.math.LinearEquation.*;
import static org.clafer.math.LinearEquation.Op.*;
import static org.clafer.math.LinearFunctionBuilder.constant;
import static org.clafer.math.LinearFunctionBuilder.term;
import util.tools.MathUtils;

/**
 * @author jimmy
 */
public class LinearSystem {

    private final LinearEquation[] equations;

    public LinearSystem(Collection<LinearEquation> equations) {
        this(equations.toArray(new LinearEquation[equations.size()]));
    }

    public LinearSystem(LinearEquation... equations) {
        this.equations = Check.noNullsNotEmpty(equations);
    }

    public LinearSystem gaussJordanElimination() {
        Set<Variable> variables = new HashSet<>();
        int numSlack = 0;
        for (LinearEquation equation : equations) {
            if (equation.getOp().equals(Op.LessThanEqual)) {
                numSlack++;
            }
            variables.addAll(Arrays.asList(equation.getLeft().getVariables()));
        }
        TObjectIntMap<Variable> variableIds = new TObjectIntHashMap<>();
        int id = numSlack;
        Variable[] variableOrder = variables.toArray(new Variable[variables.size()]);
        Arrays.sort(variableOrder, VariableNameComparator.Singleton);

        for (Variable variable : variableOrder) {
            variableIds.put(variable, id++);
        }
        int slack = 0;
        Rational[][] rows = new Rational[equations.length][id];
        Rational[] column = new Rational[equations.length];

        for (int i = 0; i < rows.length; i++) {
            LinearEquation equation = equations[i];
            Rational[] row = rows[i];
            Arrays.fill(row, Rational.Zero);
            if (equation.getOp().equals(Op.LessThanEqual)) {
                row[slack++] = Rational.One;
            }
            LinearFunction function = equation.getLeft();
            for (int j = 0; j < function.getCoefficients().length; j++) {
                int coeffient = function.getCoefficients()[j];
                int variable = variableIds.get(function.getVariables()[j]);
                assert row[variable].isZero();
                row[variable] = new Rational(coeffient);
            }
            assert column[i].isZero();
            column[i] = new Rational(equation.getRight());
        }

        Matrix a = new Matrix(rows);
        Matrix b = new Matrix(column);
        Matrix p = a.addColumns(Matrix.identity(a.numberOfRows())).gaussJordanElimination()
                .subColumns(a.numberOfColumns());

        Set<LinearEquation> optimize = new LinkedHashSet<>(equations.length * 2);
        optimize.addAll(Arrays.asList(equations));
        for (TIntObjectMap<Rational> row : p.multiply(a.addColumns(b)).getRows()) {
            LinearEquation equation = toEquation(row, id + 1, variableOrder);
            switch (equation.isEntailed()) {
                case TrueFalseDomain:
                    optimize.add(equation);
                    break;
                case FalseDomain:
                // TODO
            }
        }
        return new LinearSystem(optimize);
    }

    private static LinearEquation toEquation(TIntObjectMap<Rational> row, int columns, Variable[] variables) {
        int slacks = columns - 1 - variables.length;
        boolean hasSlack = false;
        TIntList cs = new TIntArrayList(row.size() - 1);
        List<Variable> vs = new ArrayList<>(row.size() - 1);

        int right = 0;
        TIntObjectIterator<Rational> iter = row.iterator();
        for (int i = row.size(); i-- > 0;) {
            iter.advance();
            int column = iter.key();
            Rational r = iter.value();
            if (column == columns - 1) {
                if (r.isWhole()) {
                    right = (int) r.getNumerator();
                } else {
                    throw new UnsupportedOperationException("TODO");
                }
            } else if (column < slacks) {
                assert !hasSlack;
                assert r.isOne();
                hasSlack = true;
            } else {
                if (r.isWhole()) {
                    cs.add((int) r.getNumerator());
                    vs.add(variables[column - slacks]);
                } else {
                    throw new UnsupportedOperationException("TODO");
                }
            }
        }
        LinearFunction left = new LinearFunction(cs.toArray(), vs.toArray(new Variable[vs.size()]), 0);
        return new LinearEquation(left, hasSlack ? LessThanEqual : Equal, right);
    }

    private void replaceEquation(
            LinearEquation oldEquation,
            LinearEquation newEquation,
            Map<Variable, List<LinearEquation>> positiveOccurences,
            Map<Variable, List<LinearEquation>> negativeOccurences) {
        if (oldEquation != null) {
            LinearFunction left = oldEquation.getLeft();
            int[] coefficients = left.getCoefficients();
            Variable[] variables = left.getVariables();
            for (int i = 0; i < coefficients.length; i++) {
                assert coefficients[i] != 0;
                Map<Variable, List<LinearEquation>> map
                        = coefficients[i] > 0 ? positiveOccurences : negativeOccurences;
                map.get(variables[i]).remove(oldEquation);
            }
        }
        LinearFunction left = newEquation.getLeft();
        int[] coefficients = left.getCoefficients();
        Variable[] variables = left.getVariables();
        for (int i = 0; i < coefficients.length; i++) {
            assert coefficients[i] != 0;
            Map<Variable, List<LinearEquation>> map
                    = coefficients[i] > 0 ? positiveOccurences : negativeOccurences;
            List<LinearEquation> list = map.get(variables[i]);
            if (list == null) {
                list = new ArrayList<>();
                map.put(variables[i], list);
            }
            list.add(newEquation);
        }
    }

    private boolean cost(LinearEquation equation,
            Map<Variable, Pair<LinearEquation, Integer>> bestLowBound,
            Map<Variable, Pair<LinearEquation, Integer>> bestHighBound,
            Map<Variable, List<LinearEquation>> positiveOccurences,
            Map<Variable, List<LinearEquation>> negativeOccurences) {
        LinearFunction left = equation.getLeft();
        int[] coefficients = left.getCoefficients();
        Variable[] variables = left.getVariables();
        assert left.getConstant() == 0;
        int right = equation.getRight();

        int low = left.getLowBound();
        assert LessThanEqual.equals(equation.getOp());
        boolean changed = false;
        for (int i = 0; i < coefficients.length; i++) {
            int ai = coefficients[i];
            Variable vi = variables[i];
            assert ai != 0;
            if (ai > 0) {
                int hb = MathUtils.divCeil(right - (low - ai * vi.getLowBound()), ai);
                Pair<LinearEquation, Integer> best = bestHighBound.get(vi);
                if (best == null || best.getSnd() > hb) {
                    bestHighBound.put(vi, new Pair<>(equation, hb));
                    replaceEquation(best == null ? null : best.getFst(),
                            equation, positiveOccurences, negativeOccurences);
                    changed = true;
                }
            } else {
                int lb = MathUtils.divFloor(right - (low - ai * vi.getHighBound()), ai);
                Pair<LinearEquation, Integer> best = bestLowBound.get(vi);
                if (best == null || best.getSnd() < lb) {
                    bestLowBound.put(vi, new Pair<>(equation, lb));
                    replaceEquation(best == null ? null : best.getFst(),
                            equation, positiveOccurences, negativeOccurences);
                    changed = true;
                }
            }
        }
        return changed;
    }

    public LinearSystem fourierMotzkinElimination() {
        // bestLowBound[v] returns the constraint that gives the highest low bound for v
        Map<Variable, Pair<LinearEquation, Integer>> bestLowBound = new HashMap<>();
        // bestLowBound[v] returns the constraint that gives the lowest high bound for v
        Map<Variable, Pair<LinearEquation, Integer>> bestHighBound = new HashMap<>();
        // positiveOccurences[v] returns the equations where v has a positive coefficient
        Map<Variable, List<LinearEquation>> positiveOccurences = new HashMap<>();
        // positiveOccurences[v] returns the equations where v has a negative coefficient
        Map<Variable, List<LinearEquation>> negativeOccurences = new HashMap<>();

        for (LinearEquation equation : equations) {
            switch (equation.getOp()) {
                case Equal:
                    LinearFunction left = equation.getLeft();
                    int right = equation.getRight();
                    cost(new LinearEquation(left, Op.LessThanEqual, right),
                            bestLowBound, bestHighBound, positiveOccurences, negativeOccurences);
                    cost(new LinearEquation(left.scale(-1), Op.LessThanEqual, -right),
                            bestLowBound, bestHighBound, positiveOccurences, negativeOccurences);
                    break;
                case LessThanEqual:
                    cost(equation, bestLowBound, bestHighBound, positiveOccurences, negativeOccurences);
                    break;
            }
        }

        boolean changed;
        do {
            changed = false;
            // Because we are iterating over the map and modifying it at the same time, we
            // need to copy it first.
            @SuppressWarnings("unchecked")
            Entry<Variable, List<LinearEquation>>[] entries
                    = (Entry<Variable, List<LinearEquation>>[]) new Entry<?, ?>[positiveOccurences.size()];
            entries = positiveOccurences.entrySet().toArray(entries);
            for (Entry<Variable, List<LinearEquation>> entry : entries) {
                Variable variable = entry.getKey();
                List<LinearEquation> positives = entry.getValue();
                if (!positives.isEmpty()) {
                    List<LinearEquation> negatives = negativeOccurences.get(variable);
                    if (negatives != null && !negatives.isEmpty()) {
                        // Remove duplicates
                        for (LinearEquation p : new HashSet<>(positives)) {
                            LinearFunction pLeft = p.getLeft();
                            int[] pCoefficients = pLeft.getCoefficients();
                            Variable[] pVariables = pLeft.getVariables();
                            int a0 = p.getRight();
                            int ak = pLeft.getCoefficient(variable);
                            // Remove duplicates
                            for (LinearEquation n : new HashSet<>(negatives)) {
                                LinearFunction nLeft = n.getLeft();
                                int[] nCoefficients = nLeft.getCoefficients();
                                Variable[] nVariables = nLeft.getVariables();
                                int b0 = n.getRight();
                                int bk = nLeft.getCoefficient(variable);

                                int[] newCoefficients = new int[pCoefficients.length + nCoefficients.length - 2];
                                Variable[] newVariables = new Variable[newCoefficients.length];

                                int z = 0;
                                for (int i = 0; i < pCoefficients.length; i++) {
                                    if (!variable.equals(pVariables[i])) {
                                        newCoefficients[z] = pCoefficients[i] * -bk;
                                        newVariables[z] = pVariables[i];
                                        z++;
                                    }
                                }
                                for (int i = 0; i < nCoefficients.length; i++) {
                                    if (!variable.equals(nVariables[i])) {
                                        newCoefficients[z] = nCoefficients[i] * ak;
                                        newVariables[z] = nVariables[i];
                                        z++;
                                    }
                                }
                                assert z == newCoefficients.length;
                                assert z == newVariables.length;

                                LinearEquation newEquation = new LinearEquation(
                                        new LinearFunction(newCoefficients, newVariables, 0),
                                        LessThanEqual,
                                        a0 * -bk + b0 * ak);
                                switch (newEquation.isEntailed()) {
                                    case TrueFalseDomain:
                                        changed |= cost(newEquation, bestLowBound, bestHighBound, positiveOccurences, negativeOccurences);
                                        break;
                                    case FalseDomain:
                                    // TODO
                                }
                            }
                        }
                    }
                }
            }
        } while (changed);

        Set<LinearEquation> newEquations = new LinkedHashSet<>(
                bestLowBound.size() + bestHighBound.size() + equations.length);
        newEquations.addAll(Arrays.asList(equations));
        for (Pair<LinearEquation, Integer> b : bestLowBound.values()) {
            newEquations.add(b.getFst());
        }
        for (Pair<LinearEquation, Integer> b : bestHighBound.values()) {
            newEquations.add(b.getFst());
        }
        return new LinearSystem(newEquations);
    }

    public LinearSystem dominantElimination() {
        Set<LinearEquation> optimize = new LinkedHashSet<>(Arrays.asList(equations));
        for (LinearEquation equation : equations) {
            if (Equal.equals(equation.getOp())) {
                optimize.remove(new LinearEquation(equation.getLeft(), LessThanEqual, equation.getRight()));
                optimize.remove(new LinearEquation(equation.getLeft().scale(-1), LessThanEqual, -equation.getRight()));
            }
        }
        return optimize.size() == equations.length ? this : new LinearSystem(optimize);
    }

    @Override
    public String toString() {
        StringBuilder result = new StringBuilder();
        for (LinearEquation equation : equations) {
            result.append(equation).append('\n');
        }
        return result.toString();
    }

    public static void main(String[] args) throws Exception {
//        Solver solver = new Solver();
//        IntVar a = VF.enumerated("a", 0, 4, solver);
//        IntVar b = VF.enumerated("b", 0, 4, solver);
//        IntVar c = VF.enumerated("c", 0, 4, solver);
//        IntVar d = VF.enumerated("d", 0, 4, solver);
//        IntVar z = VF.enumerated("z", 0, 42, solver);
//        solver.post(ICF.sum(new IntVar[]{c, d}, "<=", VF.offset(z, 4)));
//        solver.post(ICF.arithm(c, "+", d, ">=", 4));
//        solver.post(ICF.arithm(z, ">=", 8));
//        solver.post(ICF.sum(new IntVar[]{a, b, c, d}, z));
//
//        solver.propagate();
//        System.out.println(solver);
//        if (solver.findSolution()) {
//            do {
//                System.out.println(a.getValue() + " : " + b.getValue());
//            } while (solver.nextSolution());
//        }
//        ::(-1)*z + 1 * d + 1 * c <= -4
//(-1)*d + (-1) * c <= -4
//(-1)*z <= -8
//(-1)*z + 1 * d + 1 * c + 1 * b + 1 * a = 0
        Variable a = new Variable("a", 0, 4);
        Variable b = new Variable("b", 0, 4);
        Variable c = new Variable("c", 0, 4);
        Variable d = new Variable("d", 0, 4);
        Variable e = new Variable("e", 0, 4);
        Variable f = new Variable("f", 0, 4);
        Variable g = new Variable("g", 0, 4);
        Variable h = new Variable("h", 0, 4);
        Variable z = new Variable("z", 0, 42);

        LinearSystem ls = new LinearSystem(
                greaterThanEqual(term(a).plusTerm(b), constant(4)),
                greaterThanEqual(term(c).plusTerm(d), constant(4)),
                greaterThanEqual(term(e).plusTerm(f), constant(4)),
                greaterThanEqual(term(g).plusTerm(h), constant(4)),
                equal(term(a).plusTerm(b).plusTerm(c).plusTerm(d).plusTerm(e).plusTerm(f).plusTerm(g).plusTerm(h), term(z))
        );

        ls = ls.fourierMotzkinElimination();
        System.out.println(ls);
        ls = ls.gaussJordanElimination();
        System.out.println("::" + ls);
        ls = ls.dominantElimination();
        System.out.println(ls);
    }
}
