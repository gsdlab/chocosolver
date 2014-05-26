package org.clafer.math;

import gnu.trove.iterator.TIntObjectIterator;
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
import org.clafer.common.Util;
import org.clafer.math.LinearEquation.Op;
import static org.clafer.math.LinearEquation.*;
import static org.clafer.math.LinearEquation.Op.*;
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

    public LinearEquation[] getEquations() {
        return equations;
    }

    /**
     * Create redundant constraints based on Gauss-Jordan elimination.
     * "Propagating systems of dense linear integer constraints" by Thibaut
     * Feydy and Peter J. Stuckey.
     *
     * @return a linear system with redundant constraints
     */
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
            column[i] = new Rational(equation.getRight());
        }

        Matrix a = new Matrix(rows);
        Matrix b = new Matrix(column);
        Matrix p = a.addColumns(Matrix.identity(a.numberOfRows())).gaussJordanElimination()
                .subColumns(a.numberOfColumns());

        Set<LinearEquation> optimize = new LinkedHashSet<>(equations.length * 2);
        optimize.addAll(Arrays.asList(equations));
        for (TIntObjectMap<Rational> row : p.multiply(a.addColumns(b)).getRows()) {
            for (LinearEquation equation : toEquation(row, id + 1, variableOrder)) {
                switch (equation.isEntailed()) {
                    case TrueFalseDomain:
                        optimize.add(equation);
                        break;
                    case FalseDomain:
                    // TODO
                }
            }
        }
        return new LinearSystem(optimize);
    }

    private static LinearEquation[] toEquation(TIntObjectMap<Rational> row, int columns, Variable[] variables) {
        int slacks = columns - 1 - variables.length;
        boolean hasSlack = false;
        List<Rational> cs = new ArrayList<>(row.size() - 1);
        List<Variable> vs = new ArrayList<>(row.size() - 1);

        boolean allWhole = true;
        Rational right = Rational.Zero;
        TIntObjectIterator<Rational> iter = row.iterator();
        for (int i = row.size(); i-- > 0;) {
            iter.advance();
            int column = iter.key();
            Rational r = iter.value();
            if (column == columns - 1) {
                allWhole &= r.isWhole();
                right = r;
            } else if (column < slacks) {
                assert !hasSlack;
                assert r.isOne();
                hasSlack = true;
            } else {
                allWhole &= r.isWhole();
                cs.add(r);
                vs.add(variables[column - slacks]);
            }
        }
        if (allWhole) {
            int[] ics = new int[cs.size()];
            for (int i = 0; i < ics.length; i++) {
                assert cs.get(i).getDenominator() == 1;
                ics[i] = (int) cs.get(i).getNumerator();
            }
            assert right.getDenominator() == 1;
            LinearFunction left = new LinearFunction(ics, vs.toArray(new Variable[vs.size()]), 0);
            return new LinearEquation[]{new LinearEquation(left, hasSlack ? LessThanEqual : Equal, (int) right.getNumerator())};
        }
        Rational min = right;
        Rational max = right;
        long lcm = right.getDenominator();
        for (Rational c : cs) {
            if (min.compareTo(c) > 0) {
                min = c;
            }
            if (max.compareTo(c) < 0) {
                max = c;
            }
            lcm = Util.lcm(lcm, c.getDenominator());
        }
        if (max.ceil() * lcm < 50000 && min.floor() * lcm > -50000) {
            int[] ics = new int[cs.size()];
            for (int i = 0; i < ics.length; i++) {
                Rational c = cs.get(i);
                ics[i] = (int) (c.getNumerator() * (lcm / c.getDenominator()));
            }
            int r = (int) (right.getNumerator() * (lcm / right.getDenominator()));
            LinearFunction left = new LinearFunction(ics, vs.toArray(new Variable[vs.size()]), 0);
            return new LinearEquation[]{new LinearEquation(left, hasSlack ? LessThanEqual : Equal, r)};
        }
        long multiplier = Math.min(Math.abs(50000 / max.ceil()), Math.abs(50000 / min.floor()));
        if (multiplier == 0) {
            multiplier = 1;
        }
        int[] lIcs = new int[cs.size()];
        int[] gIcs = new int[cs.size()];
        Rational lR = right.mul(multiplier);
        Rational gR = lR;
        for (int i = 0; i < lIcs.length; i++) {
            Rational c = cs.get(i).mul(multiplier);
            assert !c.isZero();
            if (c.isPositive()) {
                lIcs[i] = (int) c.floor();
                gIcs[i] = (int) -c.ceil();
                lR = lR.add(vs.get(i).getHighBound());
                gR = gR.sub(vs.get(i).getHighBound());
            } else {
                lIcs[i] = (int) c.ceil();
                gIcs[i] = (int) -c.floor();
                lR = lR.sub(vs.get(i).getLowBound());
                gR = gR.add(vs.get(i).getLowBound());
            }
        }
        LinearFunction lte = new LinearFunction(lIcs, vs.toArray(new Variable[vs.size()]), 0);
        LinearFunction gte = new LinearFunction(gIcs, vs.toArray(new Variable[vs.size()]), 0);
        return hasSlack
                ? new LinearEquation[]{new LinearEquation(lte, LessThanEqual, (int) lR.ceil())}
                : new LinearEquation[]{new LinearEquation(lte, LessThanEqual, (int) lR.ceil()),
                    new LinearEquation(gte, LessThanEqual, (int) -gR.floor())};
    }

    private void replaceEquation(
            LinearEquation oldEquation,
            LinearEquation newEquation,
            Map<Variable, TObjectIntMap<LinearEquation>> positiveOccurences,
            Map<Variable, TObjectIntMap<LinearEquation>> negativeOccurences) {
        if (oldEquation != null) {
            LinearFunction left = oldEquation.getLeft();
            int[] coefficients = left.getCoefficients();
            Variable[] variables = left.getVariables();
            for (int i = 0; i < coefficients.length; i++) {
                assert coefficients[i] != 0;
                Map<Variable, TObjectIntMap<LinearEquation>> map
                        = coefficients[i] > 0 ? positiveOccurences : negativeOccurences;
                TObjectIntMap<LinearEquation> references = map.get(variables[i]);
                assert references != null;
                if (references.adjustOrPutValue(oldEquation, -1, 0) == 0) {
                    references.remove(variables[i]);
                }
            }
        }
        LinearFunction left = newEquation.getLeft();
        int[] coefficients = left.getCoefficients();
        Variable[] variables = left.getVariables();
        for (int i = 0; i < coefficients.length; i++) {
            assert coefficients[i] != 0;
            Map<Variable, TObjectIntMap<LinearEquation>> map
                    = coefficients[i] > 0 ? positiveOccurences : negativeOccurences;
            TObjectIntMap<LinearEquation> references = map.get(variables[i]);
            if (references == null) {
                references = new TObjectIntHashMap<>();
                map.put(variables[i], references);
            }
            references.adjustOrPutValue(newEquation, 1, 1);
        }
    }

    private boolean cost(LinearEquation equation,
            Map<Variable, Pair<LinearEquation, Integer>> bestLowBound,
            Map<Variable, Pair<LinearEquation, Integer>> bestHighBound,
            Map<Variable, TObjectIntMap<LinearEquation>> positiveOccurences,
            Map<Variable, TObjectIntMap<LinearEquation>> negativeOccurences) {
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

    /**
     * Create redundant constraints based on Fourier-Motzkin elimination.
     * "Propagating systems of dense linear integer constraints" by Thibaut
     * Feydy and Peter J. Stuckey.
     *
     * @return a linear system with redundant constraints
     */
    public LinearSystem fourierMotzkinElimination() {
        // bestLowBound[v] returns the constraint that gives the highest low bound for v
        Map<Variable, Pair<LinearEquation, Integer>> bestLowBound = new HashMap<>();
        // bestLowBound[v] returns the constraint that gives the lowest high bound for v
        Map<Variable, Pair<LinearEquation, Integer>> bestHighBound = new HashMap<>();
        // positiveOccurences[v] returns the equations where v has a positive coefficient.
        // Each equation may occur multiple times, treated like a reference count.
        Map<Variable, TObjectIntMap<LinearEquation>> positiveOccurences = new HashMap<>();
        // positiveOccurences[v] returns the equations where v has a negative coefficient.
        // Each equation may occur multiple times, treated like a reference count.
        Map<Variable, TObjectIntMap<LinearEquation>> negativeOccurences = new HashMap<>();

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
            Entry<Variable, TObjectIntMap<LinearEquation>>[] entries
                    = (Entry<Variable, TObjectIntMap<LinearEquation>>[]) new Entry<?, ?>[positiveOccurences.size()];
            entries = positiveOccurences.entrySet().toArray(entries);
            for (Entry<Variable, TObjectIntMap<LinearEquation>> entry : entries) {
                Variable variable = entry.getKey();
                TObjectIntMap<LinearEquation> positives = entry.getValue();
                if (!positives.isEmpty()) {
                    TObjectIntMap<LinearEquation> negatives = negativeOccurences.get(variable);
                    if (negatives != null && !negatives.isEmpty()) {
                        // Need to copy the set to avoid concurrent modification exception.
                        for (LinearEquation p : positives.keySet().toArray(new LinearEquation[positives.size()])) {
                            LinearFunction pLeft = p.getLeft();
                            int[] pCoefficients = pLeft.getCoefficients();
                            Variable[] pVariables = pLeft.getVariables();
                            int a0 = p.getRight();
                            int ak = pLeft.getCoefficient(variable);
                            // Need to copy the set to avoid concurrent modification exception.
                            for (LinearEquation n : negatives.keySet().toArray(new LinearEquation[negatives.size()])) {
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

    /**
     * Remove some redundant constraints that are never useful.
     *
     * @return a linear system with less redundant constraints
     */
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
}
