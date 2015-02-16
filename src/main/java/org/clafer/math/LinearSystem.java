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
import java.util.Iterator;
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

/**
 * @author jimmy
 */
public class LinearSystem {

    private final LinearEquation[] equations;

    public LinearSystem(Collection<LinearEquation> equations) {
        this(equations.toArray(new LinearEquation[equations.size()]));
    }

    public LinearSystem(LinearEquation... equations) {
        this.equations = Check.noNulls(equations);
    }

    public LinearEquation[] getEquations() {
        return equations;
    }

    public LinearSystem addEquations(LinearEquation[] equations) {
        if (equations.length == 0) {
            return this;
        }
        Set<LinearEquation> newEquations = new LinkedHashSet<>(this.equations.length + equations.length);
        addAll(newEquations, this.equations);
        addAll(newEquations, equations);
        return new LinearSystem(newEquations);
    }

    public LinearSystem addEquations(Collection<LinearEquation> equations) {
        if (equations.isEmpty()) {
            return this;
        }
        Set<LinearEquation> newEquations = new LinkedHashSet<>(this.equations.length + equations.size());
        addAll(newEquations, this.equations);
        newEquations.addAll(equations);
        return new LinearSystem(newEquations);
    }

    /**
     * Create redundant constraints based on Gauss-Jordan elimination.
     * "Propagating systems of dense linear integer constraints" by Thibaut
     * Feydy and Peter J. Stuckey.
     *
     * @return a linear system with redundant constraints
     */
    public LinearSystem gaussJordanElimination() {
        if (equations.length == 0) {
            return this;
        }
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
                Rational coeffient = function.getCoefficients()[j];
                int variable = variableIds.get(function.getVariables()[j]);
                assert row[variable].isZero();
                row[variable] = coeffient;
            }
            column[i] = equation.getRight();
        }

        Matrix a = new Matrix(rows);
        Matrix b = new Matrix(column);
        Matrix p = a.addColumns(Matrix.identity(a.numberOfRows())).gaussJordanElimination()
                .subColumns(a.numberOfColumns());

        Set<LinearEquation> optimize = new LinkedHashSet<>(equations.length);
        for (TIntObjectMap<Rational> row : p.multiply(a.addColumns(b)).getRows()) {
            LinearEquation equation = toEquation(row, id + 1, variableOrder);
            if (equation != null) {
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

    private static LinearEquation toEquation(TIntObjectMap<Rational> row, int columns, Variable[] variables) {
        if (row.isEmpty()) {
            return null;
        }
        int slacks = columns - 1 - variables.length;
        boolean hasSlack = false;
        List<Rational> cs = new ArrayList<>(row.size() - 1);
        List<Variable> vs = new ArrayList<>(row.size() - 1);

        Rational right = Rational.Zero;
        TIntObjectIterator<Rational> iter = row.iterator();
        for (int i = row.size(); i-- > 0;) {
            iter.advance();
            int column = iter.key();
            Rational r = iter.value();
            if (column == columns - 1) {
                right = r;
            } else if (column < slacks) {
                assert !hasSlack;
                assert r.isOne();
                hasSlack = true;
            } else {
                cs.add(r);
                vs.add(variables[column - slacks]);
            }
        }
        LinearFunction left = new LinearFunction(
                cs.toArray(new Rational[cs.size()]),
                vs.toArray(new Variable[vs.size()]),
                Rational.Zero);
        return new LinearEquation(left, hasSlack ? LessThanEqual : Equal, right);
    }

    private static <E> boolean containsAny(Collection<E> set, E[] items) {
        for (E item : items) {
            if (set.contains(item)) {
                return true;
            }
        }
        return false;
    }

    private static <E> void addAll(Collection<E> set, E[] items) {
        for (E item : items) {
            set.add(item);
        }
    }

    public LinearSystem equalityElimination() {
        if (equations.length == 0) {
            return this;
        }
        List<LinearEquation> pool = new ArrayList<>(Arrays.asList(equations));

        HashSet<Variable> touched = new HashSet<>();
        do {
            touched.clear();

            List<Variable> assignmentKeys = new ArrayList<>();
            List<LinearFunction> assignmentValues = new ArrayList<>();
            Iterator<LinearEquation> iter = pool.iterator();
            while (iter.hasNext()) {
                LinearEquation equation = iter.next();
                if (containsAny(touched, equation.getVariables())) {
                    continue;
                }
                if (Equal.equals(equation.getOp()) && equation.getVariables().length <= 2) {
                    LinearFunction function = equation.getLeft();
                    // Find the variable with the coefficient closest to zero.
                    // Tie break 1: The variable with less occurrences.
                    // Tie break 2: The variable's name.
                    Rational min = function.getCoefficients()[0].abs();
                    Variable left = function.getVariables()[0];
                    for (int i = 1; i < function.getCoefficients().length; i++) {
                        Variable v1 = left;
                        Variable v2 = function.getVariables()[i];
                        Rational r = function.getCoefficients()[i].abs();
                        int c = min.compareTo(r);
                        if (c > 0) {
                            min = r;
                            left = v2;
                        } else if (c == 0 && v1.getName().compareTo(v2.getName()) > 0) {
                            min = r;
                            left = v2;
                        }
                    }
                    Rational coefficient = equation.getLeft().getCoefficient(left);
                    LinearFunction right = new LinearFunction(coefficient, left, equation.getRight()).sub(equation.getLeft()).div(coefficient);
                    assignmentKeys.add(left);
                    assignmentValues.add(right);
                    addAll(touched, equation.getVariables());
                    iter.remove();
                }
            }
            assert assignmentKeys.size() == assignmentValues.size();
            if (!assignmentKeys.isEmpty()) {
                for (int i = 0; i < pool.size(); i++) {
                    LinearEquation equation = pool.get(i);
                    for (int j = 0; j < assignmentKeys.size(); j++) {
                        equation = equation.replace(assignmentKeys.get(j), assignmentValues.get(j));
                    }
                    pool.set(i, equation);
                }
            }
        } while (!touched.isEmpty());

        List<LinearEquation> newEquations = new ArrayList<>(pool.size());
        for (LinearEquation equation : pool) {
            switch (equation.getOp()) {
                case Equal:
                    LinearFunction left = equation.getLeft();
                    Rational right = equation.getRight();
                    newEquations.add(new LinearEquation(left, LessThanEqual, right));
                    newEquations.add(new LinearEquation(left.minus(), LessThanEqual, right.minus()));
                    break;
                case LessThanEqual:
                    newEquations.add(equation);
                    break;
                default:
                    throw new IllegalStateException();
            }
        }
        return new LinearSystem(newEquations);
    }

    private void removeEquation(Set<LinearEquation> equations,
            Map<Variable, TObjectIntMap<LinearEquation>> positiveOccurrences,
            Map<Variable, TObjectIntMap<LinearEquation>> negativeOccurrences) {
        for (LinearEquation equation : equations) {
            LinearFunction left = equation.getLeft();
            Rational[] coefficients = left.getCoefficients();
            Variable[] variables = left.getVariables();
            for (int i = 0; i < coefficients.length; i++) {
                assert !coefficients[i].isZero();
                Map<Variable, TObjectIntMap<LinearEquation>> map
                        = coefficients[i].isPositive() ? positiveOccurrences : negativeOccurrences;
                TObjectIntMap<LinearEquation> references = map.get(variables[i]);
                assert references != null;
                if (references.adjustOrPutValue(equation, -1, 0) == 0) {
                    references.remove(variables[i]);
                }
            }
        }
    }

    private void addEquation(
            LinearEquation equation,
            Map<Variable, TObjectIntMap<LinearEquation>> positiveOccurrences,
            Map<Variable, TObjectIntMap<LinearEquation>> negativeOccurrences) {
        LinearFunction left = equation.getLeft();
        Rational[] coefficients = left.getCoefficients();
        Variable[] variables = left.getVariables();
        for (int i = 0; i < coefficients.length; i++) {
            assert !coefficients[i].isZero();
            Map<Variable, TObjectIntMap<LinearEquation>> map
                    = coefficients[i].isPositive() ? positiveOccurrences : negativeOccurrences;
            TObjectIntMap<LinearEquation> references = map.get(variables[i]);
            if (references == null) {
                references = new TObjectIntHashMap<>();
                map.put(variables[i], references);
            }
            references.adjustOrPutValue(equation, 1, 1);
        }
    }

    private static <T> HashSet<T> singletonHashSet(T t) {
        HashSet<T> set = new HashSet<>();
        set.add(t);
        return set;
    }

    private boolean cost(LinearEquation equation,
            Map<Variable, Pair<Set<LinearEquation>, Rational>> bestLowBound,
            Map<Variable, Pair<Set<LinearEquation>, Rational>> bestHighBound,
            Map<Variable, TObjectIntMap<LinearEquation>> positiveOccurrences,
            Map<Variable, TObjectIntMap<LinearEquation>> negativeOccurrences) {
        LinearFunction left = equation.getLeft();
        Rational[] coefficients = left.getCoefficients();
        Variable[] variables = left.getVariables();
        assert left.getConstant().isZero();
        Rational right = equation.getRight();

        Rational low = left.getLowBound();
        assert LessThanEqual.equals(equation.getOp());
        boolean changed = false;
        for (int i = 0; i < coefficients.length; i++) {
            Rational ai = coefficients[i];
            Variable vi = variables[i];
            assert !ai.isZero();
            if (ai.isPositive()) {
                Rational hb = right.sub(low.sub(ai.mul(vi.getLowBound()))).div(ai);
                Pair<Set<LinearEquation>, Rational> best = bestHighBound.get(vi);
                if (best == null || best.getSnd().compareTo(hb) > 0) {
                    bestHighBound.put(vi, new Pair<>(singletonHashSet(equation), hb));
                    if (best != null) {
                        removeEquation(best.getFst(), positiveOccurrences, negativeOccurrences);
                    }
                    addEquation(equation, positiveOccurrences, negativeOccurrences);
                    changed = true;
                } else if (best.getSnd().compareTo(hb) == 0) {
                    if (best.getFst().add(equation)) {
                        addEquation(equation, positiveOccurrences, negativeOccurrences);
                        changed = true;
                    }
                }
            } else {
                Rational lb = right.sub(low.sub(ai.mul(vi.getHighBound()))).div(ai);
                Pair<Set<LinearEquation>, Rational> best = bestLowBound.get(vi);
                if (best == null || best.getSnd().compareTo(lb) < 0) {
                    bestLowBound.put(vi, new Pair<>(singletonHashSet(equation), lb));
                    if (best != null) {
                        removeEquation(best.getFst(), positiveOccurrences, negativeOccurrences);
                    }
                    addEquation(equation, positiveOccurrences, negativeOccurrences);
                    changed = true;
                } else if (best.getSnd().compareTo(lb) == 0) {
                    if (best.getFst().add(equation)) {
                        addEquation(equation, positiveOccurrences, negativeOccurrences);
                        changed = true;
                    }
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
        if (equations.length == 0) {
            return this;
        }
        // bestLowBound[v] returns the constraint that gives the highest low bound for v
        Map<Variable, Pair<Set<LinearEquation>, Rational>> bestLowBound = new HashMap<>();
        // bestLowBound[v] returns the constraint that gives the lowest high bound for v
        Map<Variable, Pair<Set<LinearEquation>, Rational>> bestHighBound = new HashMap<>();
        // positiveOccurrences[v] returns the equations where v has a positive coefficient.
        // Each equation may occur multiple times, treated like a reference count.
        Map<Variable, TObjectIntMap<LinearEquation>> positiveOccurrences = new HashMap<>();
        // positiveOccurrences[v] returns the equations where v has a negative coefficient.
        // Each equation may occur multiple times, treated like a reference count.
        Map<Variable, TObjectIntMap<LinearEquation>> negativeOccurrences = new HashMap<>();

        for (LinearEquation equation : equations) {
            switch (equation.getOp()) {
                case Equal:
                    LinearFunction left = equation.getLeft();
                    Rational right = equation.getRight();
                    cost(new LinearEquation(left, Op.LessThanEqual, right),
                            bestLowBound, bestHighBound, positiveOccurrences, negativeOccurrences);
                    cost(new LinearEquation(left.minus(), Op.LessThanEqual, right.minus()),
                            bestLowBound, bestHighBound, positiveOccurrences, negativeOccurrences);
                    break;
                case LessThanEqual:
                    cost(equation, bestLowBound, bestHighBound, positiveOccurrences, negativeOccurrences);
                    break;
                default:
                    throw new IllegalStateException();
            }
        }

        boolean changed;
        do {
            changed = false;
            // Because we are iterating over the map and modifying it at the same time, we
            // need to copy it first.
            @SuppressWarnings("unchecked")
            Entry<Variable, TObjectIntMap<LinearEquation>>[] entries
                    = (Entry<Variable, TObjectIntMap<LinearEquation>>[]) new Entry<?, ?>[positiveOccurrences.size()];
            entries = positiveOccurrences.entrySet().toArray(entries);
            for (Entry<Variable, TObjectIntMap<LinearEquation>> entry : entries) {
                Variable variable = entry.getKey();
                TObjectIntMap<LinearEquation> positives = entry.getValue();
                if (!positives.isEmpty()) {
                    TObjectIntMap<LinearEquation> negatives = negativeOccurrences.get(variable);
                    if (negatives != null && !negatives.isEmpty()) {
                        // Need to copy the set to avoid concurrent modification exception.
                        for (LinearEquation p : positives.keySet().toArray(new LinearEquation[positives.size()])) {
                            LinearFunction pLeft = p.getLeft();
                            Rational[] pCoefficients = pLeft.getCoefficients();
                            Variable[] pVariables = pLeft.getVariables();
                            Rational a0 = p.getRight();
                            Rational ak = pLeft.getCoefficient(variable);
                            // Need to copy the set to avoid concurrent modification exception.
                            for (LinearEquation n : negatives.keySet().toArray(new LinearEquation[negatives.size()])) {
                                LinearFunction nLeft = n.getLeft();
                                Rational[] nCoefficients = nLeft.getCoefficients();
                                Variable[] nVariables = nLeft.getVariables();
                                Rational b0 = n.getRight();
                                Rational bk = nLeft.getCoefficient(variable);

                                Rational[] newCoefficients = new Rational[pCoefficients.length + nCoefficients.length - 2];
                                Variable[] newVariables = new Variable[newCoefficients.length];

                                int z = 0;
                                for (int i = 0; i < pCoefficients.length; i++) {
                                    if (!variable.equals(pVariables[i])) {
                                        newCoefficients[z] = pCoefficients[i].mul(bk.minus());
                                        newVariables[z] = pVariables[i];
                                        z++;
                                    }
                                }
                                for (int i = 0; i < nCoefficients.length; i++) {
                                    if (!variable.equals(nVariables[i])) {
                                        newCoefficients[z] = nCoefficients[i].mul(ak);
                                        newVariables[z] = nVariables[i];
                                        z++;
                                    }
                                }
                                assert z == newCoefficients.length;
                                assert z == newVariables.length;

                                LinearEquation newEquation = new LinearEquation(
                                        new LinearFunction(newCoefficients, newVariables, Rational.Zero),
                                        LessThanEqual,
                                        a0.mul(bk.minus()).add(b0.mul(ak)));
                                switch (newEquation.isEntailed()) {
                                    case TrueFalseDomain:
                                        changed |= cost(newEquation, bestLowBound, bestHighBound, positiveOccurrences, negativeOccurrences);
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
                bestLowBound.size() + bestHighBound.size());
        for (Pair<Set<LinearEquation>, ?> b : bestLowBound.values()) {
            newEquations.addAll(b.getFst());
        }
        for (Pair<Set<LinearEquation>, ?> b : bestHighBound.values()) {
            newEquations.addAll(b.getFst());
        }
        return new LinearSystem(newEquations);
    }

    /**
     * If a &le; b and -b &le; a, then replace with a = b.
     *
     * @return a linear system with fewer inequalities
     */
    public LinearSystem strengthenInequalities() {
        if (equations.length == 0) {
            return this;
        }
        Set<LinearEquation> newEquations = new LinkedHashSet<>(equations.length);
        for (LinearEquation equation : equations) {
            switch (equation.getOp()) {
                case LessThanEqual:
                    LinearFunction left = equation.getLeft();
                    Rational right = equation.getRight();
                    if (newEquations.remove(new LinearEquation(left.minus(), LessThanEqual, right.minus()))) {
                        newEquations.add(new LinearEquation(left, Equal, right));
                        break;
                    }
                // fallthrough
                case Equal:
                    newEquations.add(equation);
                    break;
                default:
                    throw new IllegalStateException();
            }
        }
        return new LinearSystem(newEquations);
    }

    /**
     * Remove some redundant constraints that are never useful.
     *
     * @return a linear system with less redundant constraints
     */
    public LinearSystem dominantElimination() {
        if (equations.length == 0) {
            return this;
        }
        Set<LinearEquation> optimize = new LinkedHashSet<>(Arrays.asList(equations));
        for (LinearEquation equation : equations) {
            if (Equal.equals(equation.getOp())) {
                optimize.remove(new LinearEquation(equation.getLeft(), LessThanEqual, equation.getRight()));
                optimize.remove(new LinearEquation(equation.getLeft().minus(), LessThanEqual, equation.getRight().minus()));
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
