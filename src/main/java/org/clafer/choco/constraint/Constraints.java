package org.clafer.choco.constraint;

import org.clafer.choco.constraint.propagator.PropTransitive;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import org.clafer.choco.constraint.propagator.PropAcyclic;
import org.clafer.choco.constraint.propagator.PropAnd;
import org.clafer.choco.constraint.propagator.PropArrayToSet;
import org.clafer.choco.constraint.propagator.PropArrayToSetCard;
import org.clafer.choco.constraint.propagator.PropAtMostTransitiveClosure;
import org.clafer.choco.constraint.propagator.PropContinuous;
import org.clafer.choco.constraint.propagator.PropContinuousUnion;
import org.clafer.choco.constraint.propagator.PropCountNotEqual;
import org.clafer.choco.constraint.propagator.PropEqualXY_Z;
import org.clafer.choco.constraint.propagator.PropFilterString;
import org.clafer.choco.constraint.propagator.PropIfThenElse;
import org.clafer.choco.constraint.propagator.PropIntChannel;
import org.clafer.choco.constraint.propagator.PropIntMemberNonemptySet;
import org.clafer.choco.constraint.propagator.PropJoinFunction;
import org.clafer.choco.constraint.propagator.PropJoinFunctionCard;
import org.clafer.choco.constraint.propagator.PropJoinInjectiveRelationCard;
import org.clafer.choco.constraint.propagator.PropJoinRelation;
import org.clafer.choco.constraint.propagator.PropLength;
import org.clafer.choco.constraint.propagator.PropLexChainChannel;
import org.clafer.choco.constraint.propagator.PropLone;
import org.clafer.choco.constraint.propagator.PropMask;
import org.clafer.choco.constraint.propagator.PropNotEqualXY_Z;
import org.clafer.choco.constraint.propagator.PropOne;
import org.clafer.choco.constraint.propagator.PropOr;
import org.clafer.choco.constraint.propagator.PropReflexive;
import org.clafer.choco.constraint.propagator.PropReifyEqualXY;
import org.clafer.choco.constraint.propagator.PropSamePrefix;
import org.clafer.choco.constraint.propagator.PropSelectN;
import org.clafer.choco.constraint.propagator.PropSetBounded;
import org.clafer.choco.constraint.propagator.PropSetDifference;
import org.clafer.choco.constraint.propagator.PropSetLowBound;
import org.clafer.choco.constraint.propagator.PropSetStrictHighBound;
import org.clafer.choco.constraint.propagator.PropSetMax;
import org.clafer.choco.constraint.propagator.PropSetMin;
import org.clafer.choco.constraint.propagator.PropSetNotEqualC;
import org.clafer.choco.constraint.propagator.PropSetSum;
import org.clafer.choco.constraint.propagator.PropSetUnion;
import org.clafer.choco.constraint.propagator.PropSetUnionCard;
import org.clafer.choco.constraint.propagator.PropSingleton;
import org.clafer.choco.constraint.propagator.PropTransitiveUnreachable;
import org.clafer.choco.constraint.propagator.PropUnreachable;
import org.clafer.common.Util;
import org.chocosolver.solver.Solver;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.constraints.ICF;
import org.chocosolver.solver.constraints.Propagator;
import org.chocosolver.solver.constraints.binary.PropEqualXY_C;
import org.chocosolver.solver.constraints.binary.PropEqualX_Y;
import org.chocosolver.solver.constraints.binary.PropEqualX_YC;
import org.chocosolver.solver.constraints.binary.PropGreaterOrEqualX_Y;
import org.chocosolver.solver.constraints.nary.element.PropElementV_fast;
import org.chocosolver.solver.constraints.nary.sum.PropSumEq;
import org.chocosolver.solver.constraints.set.PropIntersection;
import org.chocosolver.solver.constraints.set.PropSubsetEq;
import org.chocosolver.solver.constraints.unary.PropEqualXC;
import org.chocosolver.solver.constraints.unary.PropGreaterOrEqualXC;
import org.chocosolver.solver.constraints.unary.PropLessOrEqualXC;
import org.chocosolver.solver.variables.BoolVar;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.solver.variables.SetVar;
import org.chocosolver.solver.variables.VF;
import org.chocosolver.solver.variables.Variable;
import org.clafer.choco.constraint.propagator.PropReifyEqualXC;

/**
 * Custom Choco constraints. Designed for Clafer. Note that these constraints
 * requires that the envelope and kernel to be in sorted order from lowest to
 * highest, which is not explicitly enforced by Choco.
 *
 * @author jimmy
 */
public class Constraints {

    private static int varNum = 0;

    private Constraints() {
    }

    private static Optional<Propagator<IntVar>> eq(IntVar l, IntVar r) {
        if (l.isInstantiated()) {
            if (r.isInstantiatedTo(l.getValue())) {
                return Optional.empty();
            }
            return Optional.of(new PropEqualXC(r, l.getValue()));
        }
        if (r.isInstantiated()) {
            return Optional.of(new PropEqualXC(l, r.getValue()));
        }
        return Optional.of(new PropEqualX_Y(l, r));
    }

    private static Optional<Propagator<IntVar>> eq(IntVar l, int r) {
        if (l.isInstantiatedTo(r)) {
            return Optional.empty();
        }
        return Optional.of(new PropEqualXC(l, r));
    }

    private static Propagator<IntVar> lessThanEq(IntVar l, IntVar g) {
        if (l.isInstantiated()) {
            return new PropGreaterOrEqualXC(g, l.getValue());
        }
        if (g.isInstantiated()) {
            return new PropLessOrEqualXC(l, g.getValue());
        }
        return new PropGreaterOrEqualX_Y(new IntVar[]{g, l});
    }

    private static Propagator<IntVar> greaterThanEq(IntVar g, IntVar l) {
        return lessThanEq(l, g);
    }

    private static Propagator<IntVar> sumEq(IntVar[] ints, IntVar sum) {
        List<IntVar> filter = new ArrayList<>(ints.length);
        int constant = 0;
        for (IntVar var : ints) {
            if (var.isInstantiated()) {
                constant += var.getValue();
            } else {
                filter.add(var);
            }
        }
        IntVar[] filtered
                = filter.size() == ints.length
                        ? ints
                        : filter.toArray(new IntVar[filter.size()]);
        switch (filtered.length) {
            case 0:
                return new PropEqualXC(sum, constant);
            case 1:
                if (sum.isInstantiated()) {
                    return new PropEqualXC(filtered[0], sum.getValue() - constant);
                }
                return constant == 0
                        ? new PropEqualX_Y(filtered[0], sum)
                        : new PropEqualX_YC(new IntVar[]{filtered[0], sum}, -constant);
            case 2:
                if (sum.isInstantiated()) {
                    return new PropEqualXY_C(filtered, sum.getValue() - constant);
                }
            // fallthrough
            default:
                if (constant == 0) {
                    return new PropSumEq(filtered, sum);
                }
                return new PropSumEq(Util.cons(VF.fixed(constant, sum.getSolver()), filtered), sum);
        }
    }

    private static IntVar enumerated(String name, int lb, int ub, Solver solver) {
        return lb == ub
                ? VF.fixed(lb, solver)
                : VF.enumerated(name, lb, ub, solver);
    }

    /**
     *******************
     *
     * Boolean. When using boolean constraints, beware of the cases where the
     * documentation states that certain cardinality constraints are to be
     * enforced elsewhere. The only reason is for efficiency.
     *
     *******************
     */
    /**
     * A constraint enforcing
     * {@code operands[0] ∧ operands[1] ∧ ... ∧ operands[n]}.
     *
     * @param operands the operands
     * @return constraint {@code operands[0] ∧ operands[1] ∧ ... ∧ operands[n]}
     */
    public static Constraint and(BoolVar... operands) {
        return new Constraint("and", new PropAnd(operands));
    }

    /**
     * A constraint enforcing
     * {@code operands[0] + operands[1] + ... + operands[n] ≤ 1}.
     *
     * @param operands the operands
     * @return constraint
     * {@code operands[0] + operands[1] + ... + operands[n] ≤ 1}
     */
    public static Constraint lone(BoolVar... operands) {
        return new Constraint("lone", new PropLone(operands));
    }

    /**
     * A constraint enforcing
     * {@code operands[0] + operands[1] + ... + operands[n] = 1}.
     *
     * @param operands the operands
     * @return constraint
     * {@code operands[0] + operands[1] + ... + operands[n] = 1}
     */
    public static Constraint one(BoolVar... operands) {
        return new Constraint("one", new PropOne(operands));
    }

    /**
     * A constraint enforcing
     * {@code operands[0] ∨ operands[1] ∨ ... ∨ operands[n]}.
     *
     * @param operands the operands
     * @return constraint {@code operands[0] ∨ operands[1] ∨ ... ∨ operands[n]}
     */
    public static Constraint or(BoolVar... operands) {
        return new Constraint("or", new PropOr(operands));
    }

    /**
     * A constraint enforcing
     * {@code constraints[0] ∨ constraints[1] ∨ ... ∨ constraints[n]}. The
     * reason this constraint is useful is because it does not require
     * introducing new reified variables to the solver thus can be added
     * dynamically while the solver is in progress.
     *
     * @param constraints the constraints
     * @return constraint
     * {@code constraints[0] ∨ constraints[1] ∨ ... ∨ constraints[n]}
     */
    public static Constraint or(Constraint... constraints) {
        return new OrConstraint(constraints);
    }

    /**
     * A constraint enforcing
     * {@code antecedent => consequent && !antecedent => alternative}.
     *
     * @param antecedent the antecedent
     * @param consequent the consequent
     * @param alternative the alternative
     * @return constraint
     * {@code antecedent => consequent && !antecedent => alternative}
     */
    public static Constraint ifThenElse(final BoolVar antecedent, final BoolVar consequent, final BoolVar alternative) {
        return new Constraint("ifThenElse", new PropIfThenElse(antecedent, consequent, alternative)) {
            @Override
            public Constraint makeOpposite() {
                return ifThenElse(antecedent, consequent.not(), alternative.not());
            }
        };
    }

    /**
     * A constraint enforcing {@code reify <=> (variable = constant)}.
     *
     * @param reify the reified constraint
     * @param variable the variable
     * @param constant the constant
     * @return constraint {@code reify <=> (variable = constant)}
     */
    public static Constraint reifyEqual(BoolVar reify, IntVar variable, int constant) {
        return new ReifyEqualXC(reify, true, variable, constant);
    }

    /**
     * A constraint enforcing {@code reify <=> (v1 = v2)}.
     *
     * @param reify the reified constraint
     * @param v1 the first variable
     * @param v2 the second variable
     * @return constraint {@code reify <=> (v1 = v2)}
     */
    public static Constraint reifyEqual(BoolVar reify, IntVar v1, IntVar v2) {
        return new ReifyEqualXY(reify, true, v1, v2);
    }

    public static Constraint reifyEqual(IntVar reify, int value, IntVar v1, IntVar v2) {
        return new Constraint("reifyIntEqual", new PropReifyEqualXY(reify, value, v1, v2));
    }

    /**
     * A constraint enforcing {@code reify <=> (variable ≠ constant)}.
     *
     * @param reify the reified constraint
     * @param variable the variable
     * @param constant the constant
     * @return constraint {@code reify <=> (variable ≠ constant)}
     */
    public static Constraint reifyNotEqual(BoolVar reify, IntVar variable, int constant) {
        return new ReifyEqualXC(reify, false, variable, constant);
    }

    /**
     * A constraint enforcing {@code reify <=> (v1 ≠ v2)}.
     *
     * @param reify the reified constraint
     * @param v1 the first variable
     * @param v2 the second variable
     * @return constraint {@code reify <=> (v1 ≠ v2)}
     */
    public static Constraint reifyNotEqual(BoolVar reify, IntVar v1, IntVar v2) {
        return new ReifyEqualXY(reify, false, v1, v2);
    }

    /**
     * A constraint enforcing {@code count = (Σ_i array[i] != value)}.
     *
     * @param value the value
     * @param array the array
     * @param count the count
     * @return constraint {@code count = (Σ_i array[i] != value)}
     */
    public static Constraint countNotEqual(int value, IntVar[] array, IntVar count) {
        return new Constraint("countNotEqual", new PropCountNotEqual(value, array, count));
    }

    /**
     * A constraint enforcing {@code set1 = set2}. Does not enforce that
     * {@code set1Card = |set1Card|} nor {@code set2Card = |set2Card|} because
     * of how the compilation works, it is already enforced elsewhere.
     *
     * @param set1 the left set
     * @param set1Card the cardinality of {@code set1}
     * @param set2 the right set
     * @param set2Card the cardinality of {@code set2}
     * @return constraint {@code set1 = set2}
     */
    public static Constraint equal(SetVar set1, IntVar set1Card, SetVar set2, IntVar set2Card) {
        return new SetEquality(set1, set1Card, true, set2, set2Card);
    }

    /**
     * A constraint enforcing {@code set1 ≠ set2}.
     *
     * @param set1 the left set
     * @param set1Card the cardinality of {@code set1}
     * @param set2 the right set
     * @param set2Card the cardinality of {@code set2}
     * @return constraint {@code set1 ≠ set2}
     */
    public static Constraint notEqual(SetVar set1, IntVar set1Card, SetVar set2, IntVar set2Card) {
        return new SetEquality(set1, set1Card, false, set2, set2Card);
    }

    /**
     * A constraint enforcing {@code set ≠ {constant}}.
     *
     * @param set the set
     * @param constant the constant
     * @return constraint {@code set1 ≠ set2}
     */
    public static Constraint notEqual(SetVar set, int[] constant) {
        return new Constraint("notEqual", new PropSetNotEqualC(set, constant));
    }

    public static Constraint equalArcConsistent(final IntVar x, final IntVar y, final IntVar z) {
        return new Constraint("equalArcConsistent", new PropEqualXY_Z(x, y, z)) {
            @Override
            public Constraint makeOpposite() {
                return notEqualArcConsistent(x, y, z);
            }
        };
    }

    public static Constraint notEqualArcConsistent(final IntVar x, final IntVar y, final IntVar z) {
        return new Constraint("notEqualArcConsistent", new PropNotEqualXY_Z(x, y, z)) {
            @Override
            public Constraint makeOpposite() {
                return equalArcConsistent(x, y, z);
            }
        };
    }

    public static Constraint element(IntVar value, IntVar[] array, IntVar index, int offset) {
        return new Constraint("element", new org.clafer.choco.constraint.propagator.PropElement(value, array, index, offset));
    }

    /**
     * A constraint enforcing {@code element ∈ set}.
     *
     * @param element the element
     * @param set the set
     * @return constraint {@code element ∈ set}.
     */
    public static Constraint member(IntVar element, SetVar set) {
        return new SetMember(element, set);
    }

    public static Constraint memberNonempty(IntVar element, SetVar set, IntVar setCard) {
        return new Constraint("memberNonempty", new PropIntMemberNonemptySet(element, set, setCard));
    }

    /**
     * A constraint enforcing {@code element ∉ set}.
     *
     * @param element the element
     * @param set the set
     * @return constraint {@code element ∉ set}.
     */
    public static Constraint notMember(IntVar element, SetVar set) {
        return new SetNotMember(element, set);
    }

    /**
     * A constraint enforcing {@code sub ⊆ sup}. Does not enforce that
     * {@code subCard = |sub|} nor {@code supCard = |sup|} because of how the
     * compilation works, it is already enforced elsewhere.
     *
     * @param sub the subset
     * @param subCard the cardinality of {@code sub}
     * @param sup the superset
     * @param supCard the cardinality of {@code sup}
     * @return constraint {@code sub ⊆ sup}
     */
    public static Constraint subsetEq(SetVar sub, IntVar subCard, SetVar sup, IntVar supCard) {
        return new Constraint("subsetEq",
                new PropSubsetEq(sub, sup),
                // Simple cardinality propagation.
                lessThanEq(subCard, supCard));
    }

    /**
     * A constraint enforcing {@code x ∈ sets[i] <=> ints[x] = i}.
     *
     * @param sets the sets
     * @param ints the integers
     * @return constraint {@code x ∈ sets[i] <=> ints[x] = i}
     */
    public static Constraint intChannel(SetVar[] sets, IntVar[] ints) {
        /*
         * TODO: Take cardinalities of the sets into account?
         * For example if card(sets[0]) <= 3, then if at least 3 of the integers
         * are instantiated to 0, then remove 0 from the domains of the other integers.
         * If card(sets[0]) >= 2 and only 2 ints contain 0 in their domain, then
         * set those ints to 0.
         */
        return new Constraint("intChannel", new PropIntChannel(sets, ints));
    }

    /**
     * A constraint enforcing
     * {@code array(sets[0]) ++ array(sets[1]) ++ ... ++ array(sets[n]) ∈ N}
     * where {@code array} is the sorted array representation of the set,
     * {@code ++} is append, and {@code N = {[0,1,...,i] | i ≥ 0}}.
     *
     * @param sets the sets
     * @param setCards the cardinalities of {@code sets}
     * @return constraint
     * {@code array(sets[0]) ++ array(sets[1]) ++ ... ++ array(sets[n]) ∈ N}
     */
    public static Constraint sortedSets(SetVar[] sets, IntVar[] setCards) {
        if (sets.length != setCards.length) {
            throw new IllegalArgumentException();
        }
        Solver solver = sets[0].getSolver();
        int lb = 0;
        int ub = 0;
        for (int i = 0; i < sets.length; i++) {
            ub += setCards[i].getUB();
        }

        List<Propagator<?>> propagators = new ArrayList<>();

        IntVar[] boundary = new IntVar[sets.length + 1];
        boundary[0] = VF.zero(solver);
        for (int i = 0; i < sets.length; i++) {
            if (boundary[i].isInstantiatedTo(0)) {
                boundary[i + 1] = setCards[i];
            } else if (setCards[i].isInstantiatedTo(0)) {
                boundary[i + 1] = boundary[i];
            } else {
                boundary[i + 1] = enumerated(sets[i].getName() + "@Hb", lb, ub, solver);
                solver.post(equalArcConsistent(boundary[i], setCards[i], boundary[i + 1]));
            }
            propagators.add(new PropSetLowBound(sets[i], boundary[i]));
            propagators.add(new PropIntMemberNonemptySet(boundary[i], sets[i], setCards[i]));
            propagators.add(new PropSetStrictHighBound(sets[i], boundary[i + 1]));
            propagators.add(new PropSetBounded(boundary[i], boundary[i + 1], sets[i]));
        }
        for (int i = 0; i < sets.length; i++) {
            propagators.add(new PropContinuous(sets[i], setCards[i]));
        }
        propagators.add(new PropContinuousUnion(sets, boundary[boundary.length - 1]));

        return new Constraint("sortedSets", propagators.toArray(new Propagator[propagators.size()]));
    }

    /**
     * A constraint enforcing
     * {@code strings[i] ≤ strings[j] <=> ints[i] ≤ ints[j]} and
     * {@code strings[i] = strings[j] <=> ints[i] = ints[j]}.
     *
     * @param strings the strings
     * @param ints the integers
     * @return constraint {@code strings[i] ≤ strings[j] <=> ints[i] ≤ ints[j]}
     * and {@code strings[i] = strings[j] <=> ints[i] = ints[j]}
     */
    public static Constraint lexChainChannel(IntVar[][] strings, IntVar[] ints) {
        if (strings.length != ints.length) {
            throw new IllegalArgumentException();
        }

        return new Constraint("lexChainChannel", new PropLexChainChannel(strings, ints));
    }

    /**
     * A constraint enforcing that {@code bools[i] <=> i < n}.
     *
     * @param bools the booleans
     * @param n the number of true booleans
     * @return constraint {@code bools[i] <=> i < n}
     */
    public static Constraint selectN(BoolVar[] bools, IntVar n) {
        return new Constraint("selectN", new PropSelectN(bools, n));
    }

    /**
     * A constraint enforcing no cycles. {@code edges[i] = j} implies that there
     * is a directed edge from node i to node j. {@code edges[i] = edges.length}
     * implies that there are no direct edges from node i.
     *
     * @param edges the edges of the graph
     * @return constraint enforcing no cycles
     */
    public static Constraint acyclic(IntVar... edges) {
        return new Constraint("acyclic", new PropAcyclic(edges));
    }

    /**
     * A constraint enforcing no path from one node to another.
     * {@code edges[i] = j} implies that there is a directed edge from node i to
     * node j. {@code edges[i] ≥ edges.length} implies that there are no direct
     * edges from node i.
     *
     * @param edges the edges of the graph
     * @param from starting node
     * @param to destination node
     * @return constraint enforcing no path from one node to another
     */
    public static Constraint unreachable(IntVar[] edges, int from, int to) {
        return new Constraint("unreachable", new PropUnreachable(edges, from, to));
    }

    /**
     * A constraint enforcing
     * {@code result[i] = if i \u003c array(set).length then string[array(set)[i] - offset] else -1}
     * where {@code array} is the sorted array representation of the set. Does
     * not enforce that {@code setCard = |setCard|} because of how the
     * compilation works, it is already enforced elsewhere.
     *
     * @param set the set
     * @param setCard the cardinality of {@code set}
     * @param offset the offset
     * @param string the string
     * @param result the result
     * @return constraint
     * {@code result[i] = if i \u003c array(set).length then string[array(set)[i] - offset] else -1}
     */
    public static Constraint filterString(SetVar set, IntVar setCard, int offset, IntVar[] string, IntVar[] result) {
        return new Constraint("filterString", new PropFilterString(set, setCard, offset, string, result));
    }

    /**
     *******************
     *
     * Integer. When using integer constraints, beware of the cases where the
     * documentation states that certain cardinality constraints are to be
     * enforced elsewhere. The only reason is for efficiency.
     *
     *******************
     */
    /**
     * <p>
     * A constraint enforcing {@code Σ set= sum}. Does not enforce that
     * {@code sumCard = |sum|} because of how the compilation works, it is
     * already enforced elsewhere.
     * </p>
     * <p>
     * More * efficient than the standard operation in the Choco library when
     * the cardinality is bounded to be relatively small.
     * </p>
     * <p>
     * For example:
     * <pre>
     *   Animal 2
     *     Age -> integer
     *   [Animal.Age = 1000]
     * </pre>
     * </p>
     * <p>
     * {@code Animal.Age} is a set with a very large envelope. However, due to
     * static analysis of the model, it is easy to see that the cardinality must
     * be 2. In general, the cardinality is bounded by the scope of Age,
     * although often times the analysis will find a tighter bound. Once the
     * first integer x is selected for the set, the second integer 1000 - x is
     * already determined due to cardinality = 2. Since the Choco library's
     * setSum constraint is not given the cardinality, it cannot make this
     * deduction.
     * </p>
     *
     * @param set the set of integers
     * @param setCard the cardinality of {@code set}
     * @param sum the sum of the set
     * @return constraint {@code Σ set= sum}
     */
    public static Constraint setSum(SetVar set, IntVar setCard, IntVar sum) {
        return new Constraint("setSum", new PropSetSum(set, setCard, sum));
    }

    /**
     *******************
     *
     * Set. When using set constraints, beware of the cases where the
     * documentation states that certain cardinality constraints are to be
     * enforced elsewhere. The only reason is for efficiency.
     *
     *******************
     */
    /**
     * A constraint enforcing {@code {ivar} = svar}.
     *
     * @param ivar the integer
     * @param svar the singleton set
     * @return constraint {@code {ivar} = svar}
     */
    public static Constraint singleton(IntVar ivar, SetVar svar) {
        return new Constraint("singleton", new PropSingleton(ivar, svar));
    }

    /**
     * A constraint enforcing {@code {ivar} = svar} and {@code svarCard = 1}.
     * Does not enforce that {@code svarCard = |svarCard|} because of how the
     * compilation works, it is already enforced elsewhere.
     *
     * @param ivar the integer
     * @param svar the singleton set
     * @param svarCard the cardinality of {@code svar}
     * @return constraint {@code {ivar} = svar}
     */
    public static Constraint singleton(IntVar ivar, SetVar svar, IntVar svarCard) {
        return new Constraint("singleton",
                new PropSingleton(ivar, svar),
                new PropEqualXC(svarCard, 1));
    }

    /**
     * A constraint enforcing {@code {ivar[0], ivar[1], ..., ivar[n]} = svar}.
     *
     * @param ivars the array
     * @param svar the set
     * @param svarCard the cardinality of {@code svar}
     * @return constraint {@code {ivar[0], ivar[1], ..., ivar[n]} = svar}
     */
    public static Constraint arrayToSet(IntVar[] ivars, SetVar svar, IntVar svarCard) {
        return arrayToSet(ivars, svar, svarCard, null);
    }

    /**
     * A constraint enforcing {@code {ivar[0], ivar[1], ..., ivar[n]} = svar}
     * and {@code for all constant k, |{i | ivar[i] = k}| ≤ globalCardinality}.
     * Does not enforce that {@code svarCard = |svar|} because of how the
     * compilation works, it is already enforced elsewhere.
     *
     * @param ivars the array
     * @param svar the set
     * @param svarCard the cardinality of {@code svar}
     * @param globalCardinality the global cardinality of the array elements
     * @return constraint {@code {ivar[0], ivar[1], ..., ivar[n]} = svar} and
     * {@code for all constant k |{i | ivar[i] = k}| ≤ globalCardinality}
     */
    public static Constraint arrayToSet(IntVar[] ivars, SetVar svar, IntVar svarCard, Integer globalCardinality) {
        return new Constraint("arrayToSet",
                new PropArrayToSet(ivars, svar),
                new PropArrayToSetCard(ivars, svarCard, globalCardinality));
    }

    @Deprecated // Every join relation in Clafer is injective.
    public static Constraint joinRelation(SetVar take, SetVar[] children, SetVar to) {
        return new Constraint("joinRelation", new PropJoinRelation(take, children, to));
    }

    /**
     * A constraint enforcing {@code take.children = to} where children is an
     * injective relation. The representation of the relation is explained in
     * {@link PropJoinRelation}. Does not enforce that the children relation is
     * injective nor {@code takeCard = |take|} nor
     * {@code childrenCards[i] = |children[i]|} nor {@code toCard = |to|}
     * because of how the compilation works, it is already enforced elsewhere.
     *
     * @param take the left-hand side set
     * @param takeCard the cardinality of {@code take}
     * @param children the set representation of a injective binary relation
     * @param childrenCards the cardinalities of {@code children}
     * @param to the right-hand side set
     * @param toCard the cardinality of {@code to}
     * @return constraint {@code take.children = to}
     * @see PropJoinFunction
     */
    public static Constraint joinInjectiveRelation(SetVar take, IntVar takeCard, SetVar[] children, IntVar[] childrenCards, SetVar to, IntVar toCard) {
        if (children.length != childrenCards.length) {
            throw new IllegalArgumentException();
        }

        return new Constraint("joinInjectiveRelation",
                new PropJoinRelation(take, children, to),
                new PropJoinInjectiveRelationCard(take, takeCard, childrenCards, toCard));
    }

    /**
     * A constraint enforcing {@code take.refs = to} where refs is a function.
     * The representation of the function is explained in
     * {@link PropJoinFunction}. Does not enforce that {@code takeCard = |take|}
     * nor {@code toCard = |to|} because of how the compilation works, it is
     * already enforced elsewhere.
     *
     * @param take the left-hand side set
     * @param takeCard the cardinality of {@code take}
     * @param refs the integer representation of a binary function
     * @param to the right-hand side set
     * @param toCard the cardinality of {@code to}
     * @return constraint {@code take.refs = to}
     * @see PropJoinFunction
     */
    public static Constraint joinFunction(SetVar take, IntVar takeCard, IntVar[] refs, SetVar to, IntVar toCard) {
        return joinFunction(take, takeCard, refs, to, toCard, null);
    }

    /**
     * A constraint enforcing {@code take.refs = to} where refs is a function
     * and {@code for all k in take, |{i | refs[i] = k}| ≤ globalCardinality}.
     * The representation of the function is explained in
     * {@link PropJoinFunction}. Does not enforce that {@code takeCard = |take|}
     * nor {@code toCard = |to|} because of how the compilation works, it is
     * already enforced elsewhere.
     *
     * @param take the left-hand side set
     * @param takeCard the cardinality of {@code take}
     * @param refs the integer representation of a binary function
     * @param to the right-hand side set
     * @param toCard the cardinality of {@code to}
     * @param globalCardinality the global cardinality of the {@code refs}
     * function for the domain of {@code take}
     * @return constraint {@code take.refs = to} and
     * {@code for all k in take, |{i | refs[i] = k}| ≤ globalCardinality}
     * @see PropJoinFunction
     */
    public static Constraint joinFunction(SetVar take, IntVar takeCard, IntVar[] refs, SetVar to, IntVar toCard, Integer globalCardinality) {
        if (globalCardinality != null && globalCardinality <= 0) {
            throw new IllegalArgumentException();
        }
        return new Constraint("joinFunction",
                new PropJoinFunction(take, refs, to),
                new PropJoinFunctionCard(take, takeCard, refs, toCard, globalCardinality));
    }

    /**
     * A constraint enforcing {@code minuend - subtrahend = difference}. Does
     * not enforce that {@code minuendCard = |minuend|} nor
     * {@code subtrahendCard = |subtrahend|} nor
     * {@code differenceCard = |difference|} because of how the compilation
     * works, it is already enforced elsewhere.
     *
     * @param minuend the minuend
     * @param minuendCard the cardinality of {@code minuend}
     * @param subtrahend the subtrahend
     * @param subtrahendCard the cardinality of {@code subtrahend}
     * @param difference the difference
     * @param differenceCard the cardinality of {@code difference}
     * @return constraint {@code minuend - subtrahend = difference}
     */
    public static Constraint difference(
            SetVar minuend, IntVar minuendCard,
            SetVar subtrahend, IntVar subtrahendCard,
            SetVar difference, IntVar differenceCard) {
        return new Constraint("difference",
                new PropSetDifference(minuend, subtrahend, difference),
                // Simple cardinality propagation.
                greaterThanEq(minuendCard, differenceCard));
    }

    /**
     * A constraint enforcing
     * {@code operands[0] ∩ operands[1] ∩ ... ∩ operands[n] = intersection}.
     * Does not enforce that {@code operandCards[i] = |operands[i]|} nor
     * {@code intersectionCard = |intersection|} because of how the compilation
     * works, it is already enforced elsewhere.
     *
     * @param operands the operands
     * @param operandCards the cardinalities of {@code operands}
     * @param intersection the intersection
     * @param intersectionCard the cardinality of {@code intersection}
     * @return constraint
     * {@code operands[0] ∩ operands[1] ∩ ... ∩ operands[n] = intersection}
     */
    public static Constraint intersection(
            SetVar[] operands, IntVar[] operandCards,
            SetVar intersection, IntVar intersectionCard) {
        if (operands.length != operandCards.length) {
            throw new IllegalArgumentException();
        }

        @SuppressWarnings("unchecked")
        Propagator<? extends Variable>[] propagators = new Propagator<?>[operandCards.length + 2];
        // See SCF.intersection(operands, intersection);
        // TODO: Needs to add the same propagator twice because the implementation
        // is not guaranteed to be idempotent. If it ever becomes idempotent, then
        // follow their implementation.
        propagators[0] = new PropIntersection(operands, intersection);
        propagators[1] = new PropIntersection(operands, intersection);
        for (int i = 0; i < operandCards.length; i++) {
            // Simple cardinality propagation.
            propagators[i + 2] = greaterThanEq(operandCards[i], intersectionCard);
        }
        return new Constraint("intersection", propagators);
    }

    /**
     * A constraint enforcing
     * {@code operands[0] ∪ operands[1] ∪ ... ∪ operands[n] = union}. Does not
     * enforce that {@code operandCards[i] = |operands[i]|} nor
     * {@code unionCard = |union|} because of how the compilation works, it is
     * already enforced elsewhere.
     *
     * @param operands the operands
     * @param operandCards the cardinalities of {@code operands}
     * @param union the union
     * @param unionCard the cardinality of {@code union}
     * @param disjoint the sets are disjoint
     * @return constraint
     * {@code operands[0] ∪ operands[1] ∪ ... ∪ operands[n] = union}
     */
    public static Constraint union(
            SetVar[] operands, IntVar[] operandCards,
            SetVar union, IntVar unionCard,
            boolean disjoint) {
        if (operands.length != operandCards.length) {
            throw new IllegalArgumentException();
        }

        return new Constraint("union",
                new PropSetUnion(operands, union),
                disjoint
                        ? sumEq(operandCards, unionCard)
                        : new PropSetUnionCard(operandCards, unionCard));
    }

    public static Constraint max(SetVar set, IntVar setCard, IntVar max) {
        return new Constraint("max", new PropSetMax(set, setCard, max));
    }

    public static Constraint max(SetVar set, IntVar setCard, IntVar max, int d) {
        if (setCard.getLB() > 0) {
            return max(set, setCard, max);
        }
        return new Constraint("max", new PropSetMax(set, setCard, max),
                new PropReifyEqualXC(setCard, 0, max, d));
    }

    public static Constraint stritctHighBound(SetVar set, IntVar bound) {
        return new Constraint("strictHighBound", new PropSetStrictHighBound(set, bound));
    }

    public static Constraint min(SetVar set, IntVar setCard, IntVar min) {
        return new Constraint("min", new PropSetMin(set, setCard, min));
    }

    public static Constraint lowBound(SetVar set, IntVar bound) {
        return new Constraint("lowBound", new PropSetLowBound(set, bound));
    }

    public static Constraint element(IntVar index, SetVar[] array, IntVar[] arrayCards, SetVar value, IntVar valueCard) {
        if (array.length != arrayCards.length) {
            throw new IllegalArgumentException();
        }
        return new Constraint("element",
                new org.chocosolver.solver.constraints.set.PropElement(index, array, 0, value),
                new org.chocosolver.solver.constraints.set.PropElement(index, array, 0, value),
                new PropElementV_fast(valueCard, arrayCards, index, 0, true),
                new PropElementV_fast(valueCard, arrayCards, index, 0, true));
    }

    /**
     * A constraint enforcing
     * {@code {i + from | i ∈ member} = set ∩ {from, from + 1, ..., to - 1}}.
     * Does not enforce that {@code setCard = |set|} nor
     * {@code maskedCard = |masked|} because of how the compilation works, it is
     * already enforced elsewhere.
     *
     * @param set the set
     * @param setCard the cardinality of {@code set}
     * @param masked the masked set
     * @param maskedCard the cardinality of {@code masked}
     * @param from the inclusive start of the mask
     * @param to the exclusive end of the mask
     * @return constraint
     * {@code {i + from | i ∈ member} = set ∩ {from, from + 1, ..., to - 1}}
     */
    public static Constraint mask(
            SetVar set, IntVar setCard,
            SetVar masked, IntVar maskedCard,
            int from, int to) {
        return new Constraint("mask",
                new PropMask(set, masked, from, to),
                // Simple cardinality propagation.
                greaterThanEq(setCard, maskedCard));
    }

    /**
     * TODO STRING
     */
    public static Constraint length(IntVar[] chars, IntVar length) {
        return new Constraint("length", new PropLength(chars, length));
    }

    /**
     * TODO STRING
     */
    public static Constraint equal(
            IntVar[] chars1, IntVar length1,
            IntVar[] chars2, IntVar length2) {
        List<Propagator<IntVar>> propagators = new ArrayList<>();
        eq(length1, length2).ifPresent(propagators::add);
        for (int i = 0; i < Math.min(chars1.length, chars2.length); i++) {
            eq(chars1[i], chars2[i]).ifPresent(propagators::add);
        }
        for (int i = Math.min(chars1.length, chars2.length); i < chars1.length; i++) {
            eq(chars1[i], 0).ifPresent(propagators::add);
        }
        for (int i = Math.min(chars1.length, chars2.length); i < chars2.length; i++) {
            eq(chars2[i], 0).ifPresent(propagators::add);
        }
        if (propagators.isEmpty()) {
            return length1.getSolver().TRUE;
        }
        return new Constraint("arrayEqual",
                propagators.toArray(new Propagator<?>[propagators.size()]));
    }

    /**
     * TODO STRING
     */
    public static Constraint notEqual(
            IntVar[] chars1, IntVar length1,
            IntVar[] chars2, IntVar length2) {
        return equal(chars1, length1, chars2, length2).getOpposite();
    }

    public static Constraint lessThan(IntVar[] chars1, IntVar[] chars2) {
        int maxLength = Math.max(chars1.length, chars2.length);
        return ICF.lex_less(
                pad(chars1, maxLength, chars1[0].getSolver().ZERO),
                pad(chars2, maxLength, chars1[0].getSolver().ZERO));
    }

    public static Constraint lessThanEqual(IntVar[] chars1, IntVar[] chars2) {
        int maxLength = Math.max(chars1.length, chars2.length);
        return ICF.lex_less_eq(
                pad(chars1, maxLength, chars1[0].getSolver().ZERO),
                pad(chars2, maxLength, chars1[0].getSolver().ZERO));
    }

    private static IntVar[] charsAt(Solver solver, IntVar[][] strings, int index) {
        IntVar[] charsAt = new IntVar[strings.length];
        for (int i = 0; i < charsAt.length; i++) {
            charsAt[i] = index < strings[i].length
                    ? strings[i][index] : solver.ZERO;
        }
        return charsAt;
    }

    /**
     * TODO STRING
     */
    public static Constraint element(IntVar index,
            IntVar[][] array, IntVar[] arrayLengths,
            IntVar[] value, IntVar valueLength) {
        if (array.length != arrayLengths.length) {
            throw new IllegalArgumentException();
        }
        List<Propagator<IntVar>> propagators = new ArrayList<>();
        // See ICF.element(value, table, index, offset);
        // TODO: Needs to add the same propagator twice because the implementation
        // is not guaranteed to be idempotent. If it ever becomes idempotent, then
        // follow their implementation.
        propagators.add(new PropElementV_fast(valueLength, arrayLengths, index, 0, true));
        propagators.add(new PropElementV_fast(valueLength, arrayLengths, index, 0, true));
        for (int i = 0; i < value.length; i++) {
            IntVar[] charsAt = charsAt(index.getSolver(), array, i);
            propagators.add(new PropElementV_fast(value[i], charsAt, index, 0, true));
            propagators.add(new PropElementV_fast(value[i], charsAt, index, 0, true));
        }
        return new Constraint("Element",
                propagators.toArray(new Propagator<?>[propagators.size()]));
    }

    /**
     * TODO STRING
     */
    public static Constraint prefix(
            IntVar[] prefix, IntVar prefixLength,
            IntVar[] word, IntVar wordLength) {
        if (prefixLength.getLB() > wordLength.getUB()) {
            return prefixLength.getSolver().FALSE;
        }
        return new Constraint("Prefix",
                lessThanEq(prefixLength, wordLength),
                new PropSamePrefix(prefixLength, prefix, word));
    }

    public static Constraint suffix(
            IntVar[] suffix, IntVar suffixLength,
            IntVar[] word, IntVar wordLength) {
        Solver solver = suffixLength.getSolver();
        if (suffixLength.getLB() > wordLength.getUB()) {
            return solver.FALSE;
        }
        IntVar prefixLength = VF.enumerated("SuffixVar" + varNum++,
                Math.min(wordLength.getLB() - suffixLength.getUB(), 0),
                wordLength.getUB() - suffixLength.getLB(), solver);
        solver.post(new Constraint("SuffixVarSum",
                sumEq(new IntVar[]{prefixLength, suffixLength}, wordLength)));
        List<Propagator<IntVar>> propagators = new ArrayList<>();
        propagators.add(lessThanEq(suffixLength, wordLength));
        for (int i = 0; i < suffix.length; i++) {
            IntVar[] pad = pad(word, prefixLength.getUB() + i + 1, suffixLength.getSolver().ZERO);
            // See ICF.element(value, table, index, offset);
            // TODO: Needs to add the same propagator twice because the implementation
            // is not guaranteed to be idempotent. If it ever becomes idempotent, then
            // follow their implementation.
            propagators.add(new PropElementV_fast(suffix[i], pad, prefixLength, -i, true));
            propagators.add(new PropElementV_fast(suffix[i], pad, prefixLength, -i, true));
        }
        return new Constraint("Suffix",
                propagators.toArray(new Propagator<?>[propagators.size()]));
    }

    /**
     * TODO STRING
     */
    public static Constraint concat(
            IntVar[] left, IntVar leftLength,
            IntVar[] right, IntVar rightLength,
            IntVar[] concat, IntVar concatLength) {
        if (leftLength.getLB() + rightLength.getLB() > concatLength.getUB()) {
            return leftLength.getSolver().FALSE;
        }
        List<Propagator<IntVar>> propagators = new ArrayList<>();
        propagators.add(sumEq(new IntVar[]{leftLength, rightLength}, concatLength));
        propagators.add(new PropSamePrefix(leftLength, left, concat));
        for (int i = 0; i < right.length; i++) {
            IntVar[] pad = pad(concat, left.length + i + 1, leftLength.getSolver().ZERO);
            // See ICF.element(value, table, index, offset);
            // TODO: Needs to add the same propagator twice because the implementation
            // is not guaranteed to be idempotent. If it ever becomes idempotent, then
            // follow their implementation.
            propagators.add(new PropElementV_fast(right[i], pad, leftLength, -i, true));
            propagators.add(new PropElementV_fast(right[i], pad, leftLength, -i, true));
        }
        return new Constraint("Concat",
                propagators.toArray(new Propagator<?>[propagators.size()]));
    }

    private static IntVar[] pad(IntVar[] chars, int length, IntVar zero) {
        if (length == chars.length) {
            return chars;
        }
        if (length < chars.length) {
            return Arrays.copyOf(chars, length);
        }
        IntVar[] pad = Arrays.copyOf(chars, length);
        Arrays.fill(pad, chars.length, pad.length, zero);
        return pad;
    }

    public static Constraint transitive(SetVar[] relation) {
        return new Constraint("transitive",
                new PropTransitive(relation),
                new PropTransitiveUnreachable(relation));
    }

    public static Constraint reflexive(SetVar[] relation) {
        return new Constraint("transitive", new PropReflexive(relation));
    }

    public static Constraint transitiveClosure(SetVar[] relation, SetVar[] closure, boolean reflexive) {
        return reflexive
                ? transitiveReflexiveClosure(relation, closure)
                : transitiveClosure(relation, closure);
    }

    public static Constraint transitiveClosure(SetVar[] relation, SetVar[] closure) {
        if (relation.length != closure.length) {
            throw new IllegalArgumentException();
        }
        @SuppressWarnings("unchecked")
        Propagator<SetVar>[] propagators
                = (Propagator<SetVar>[]) new Propagator<?>[relation.length + 3];
        for (int i = 0; i < relation.length; i++) {
            propagators[i] = new PropSubsetEq(relation[i], closure[i]);
        }
        propagators[relation.length] = new PropAtMostTransitiveClosure(relation, closure, false);
        propagators[relation.length + 1] = new PropTransitive(closure);
        propagators[relation.length + 2] = new PropTransitiveUnreachable(closure);
        return new Constraint("transitive", propagators);
    }

    public static Constraint transitiveReflexiveClosure(SetVar[] relation, SetVar[] closure) {
        if (relation.length != closure.length) {
            throw new IllegalArgumentException();
        }
        @SuppressWarnings("unchecked")
        Propagator<SetVar>[] propagators
                = (Propagator<SetVar>[]) new Propagator<?>[relation.length + 4];
        for (int i = 0; i < relation.length; i++) {
            propagators[i] = new PropSubsetEq(relation[i], closure[i]);
        }
        propagators[relation.length] = new PropAtMostTransitiveClosure(relation, closure, true);
        propagators[relation.length + 1] = new PropReflexive(closure);
        propagators[relation.length + 2] = new PropTransitive(closure);
        propagators[relation.length + 3] = new PropTransitiveUnreachable(closure);
        return new Constraint("transitiveReflexive", propagators);
    }
}
