package org.clafer.choco.constraint;

import java.util.ArrayList;
import java.util.List;
import org.clafer.choco.constraint.propagator.PropAcyclic;
import org.clafer.choco.constraint.propagator.PropAnd;
import org.clafer.choco.constraint.propagator.PropArrayToSet;
import org.clafer.choco.constraint.propagator.PropArrayToSetCard;
import org.clafer.choco.constraint.propagator.PropFilterString;
import org.clafer.choco.constraint.propagator.PropIfThenElse;
import org.clafer.choco.constraint.propagator.PropIntChannel;
import org.clafer.choco.constraint.propagator.PropIntNotMemberSet;
import org.clafer.choco.constraint.propagator.PropJoinFunction;
import org.clafer.choco.constraint.propagator.PropJoinFunctionCard;
import org.clafer.choco.constraint.propagator.PropJoinInjectiveRelationCard;
import org.clafer.choco.constraint.propagator.PropJoinRelation;
import org.clafer.choco.constraint.propagator.PropLexChainChannel;
import org.clafer.choco.constraint.propagator.PropLone;
import org.clafer.choco.constraint.propagator.PropMask;
import org.clafer.choco.constraint.propagator.PropOne;
import org.clafer.choco.constraint.propagator.PropOr;
import org.clafer.choco.constraint.propagator.PropSelectN;
import org.clafer.choco.constraint.propagator.PropSetDifference;
import org.clafer.choco.constraint.propagator.PropSetNotEqualC;
import org.clafer.choco.constraint.propagator.PropSetSum;
import org.clafer.choco.constraint.propagator.PropSetUnion;
import org.clafer.choco.constraint.propagator.PropSetUnionCard;
import org.clafer.choco.constraint.propagator.PropSingleton;
import org.clafer.choco.constraint.propagator.PropSortedSets;
import org.clafer.choco.constraint.propagator.PropSortedSetsCard;
import org.clafer.choco.constraint.propagator.PropUnreachable;
import org.clafer.common.Util;
import solver.constraints.Constraint;
import solver.constraints.Propagator;
import solver.constraints.binary.PropEqualXY_C;
import solver.constraints.binary.PropEqualX_Y;
import solver.constraints.binary.PropEqualX_YC;
import solver.constraints.binary.PropGreaterOrEqualX_Y;
import solver.constraints.nary.sum.PropSumEq;
import solver.constraints.set.PropIntersection;
import solver.constraints.set.PropSubsetEq;
import solver.constraints.unary.PropEqualXC;
import solver.constraints.unary.PropGreaterOrEqualXC;
import solver.constraints.unary.PropLessOrEqualXC;
import solver.variables.BoolVar;
import solver.variables.IntVar;
import solver.variables.SetVar;
import solver.variables.VF;
import solver.variables.Variable;

/**
 * Custom Choco constraints. Designed for Clafer. Note that these constraints
 * requires that the envelope and kernel to be in sorted order from lowest to
 * highest, which is not explicitly enforced by Choco.
 *
 * @author jimmy
 */
public class Constraints {

    private Constraints() {
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
        IntVar[] filtered =
                filter.size() == ints.length
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
                return new PropSumEq(Util.cons(VF.fixed(constant, sum.getSolver()), filtered), sum);
        }
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
    public static Constraint ifThenElse(BoolVar antecedent, BoolVar consequent, BoolVar alternative) {
        return new Constraint("ifThenElse", new PropIfThenElse(antecedent, consequent, alternative));
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

    /**
     * A constraint enforcing {@code element ∉ set}.
     *
     * @param element the element
     * @param set the set
     * @return constraint {@code element ∉ set}.
     */
    public static Constraint notMember(IntVar element, SetVar set) {
        return new Constraint("notMember", new PropIntNotMemberSet(element, set));
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

        return new Constraint("sortedSets",
                new PropSortedSets(sets),
                new PropSortedSetsCard(sets, setCards));
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
     * <p> {@code Animal.Age} is a set with a very large envelope. However, due
     * to static analysis of the model, it is easy to see that the cardinality
     * must be 2. In general, the cardinality is bounded by the scope of Age,
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
        Propagator<? extends Variable>[] propagators = new Propagator[operandCards.length + 2];
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
}
