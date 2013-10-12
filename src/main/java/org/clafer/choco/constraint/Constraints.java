package org.clafer.choco.constraint;

import org.clafer.choco.constraint.propagator.PropAnd;
import org.clafer.choco.constraint.propagator.PropJoinRelation;
import org.clafer.choco.constraint.propagator.PropJoinFunction;
import org.clafer.choco.constraint.propagator.PropSelectN;
import org.clafer.choco.constraint.propagator.PropSingleton;
import org.clafer.choco.constraint.propagator.PropArrayToSet;
import org.clafer.choco.constraint.propagator.PropArrayToSetCard;
import org.clafer.choco.constraint.propagator.PropFilterString;
import org.clafer.choco.constraint.propagator.PropIntChannel;
import org.clafer.choco.constraint.propagator.PropIntNotMemberSet;
import org.clafer.choco.constraint.propagator.PropJoinFunctionCard;
import org.clafer.choco.constraint.propagator.PropJoinInjectiveRelationCard;
import org.clafer.choco.constraint.propagator.PropLexChainChannel;
import org.clafer.choco.constraint.propagator.PropLone;
import org.clafer.choco.constraint.propagator.PropOne;
import org.clafer.choco.constraint.propagator.PropOr;
import org.clafer.choco.constraint.propagator.PropReifyEqualXC;
import org.clafer.choco.constraint.propagator.PropReifyNotEqualXC;
import org.clafer.choco.constraint.propagator.PropSetDifference;
import org.clafer.choco.constraint.propagator.PropSetEqual;
import org.clafer.choco.constraint.propagator.PropSetNotEqual;
import org.clafer.choco.constraint.propagator.PropSetNotEqualC;
import org.clafer.choco.constraint.propagator.PropSetSum;
import org.clafer.choco.constraint.propagator.PropSetUnion;
import org.clafer.choco.constraint.propagator.PropSetUnionCard;
import org.clafer.choco.constraint.propagator.PropSortedSets;
import org.clafer.choco.constraint.propagator.PropSortedSetsCard;
import solver.constraints.Constraint;
import solver.constraints.Propagator;
import solver.constraints.binary.PropEqualX_Y;
import solver.constraints.binary.PropGreaterOrEqualX_Y;
import solver.constraints.set.PropCardinality;
import solver.constraints.set.PropIntersection;
import solver.constraints.set.PropSubsetEq;
import solver.constraints.unary.PropEqualXC;
import solver.constraints.unary.PropGreaterOrEqualXC;
import solver.constraints.unary.PropLessOrEqualXC;
import solver.variables.BoolVar;
import solver.variables.IntVar;
import solver.variables.SetVar;
import solver.variables.Variable;

/**
 *
 * @author jimmy
 */
public class Constraints {

    private Constraints() {
    }

    private static Propagator<IntVar> lessThanEq(IntVar l, IntVar g) {
        if (l.instantiated()) {
            return new PropGreaterOrEqualXC(g, l.getValue());
        }
        if (g.instantiated()) {
            return new PropLessOrEqualXC(l, g.getValue());
        }
        return new PropGreaterOrEqualX_Y(new IntVar[]{g, l});
    }

    private static Propagator<IntVar> greaterThanEq(IntVar g, IntVar l) {
        return lessThanEq(l, g);
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
        Constraint<BoolVar, PropAnd> constraint = new Constraint<BoolVar, PropAnd>(operands, operands[0].getSolver());
        constraint.setPropagators(new PropAnd(operands));
        return constraint;
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
        Constraint<BoolVar, PropLone> constraint = new Constraint<BoolVar, PropLone>(operands, operands[0].getSolver());
        constraint.setPropagators(new PropLone(operands));
        return constraint;
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
        Constraint<BoolVar, PropOne> constraint = new Constraint<BoolVar, PropOne>(operands, operands[0].getSolver());
        constraint.setPropagators(new PropOne(operands));
        return constraint;
    }

    /**
     * A constraint enforcing
     * {@code operands[0] ∨ operands[1] ∨ ... ∨ operands[n]}.
     *
     * @param operands the operands
     * @return constraint {@code operands[0] ∨ operands[1] ∨ ... ∨ operands[n]}
     */
    public static Constraint or(BoolVar... operands) {
        Constraint<BoolVar, PropOr> constraint = new Constraint<BoolVar, PropOr>(operands, operands[0].getSolver());
        constraint.setPropagators(new PropOr(operands));
        return constraint;
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
     * A constraint enforcing {@code reify <=> (variable = constant)}.
     *
     * @param reify the reified constraint
     * @param variable the variable
     * @param constant the constant
     * @return constraint {@code reify <=> (variable = constant)}
     */
    public static Constraint reifyEqual(BoolVar reify, IntVar variable, int constant) {
        Constraint<IntVar, PropReifyEqualXC> constraint =
                new Constraint<IntVar, PropReifyEqualXC>(new IntVar[]{reify, variable}, reify.getSolver());
        constraint.setPropagators(new PropReifyEqualXC(reify, variable, constant));
        return constraint;
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
        Constraint<IntVar, PropReifyNotEqualXC> constraint =
                new Constraint<IntVar, PropReifyNotEqualXC>(new IntVar[]{reify, variable}, reify.getSolver());
        constraint.setPropagators(new PropReifyNotEqualXC(reify, variable, constant));
        return constraint;
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
        @SuppressWarnings("unchecked")
        Constraint<? extends Variable, Propagator<? extends Variable>> constraint =
                new Constraint(new Variable[]{set1, set1Card, set2, set2Card}, set1.getSolver());

        @SuppressWarnings("unchecked")
        Propagator<? extends Variable>[] propagators = new Propagator[]{
            new PropSetEqual(set1, set2),
            new PropEqualX_Y(set1Card, set2Card)
        };
        constraint.setPropagators(propagators);

        return constraint;
    }

    /**
     * A constraint enforcing {@code set1 ≠ set2}.
     *
     * @param set1 the left set
     * @param set2 the right set
     * @return constraint {@code set1 ≠ set2}
     */
    public static Constraint notEqual(SetVar set1, SetVar set2) {
        Constraint<SetVar, PropSetNotEqual> constraint =
                new Constraint<SetVar, PropSetNotEqual>(new SetVar[]{set1, set2}, set1.getSolver());
        constraint.setPropagators(new PropSetNotEqual(set1, set2));
        return constraint;
    }

    /**
     * A constraint enforcing {@code set ≠ {constant}}.
     *
     * @param set the set
     * @param constant the constant
     * @return constraint {@code set1 ≠ set2}
     */
    public static Constraint notEqual(SetVar set, int[] constant) {
        Constraint<SetVar, PropSetNotEqualC> constraint =
                new Constraint<SetVar, PropSetNotEqualC>(new SetVar[]{set}, set.getSolver());
        constraint.setPropagators(new PropSetNotEqualC(set, constant));
        return constraint;
    }

    /**
     * A constraint enforcing {@code element ∉ set}.
     *
     * @param element the element
     * @param set the set
     * @return constraint {@code element ∉ set}.
     */
    public static Constraint notMember(IntVar element, SetVar set) {
        Constraint<Variable, PropIntNotMemberSet> constraint =
                new Constraint<Variable, PropIntNotMemberSet>(new Variable[]{element, set}, element.getSolver());
        constraint.setPropagators(new PropIntNotMemberSet(element, set));
        return constraint;
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
        @SuppressWarnings("unchecked")
        Constraint<? extends Variable, Propagator<? extends Variable>> constraint =
                new Constraint(new Variable[]{sub, subCard, sup, supCard}, sub.getSolver());

        @SuppressWarnings("unchecked")
        Propagator<? extends Variable>[] propagators = new Propagator[]{
            new PropSubsetEq(sub, sup),
            lessThanEq(subCard, supCard)
        };
        constraint.setPropagators(propagators);

        return constraint;
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
        Variable[] variables = new Variable[sets.length + ints.length];
        System.arraycopy(sets, 0, variables, 0, sets.length);
        System.arraycopy(ints, 0, variables, sets.length, ints.length);

        Constraint<Variable, PropIntChannel> constraint =
                new Constraint<Variable, PropIntChannel>(variables, sets[0].getSolver());
        constraint.setPropagators(new PropIntChannel(sets, ints));

        return constraint;
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

        Variable[] variables = new Variable[sets.length + setCards.length];
        System.arraycopy(sets, 0, variables, 0, sets.length);
        System.arraycopy(setCards, 0, variables, sets.length, setCards.length);

        @SuppressWarnings("unchecked")
        Constraint<? extends Variable, Propagator<? extends Variable>> constraint =
                new Constraint(variables, sets[0].getSolver());

        @SuppressWarnings("unchecked")
        Propagator<? extends Variable>[] propagators = new Propagator[]{
            new PropSortedSets(sets),
            new PropSortedSetsCard(sets, setCards)
        };
        constraint.setPropagators(propagators);

        return constraint;
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

        IntVar[] variables = new IntVar[strings.length * strings[0].length + ints.length];
        System.arraycopy(ints, 0, variables, 0, ints.length);
        int i = ints.length;
        for (IntVar[] string : strings) {
            System.arraycopy(string, 0, variables, i, string.length);
            i += string.length;
        }
        assert i == variables.length;

        Constraint<IntVar, PropLexChainChannel> constraint =
                new Constraint<IntVar, PropLexChainChannel>(variables, variables[0].getSolver());
        constraint.setPropagators(new PropLexChainChannel(strings, ints));

        return constraint;
    }

    /**
     * A constraint enforcing that {@code bools[i] <=> i < n}.
     *
     * @param bools the booleans
     * @param n the number of true booleans
     * @return constraint {@code bools[i] <=> i < n}
     */
    public static Constraint selectN(BoolVar[] bools, IntVar n) {
        IntVar[] variables = new IntVar[bools.length + 1];
        System.arraycopy(bools, 0, variables, 0, bools.length);
        variables[bools.length] = n;

        Constraint<IntVar, PropSelectN> constraint =
                new Constraint<IntVar, PropSelectN>(variables, n.getSolver());
        constraint.setPropagators(new PropSelectN(bools, n));

        return constraint;
    }

    /**
     * A constraint enforcing
     * {@code result[i] = if i \u003c array(set).length then string[array(set)[i] - offset] else -1}
     * where {@code array} is the sorted array representation of the set.
     *
     * @param set the set
     * @param offset the offset
     * @param string the string
     * @param result the result
     * @return constraint
     * {@code result[i] = if i \u003c array(set).length then string[array(set)[i] - offset] else -1}
     */
    public static Constraint filterString(SetVar set, int offset, IntVar[] string, IntVar[] result) {
        Constraint<Variable, PropFilterString> constraint =
                new Constraint<Variable, PropFilterString>(PropFilterString.buildArray(set, string, result), set.getSolver());
        constraint.setPropagators(new PropFilterString(set, offset, string, result));
        return constraint;
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
        Constraint<Variable, PropSetSum> constraint =
                new Constraint<Variable, PropSetSum>(new Variable[]{set, setCard, sum}, set.getSolver());
        constraint.setPropagators(new PropSetSum(set, setCard, sum));
        return constraint;
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
        Constraint<Variable, PropSingleton> constraint =
                new Constraint<Variable, PropSingleton>(new Variable[]{ivar, svar}, ivar.getSolver());
        constraint.setPropagators(new PropSingleton(ivar, svar));
        return constraint;
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
        @SuppressWarnings("unchecked")
        Constraint<? extends Variable, Propagator<? extends Variable>> constraint =
                new Constraint(new Variable[]{ivar, svar, svarCard}, ivar.getSolver());

        @SuppressWarnings("unchecked")
        Propagator<? extends Variable>[] propagators = new Propagator[]{
            new PropSingleton(ivar, svar),
            new PropEqualXC(svarCard, 1)
        };
        constraint.setPropagators(propagators);

        return constraint;
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
     *
     * @param ivars the array
     * @param svar the set
     * @param svarCard the cardinality of {@code svar}
     * @param globalCardinality the global cardinality of the array elements
     * @return constraint {@code {ivar[0], ivar[1], ..., ivar[n]} = svar} and
     * {@code for all constant k |{i | ivar[i] = k}| ≤ globalCardinality}
     */
    public static Constraint arrayToSet(IntVar[] ivars, SetVar svar, IntVar svarCard, Integer globalCardinality) {
        Variable[] variables = new Variable[ivars.length + 2];
        variables[0] = svar;
        variables[1] = svarCard;
        System.arraycopy(ivars, 0, variables, 2, ivars.length);

        @SuppressWarnings("unchecked")
        Constraint<? extends Variable, Propagator<? extends Variable>> constraint =
                new Constraint(variables, svar.getSolver());

        @SuppressWarnings("unchecked")
        Propagator<? extends Variable>[] propagators = new Propagator[]{
            new PropArrayToSet(ivars, svar),
            new PropArrayToSetCard(ivars, svarCard, globalCardinality),
            new PropCardinality(svar, svarCard)
        };
        constraint.setPropagators(propagators);

        return constraint;
    }

    @Deprecated // Every join relation in Clafer is injective.
    public static Constraint joinRelation(SetVar take, SetVar[] children, SetVar to) {
        SetVar[] variables = new SetVar[children.length + 2];
        variables[0] = take;
        variables[1] = to;
        System.arraycopy(children, 0, variables, 2, children.length);

        Constraint<SetVar, PropJoinRelation> constraint = new Constraint<SetVar, PropJoinRelation>(variables, take.getSolver());
        constraint.setPropagators(new PropJoinRelation(take, children, to));

        return constraint;
    }

    /**
     * A constraint enforcing {@code take.children = to} where children is an
     * injective relation. The representation of the relation is explained in
     * {@link PropJoinRelation}. Does not enforce that the children relation is
     * injective nor that {@code childrenCards[i] = |children[i]|} because of
     * how the compilation works, it is already enforced elsewhere.
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

        Variable[] variables = new Variable[children.length * 2 + 4];
        variables[0] = take;
        variables[1] = takeCard;
        variables[2] = to;
        variables[3] = toCard;
        System.arraycopy(children, 0, variables, 4, children.length);
        System.arraycopy(childrenCards, 0, variables, 4 + children.length, childrenCards.length);

        @SuppressWarnings("unchecked")
        Constraint<? extends Variable, Propagator<? extends Variable>> constraint =
                new Constraint(variables, take.getSolver());

        @SuppressWarnings("unchecked")
        Propagator<? extends Variable>[] propagators = new Propagator[]{
            new PropJoinRelation(take, children, to),
            new PropJoinInjectiveRelationCard(take, takeCard, childrenCards, toCard),
            new PropCardinality(to, toCard)
        };
        constraint.setPropagators(propagators);

        return constraint;
    }

    /**
     * A constraint enforcing {@code take.refs = to} where refs is a function.
     * The representation of the function is explained in
     * {@link PropJoinFunction}.
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
     * {@link PropJoinFunction}.
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
        Variable[] variables = new Variable[refs.length + 4];
        variables[0] = take;
        variables[1] = takeCard;
        variables[2] = to;
        variables[3] = toCard;
        // Assumes take card is already constrained for maximum efficiency.
        System.arraycopy(refs, 0, variables, 4, refs.length);

        @SuppressWarnings("unchecked")
        Constraint<? extends Variable, Propagator<? extends Variable>> constraint =
                new Constraint(variables, take.getSolver());

        @SuppressWarnings("unchecked")
        Propagator<? extends Variable>[] propagators = new Propagator[]{
            new PropJoinFunction(take, refs, to),
            new PropJoinFunctionCard(take, takeCard, refs, toCard, globalCardinality),
            new PropCardinality(to, toCard)
        };
        constraint.setPropagators(propagators);

        return constraint;
    }

    /**
     * A constraint enforcing {@code minuend - subtrahend = difference}. Does
     * not enforce that {@code minuendCard = |minuend|} nor
     * {@code subtrahendCard = |subtrahend|} because of how the compilation
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
        Variable[] variables = new Variable[]{
            minuend, minuendCard, subtrahend, subtrahendCard, difference, differenceCard
        };

        @SuppressWarnings("unchecked")
        Constraint<? extends Variable, Propagator<? extends Variable>> constraint =
                new Constraint(variables, difference.getSolver());

        @SuppressWarnings("unchecked")
        Propagator<? extends Variable>[] propagators = new Propagator[]{
            new PropSetDifference(minuend, subtrahend, difference),
            greaterThanEq(minuendCard, differenceCard),
            new PropCardinality(difference, differenceCard)
        };
        constraint.setPropagators(propagators);

        return constraint;
    }

    /**
     * A constraint enforcing
     * {@code operands[0] ∩ operands[1] ∩ ... ∩ operands[n] = intersection}.
     * Does not enforce that {@code operandCards[i] = |operands[i]|} because of
     * how the compilation works, it is already enforced elsewhere.
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

        Variable[] variables = new Variable[operands.length + operandCards.length + 2];
        System.arraycopy(operands, 0, variables, 0, operands.length);
        System.arraycopy(operandCards, 0, variables, operands.length, operandCards.length);
        variables[operands.length + operandCards.length] = intersection;
        variables[operands.length + operandCards.length + 1] = intersectionCard;

        @SuppressWarnings("unchecked")
        Constraint<? extends Variable, Propagator<? extends Variable>> constraint =
                new Constraint(variables, intersection.getSolver());

        @SuppressWarnings("unchecked")
        Propagator<? extends Variable>[] propagators = new Propagator[operandCards.length + 3];
        // See SCF.intersection(operands, intersection);
        // TODO: Needs to add the same propagator twice because the implementation
        // is not guaranteed to be idempotent. If it ever becomes idempotent, then
        // follow their implementation.
        propagators[0] = new PropIntersection(operands, intersection);
        propagators[1] = new PropIntersection(operands, intersection);
        propagators[2] = new PropCardinality(intersection, intersectionCard);
        for (int i = 0; i < operandCards.length; i++) {
            propagators[i + 3] = greaterThanEq(operandCards[i], intersectionCard);
        }
        constraint.setPropagators(propagators);

        return constraint;
    }

    /**
     * A constraint enforcing
     * {@code operands[0] ∪ operands[1] ∪ ... ∪ operands[n] = union}. Does not
     * enforce that {@code operandCards[i] = |operands[i]|} because of how the
     * compilation works, it is already enforced elsewhere.
     *
     * @param operands the operands
     * @param operandCards the cardinalities of {@code operands}
     * @param union the union
     * @param unionCard the cardinality of {@code union}
     * @return constraint
     * {@code operands[0] ∪ operands[1] ∪ ... ∪ operands[n] = union}
     */
    public static Constraint union(SetVar[] operands, IntVar[] operandCards, SetVar union, IntVar unionCard) {
        if (operands.length != operandCards.length) {
            throw new IllegalArgumentException();
        }

        Variable[] variables = new Variable[operands.length + operandCards.length + 2];
        System.arraycopy(operands, 0, variables, 0, operands.length);
        System.arraycopy(operandCards, 0, variables, operands.length, operandCards.length);
        variables[operands.length + operandCards.length] = union;
        variables[operands.length + operandCards.length + 1] = unionCard;

        @SuppressWarnings("unchecked")
        Constraint<? extends Variable, Propagator<? extends Variable>> constraint =
                new Constraint(variables, union.getSolver());

        @SuppressWarnings("unchecked")
        Propagator<? extends Variable>[] propagators = new Propagator[]{
            new PropSetUnion(operands, union),
            new PropSetUnionCard(operandCards, unionCard),
            new PropCardinality(union, unionCard)
        };
        constraint.setPropagators(propagators);

        return constraint;
    }
}
