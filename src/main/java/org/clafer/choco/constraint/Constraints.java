package org.clafer.choco.constraint;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import org.chocosolver.solver.Model;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.constraints.Operator;
import org.chocosolver.solver.constraints.Propagator;
import org.chocosolver.solver.constraints.binary.PropEqualXY_C;
import org.chocosolver.solver.constraints.binary.PropEqualX_Y;
import org.chocosolver.solver.constraints.binary.PropEqualX_YC;
import org.chocosolver.solver.constraints.binary.PropGreaterOrEqualXY_C;
import org.chocosolver.solver.constraints.binary.PropGreaterOrEqualX_Y;
import org.chocosolver.solver.constraints.binary.PropNotEqualX_Y;
import org.chocosolver.solver.constraints.nary.element.PropElementV_fast;
import org.chocosolver.solver.constraints.nary.sum.PropSum;
import org.chocosolver.solver.constraints.set.PropIntEnumMemberSet;
import org.chocosolver.solver.constraints.set.PropIntersection;
import org.chocosolver.solver.constraints.set.PropSubsetEq;
import org.chocosolver.solver.constraints.unary.PropEqualXC;
import org.chocosolver.solver.constraints.unary.PropGreaterOrEqualXC;
import org.chocosolver.solver.constraints.unary.PropLessOrEqualXC;
import org.chocosolver.solver.constraints.unary.PropNotEqualXC;
import org.chocosolver.solver.variables.BoolVar;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.solver.variables.SetVar;
import org.chocosolver.solver.variables.Variable;
import org.chocosolver.util.ESat;
import org.chocosolver.util.objects.setDataStructures.ISetIterator;
import org.clafer.choco.constraint.propagator.PropAcyclic;
import org.clafer.choco.constraint.propagator.PropAnd;
import org.clafer.choco.constraint.propagator.PropArrayToSet;
import org.clafer.choco.constraint.propagator.PropArrayToSetCard;
import org.clafer.choco.constraint.propagator.PropAtMostTransitiveClosure;
import org.clafer.choco.constraint.propagator.PropContainsImpliesEqual;
import org.clafer.choco.constraint.propagator.PropContainsImpliesEqualCard;
import org.clafer.choco.constraint.propagator.PropContainsImpliesEqualCard2;
import org.clafer.choco.constraint.propagator.PropContinuous;
import org.clafer.choco.constraint.propagator.PropContinuousUnion;
import org.clafer.choco.constraint.propagator.PropCountNotEqual;
import org.clafer.choco.constraint.propagator.PropElement;
import org.clafer.choco.constraint.propagator.PropElementArraySupport;
import org.clafer.choco.constraint.propagator.PropElementValueSupport;
import org.clafer.choco.constraint.propagator.PropEqualXY_Z;
import org.clafer.choco.constraint.propagator.PropIntChannel;
import org.clafer.choco.constraint.propagator.PropIntMemberNonemptySet;
import org.clafer.choco.constraint.propagator.PropIntMemberSetCard;
import org.clafer.choco.constraint.propagator.PropIntMemberSetDefault;
import org.clafer.choco.constraint.propagator.PropJoinFunction;
import org.clafer.choco.constraint.propagator.PropJoinFunctionCard;
import org.clafer.choco.constraint.propagator.PropJoinInjectiveRelationCard;
import org.clafer.choco.constraint.propagator.PropJoinRelation;
import org.clafer.choco.constraint.propagator.PropLength;
import org.clafer.choco.constraint.propagator.PropLexChainChannel;
import org.clafer.choco.constraint.propagator.PropLone;
import org.clafer.choco.constraint.propagator.PropMask;
import org.clafer.choco.constraint.propagator.PropMaskCard;
import org.clafer.choco.constraint.propagator.PropNotEqualXY_Z;
import org.clafer.choco.constraint.propagator.PropOne;
import org.clafer.choco.constraint.propagator.PropOr;
import org.clafer.choco.constraint.propagator.PropReflexive;
import org.clafer.choco.constraint.propagator.PropSamePrefix;
import org.clafer.choco.constraint.propagator.PropSelectN;
import org.clafer.choco.constraint.propagator.PropSetBounded;
import org.clafer.choco.constraint.propagator.PropSetDifference;
import org.clafer.choco.constraint.propagator.PropSetLowBound;
import org.clafer.choco.constraint.propagator.PropSetMax;
import org.clafer.choco.constraint.propagator.PropSetMin;
import org.clafer.choco.constraint.propagator.PropSetNotEqualC;
import org.clafer.choco.constraint.propagator.PropSetStrictHighBound;
import org.clafer.choco.constraint.propagator.PropSetSum;
import org.clafer.choco.constraint.propagator.PropSetTernary;
import org.clafer.choco.constraint.propagator.PropSetUnion;
import org.clafer.choco.constraint.propagator.PropSetUnionCard;
import org.clafer.choco.constraint.propagator.PropSingleton;
import org.clafer.choco.constraint.propagator.PropSingletonFilter;
import org.clafer.choco.constraint.propagator.PropSubsetEqCard;
import org.clafer.choco.constraint.propagator.PropTernary;
import org.clafer.choco.constraint.propagator.PropTransitive;
import org.clafer.choco.constraint.propagator.PropTransitiveUnreachable;
import org.clafer.choco.constraint.propagator.PropUnreachable;
import org.clafer.choco.constraint.propagator.PropUtil;
import org.clafer.common.Util;

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
            return eq(r, l.getValue());
        }
        if (r.isInstantiated()) {
            return eq(l, r.getValue());
        }
        return Optional.of(new PropEqualX_Y(l, r));
    }

    private static Optional<Propagator<IntVar>> eq(IntVar l, int r) {
        if (l.isInstantiatedTo(r)) {
            return Optional.empty();
        }
        return Optional.of(new PropEqualXC(l, r));
    }

    private static Optional<Propagator<IntVar>> neq(IntVar l, IntVar r) {
        if (l.isInstantiated()) {
            return neq(r, l.getValue());
        }
        if (r.isInstantiated()) {
            return neq(l, r.getValue());
        }
        return Optional.of(new PropNotEqualX_Y(l, r));
    }

    private static Optional<Propagator<IntVar>> neq(IntVar l, int r) {
        if (l.isInstantiated() && l.getValue() != r) {
            return Optional.empty();
        }
        return Optional.of(new PropNotEqualXC(l, r));
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
                return new PropSum(Util.snoc(filtered, sum), filter.size(), Operator.EQ, -constant);
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
        return new Constraint("and", new PropAnd(operands)) {
            @Override
            public Constraint makeOpposite() {
                BoolVar[] nots = new BoolVar[operands.length];
                for (int i = 0; i < nots.length; i++) {
                    nots[i] = operands[i].not();
                }
                return or(nots);
            }
        };
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
        return new Constraint("or", new PropOr(operands)) {

            @Override
            public Constraint makeOpposite() {
                BoolVar[] nots = new BoolVar[operands.length];
                for (int i = 0; i < nots.length; i++) {
                    nots[i] = operands[i].not();
                }
                return and(nots);
            }
        };
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
        return new Constraint("ifThenElse",
                new PropGreaterOrEqualX_Y(new IntVar[]{consequent, antecedent}),
                new PropGreaterOrEqualXY_C(new IntVar[]{antecedent, alternative}, 1)) {
                    @Override
                    public Constraint makeOpposite() {
                        return ifThenElse(antecedent, consequent.not(), alternative.not());
                    }

                    @Override
                    public ESat isSatisfied() {
                        if (antecedent.isInstantiated()) {
                            return antecedent.getValue() == 1 ? consequent.getBooleanValue() : alternative.getBooleanValue();
                        }
                        if (consequent.isInstantiatedTo(1) && alternative.isInstantiatedTo(1)) {
                            return ESat.TRUE;
                        }
                        if (consequent.isInstantiatedTo(0) && alternative.isInstantiatedTo(0)) {
                            return ESat.FALSE;
                        }
                        return ESat.UNDEFINED;
                    }
                };
    }

    public static Constraint ternary(BoolVar antecedent, IntVar result, IntVar consequent, IntVar alternative) {
        return new Constraint("Ternary", new PropTernary(antecedent, result, consequent, alternative));
    }

    public static Constraint ternary(BoolVar antecedent, SetVar result, SetVar consequent, SetVar alternative) {
        return new Constraint("Ternary",
                new PropSetTernary(antecedent, result, consequent, alternative),
                new PropTernary(antecedent, result.getCard(), consequent.getCard(), alternative.getCard())
        );
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

//    public static Constraint equalSupport(final IntVar x, final IntVar y, final int c) {
//        return new Constraint("equalArcConsistent", new PropEqualX_YSupportC(x, y, c)) {
//            @Override
//            public Constraint makeOpposite() {
//                return new Constraint("notEqualSupport", new PropNotEqualX_Y(x, y), new PropNotEqualXC(x, c));
//            }
//        };
//    }
//
//    public static Constraint element(IntVar value, IntVar[] array, IntVar index, int offset) {
//        return new Constraint("element", new PropElement(value, array, index, offset));
//    }
//
//    public static Constraint elementArraySupport(IntVar value, IntVar[] array, IntVar index, int offset, int support) {
//        return new Constraint("elementArraySupport", new PropElementArraySupport(value, array, index, offset, support));
//    }
//
//    public static Constraint elementValueSupport(IntVar value, IntVar[] array, IntVar index, int offset, int support) {
//        return new Constraint("elementValueSupport", new PropElementValueSupport(value, array, index, offset, support));
//    }
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
                new PropSubsetEqCard(sub, subCard, sup, supCard));
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
     * {@code sets[i] = \{bounds[i - 1], bound[i - 1] + 1, ..., bounds[i] - 1\}}
     * where {@code bounds[-1]} is interpreted as 0.
     *
     * @param sets the sets
     * @param setCards the cardinalities of {@code sets}
     * @param bounds the bounds
     * @return constraint
     * {@code sets[i] = \{bounds[i - 1], bound[i - 1] + 1, ..., bounds[i] - 1\}}.
     */
    public static Constraint sortedSets(SetVar[] sets, IntVar[] setCards, IntVar[] bounds) {
        if (sets.length != setCards.length) {
            throw new IllegalArgumentException();
        }
        Model model = sets[0].getModel();

        List<Propagator<?>> propagators = new ArrayList<>();

        IntVar[] boundary = new IntVar[sets.length + 1];
        boundary[0] = model.intVar(0);
        for (int i = 0; i < sets.length; i++) {
            if (boundary[i].isInstantiatedTo(0)) {
                boundary[i + 1] = setCards[i];
            } else if (setCards[i].isInstantiatedTo(0)) {
                boundary[i + 1] = boundary[i];
            } else {
                boundary[i + 1] = bounds[i]; //enumerated(sets[i].getName() + "@Hb", lb, ub, solver);
                propagators.add(new PropEqualXY_Z(boundary[i], setCards[i], boundary[i + 1]));
            }
            if (!boundary[i + 1].equals(bounds[i])) {
                propagators.add(new PropEqualX_Y(boundary[i + 1], bounds[i]));
            }
            propagators.add(new PropSetLowBound(sets[i], boundary[i]));
            propagators.add(new PropIntMemberNonemptySet(boundary[i], sets[i], setCards[i]));
            propagators.add(new PropSetStrictHighBound(sets[i], boundary[i + 1]));
            propagators.add(new PropSetBounded(boundary[i], boundary[i + 1], sets[i]));
        }
        for (int i = 0; i < sets.length; i++) {
            propagators.add(new PropContinuous(sets[i], setCards[i]));
        }
        if (boundary[boundary.length - 1].getUB() < 0) {
            return model.falseConstraint();
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

    public static Constraint connected(SetVar nodes, SetVar[] edges, boolean directed) {
        return nodes.getModel().trueConstraint();
//        int nodes_upper = PropUtil.maxEnv(nodes) + 1;
//        boolean fixed_nodes = nodes.isInstantiated();
//
//        UndirectedGraph GLB = new UndirectedGraph(s, nodes_upper, SetType.BITSET, fixed_nodes);
//        UndirectedGraph GUB = new UndirectedGraph(s, nodes_upper, SetType.BITSET, fixed_nodes);
//
//        //add nodes
//        if (!fixed_nodes) {
//            for (int n = nodes.getKernelFirst(); n != SetVar.END; n = nodes.getKernelNext()) {
//                GLB.addNode(n);
//            }
//            for (int n = nodes.getEnvelopeFirst(); n != SetVar.END; n = nodes.getEnvelopeNext()) {
//                GUB.addNode(n);
//            }
//        }
//        //add edges
//        for (int i = 0; i < edges.length; i++) {
//            SetVar edge = edges[i];
//            for (int n = edge.getKernelFirst(); n != SetVar.END; n = edge.getKernelNext()) {
//                if (n < nodes_upper) {
//                    GLB.addEdge(i, n);
//                }
//            }
//            for (int n = edge.getEnvelopeFirst(); n != SetVar.END; n = edge.getEnvelopeNext()) {
//                if (n < nodes_upper) {
//                    GUB.addEdge(i, n);
//                }
//            }
//        }
//
//        IUndirectedGraphVar g = GraphVarFactory.undirected_graph_var("ConnectedVar" + varNum++, GLB, GUB, s);
//        s.post(GCF.nodes_channeling(g, nodes));
//        s.post(GCF.neighbors_channeling(g, edges));
//
//        return GCF.connected(g);
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
        if (svarCard.isInstantiatedTo(1)) {
            return new Constraint("singleton",
                    new PropSingleton(ivar, svar));
        }
        return new Constraint("singleton",
                new PropSingleton(ivar, svar),
                new PropEqualXC(svarCard, 1));
    }

    /**
     * A constraint enforcing
     * {@code if ivar = filter then {} = svar else {ivar} = svar} and
     * {@code svarCard = 1}. Does not enforce that {@code svarCard = |svarCard|}
     * because of how the compilation works, it is already enforced elsewhere.
     *
     * @param ivar the integer
     * @param svar the singleton set
     * @param svarCard the cardinality of {@code svar}
     * @param filter
     * @return constraint
     * {@code if ivar = filter then {} = svar else {ivar} = svar}
     */
    public static Constraint singletonFilter(IntVar ivar, SetVar svar, IntVar svarCard, int filter) {
        if (!ivar.contains(filter) && !svar.getUB().contains(filter)) {
            return singleton(ivar, svar, svarCard);
        }
        return new Constraint("singletonFilter",
                new PropSingletonFilter(ivar, svar, filter),
                new PropEqualX_Y(svarCard, ivar.getModel().arithm(ivar, "!=", filter).reify())) {

                    @Override
                    public ESat isSatisfied() {
                        if (svarCard.getLB() > 1) {
                            return ESat.FALSE;
                        }
                        if (svar.getLB().size() == 1) {
                            if (svar.getLB().min() == filter || ivar.isInstantiatedTo(filter)) {
                                return ESat.FALSE;
                            }
                            if (ivar.contains(svar.getLB().min())) {
                                return ivar.isInstantiated() ? ESat.TRUE : ESat.UNDEFINED;
                            } else {
                                return ESat.FALSE;
                            }
                        }
                        if (!svar.getCard().contains(1)) {
                            if (!ivar.contains(filter)) {
                                return ESat.FALSE;
                            }
                            return ivar.isInstantiated() && svar.isInstantiated() ? ESat.TRUE : ESat.UNDEFINED;
                        } else if (!svar.getCard().contains(0)) {
                            ISetIterator iter = svar.getUB().iterator();
                            while (iter.hasNext()) {
                                int env = iter.nextInt();
                                if (env != filter && ivar.contains(env)) {
                                    return ivar.isInstantiated() && svar.isInstantiated() ? ESat.TRUE : ESat.UNDEFINED;
                                }
                            }
                            return ESat.FALSE;
                        } else if (ivar.contains(filter)) {
                            if (ivar.isInstantiated()) {
                                if (svar.getUB().isEmpty()) {
                                    return ESat.TRUE;
                                }
                                if (!svar.getLB().isEmpty()) {
                                    return ESat.FALSE;
                                }
                            }
                            return ESat.UNDEFINED;
                        } else if (PropUtil.isDomIntersectEnv(ivar, svar)) {
                            return ivar.isInstantiated() && svar.isInstantiated() ? ESat.TRUE : ESat.UNDEFINED;
                        }
                        return ESat.FALSE;
                    }
                };
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
        return new Constraint("max",
                new PropGreaterOrEqualX_Y(new IntVar[]{
                    set.getModel().arithm(max, "=", d).reify(), set.getModel().arithm(setCard, "=", 0).reify()
                }),
                new PropSetMax(set, setCard, max));
    }

    public static Constraint stritctHighBound(SetVar set, IntVar bound) {
        return new Constraint("strictHighBound", new PropSetStrictHighBound(set, bound));
    }

    public static Constraint min(SetVar set, IntVar setCard, IntVar min) {
        if (setCard.getUB() <= 1) {
            return member(min, set);
        }
        return new Constraint("min",
                new PropSetMin(set, setCard, min, 0),
                new PropIntEnumMemberSet(set, min),
                new PropIntMemberSetCard(min, set, setCard));
    }

    public static Constraint min(SetVar set, IntVar setCard, IntVar min, int d) {
        if (setCard.getLB() > 0) {
            return min(set, setCard, min);
        }
        return new Constraint("min",
                new PropGreaterOrEqualX_Y(new IntVar[]{
                    set.getModel().arithm(min, "=", d).reify(), set.getModel().arithm(setCard, "=", 0).reify()
                }),
                new PropIntMemberSetDefault(min, set, setCard, d),
                new PropSetMin(set, setCard, min, d));
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
                new PropElement(valueCard, arrayCards, index, 0),
                new PropElement(valueCard, arrayCards, index, 0));
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
                new PropMaskCard(set, setCard, masked, maskedCard, from, to));
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
            return length1.getModel().trueConstraint();
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

    public static Constraint equal(IntVar[] chars1, IntVar[] chars2) {
        if (chars1.length != chars2.length) {
            throw new IllegalArgumentException();
        }
        if (chars1.length == 0) {
            throw new IllegalArgumentException();
        }
        List<Propagator> propagators = new ArrayList<>(chars1.length);
        for (int i = 0; i < chars1.length; i++) {
            eq(chars1[i], chars2[i]).ifPresent(propagators::add);
        }
        if (propagators.isEmpty()) {
            return chars1[0].getModel().trueConstraint();
        }
        return new Constraint("ArrayEqual", propagators.toArray(new Propagator[propagators.size()]));
    }

    public static Constraint lessThan(IntVar[] chars1, IntVar[] chars2) {
        int maxLength = Math.max(chars1.length, chars2.length);
        return chars1[0].getModel().lexLess(
                pad(chars1, maxLength, chars1[0].getModel().intVar(0)),
                pad(chars2, maxLength, chars1[0].getModel().intVar(0)));
    }

    public static Constraint lessThanEqual(IntVar[] chars1, IntVar[] chars2) {
        int maxLength = Math.max(chars1.length, chars2.length);
        return chars1[0].getModel().lexLessEq(
                pad(chars1, maxLength, chars1[0].getModel().intVar(0)),
                pad(chars2, maxLength, chars1[0].getModel().intVar(0)));
    }

    private static IntVar[] charsAt(Model model, IntVar[][] strings, int index) {
        IntVar[] charsAt = new IntVar[strings.length];
        for (int i = 0; i < charsAt.length; i++) {
            charsAt[i] = index < strings[i].length
                    ? strings[i][index] : model.intVar(0);
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
            IntVar[] charsAt = charsAt(index.getModel(), array, i);
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
            return prefixLength.getModel().falseConstraint();
        }
        return new Constraint("Prefix",
                lessThanEq(prefixLength, wordLength),
                new PropSamePrefix(prefixLength, prefix, word));
    }

    public static Constraint suffix(
            IntVar[] suffix, IntVar suffixLength,
            IntVar[] word, IntVar wordLength) {
        Model model = suffixLength.getModel();
        if (suffixLength.getLB() > wordLength.getUB()) {
            return model.falseConstraint();
        }
        IntVar prefixLength = model.intVar("SuffixVar" + varNum++,
                Math.min(wordLength.getLB() - suffixLength.getUB(), 0),
                wordLength.getUB() - suffixLength.getLB(), false);
        model.post(new Constraint("SuffixVarSum",
                sumEq(new IntVar[]{prefixLength, suffixLength}, wordLength)));
        List<Propagator<IntVar>> propagators = new ArrayList<>();
        propagators.add(lessThanEq(suffixLength, wordLength));
        for (int i = 0; i < suffix.length; i++) {
            IntVar[] pad = pad(word, prefixLength.getUB() + i + 1, model.intVar(0));
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
            return leftLength.getModel().falseConstraint();
        }
        List<Propagator<IntVar>> propagators = new ArrayList<>();
        propagators.add(sumEq(new IntVar[]{leftLength, rightLength}, concatLength));
        propagators.add(new PropSamePrefix(leftLength, left, concat));
        for (int i = 0; i < right.length; i++) {
            IntVar[] pad = pad(concat, left.length + i + 1, leftLength.getModel().intVar(0));
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

    public static Constraint subarray(IntVar[] subarray, IntVar sublength, IntVar index, IntVar[] suparray) {
        if (sublength.getUB() < 0 || index.getUB() < 0 || suparray.length == 0) {
            return sublength.getModel().falseConstraint();
        }
        if (subarray.length == 0) {
            return new Constraint(null, new PropEqualXC(sublength, 0), new PropLessOrEqualXC(index, suparray.length - 1));
        }
        List<Propagator<IntVar>> propagators = new ArrayList<>();
        for (int i = 0; i < subarray.length; i++) {
            IntVar[] pad = new IntVar[suparray.length];
            int j;
            for (j = 0; j < pad.length && j + i < suparray.length; j++) {
                pad[j] = suparray[j + i];
            }
            for (; j < pad.length; j++) {
                pad[j] = sublength.getModel().intVar(-1);
            }
            propagators.add(new PropElementValueSupport(subarray[i], pad, index, 0, -1));
        }
        for (int i = 0; i < suparray.length; i++) {
            IntVar[] pad = new IntVar[suparray.length + 1];
            int j;
            for (j = 0; j < i && i - j >= 0 && i - j < subarray.length; j++) {
                pad[j] = subarray[i - j];
            }
            for (; j < pad.length; j++) {
                pad[j] = sublength.getModel().intVar(-1);
            }
            propagators.add(new PropElementArraySupport(suparray[i], pad, index, 0, -1));
        }
        propagators.add(new PropLength(subarray, sublength, -1));
        return new Constraint("Substring",
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

    public static Constraint containsImpliesEqualTest(SetVar cond, int z,
            SetVar x, IntVar xCard, SetVar y, IntVar yCard) {
        return new Constraint("containsImpliesEqual",
                new PropContainsImpliesEqual(cond, z, x, y),
                new PropContainsImpliesEqualCard(cond, z, xCard, yCard),
                new PropContainsImpliesEqualCard2(x, xCard, y)
        );
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
