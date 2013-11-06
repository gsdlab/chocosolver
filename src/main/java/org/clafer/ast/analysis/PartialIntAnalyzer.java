package org.clafer.ast.analysis;

import gnu.trove.list.array.TIntArrayList;
import gnu.trove.set.hash.TIntHashSet;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstBoolExpr;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstConstant;
import org.clafer.ast.AstConstraint;
import org.clafer.ast.AstGlobal;
import org.clafer.ast.AstJoin;
import org.clafer.ast.AstJoinRef;
import org.clafer.ast.AstRef;
import org.clafer.ast.AstSetExpr;
import org.clafer.ast.AstSetTest;
import org.clafer.ast.AstSetTest.Op;
import org.clafer.ast.AstThis;
import org.clafer.ast.AstUpcast;
import org.clafer.ast.AstUtil;
import org.clafer.collection.FList;
import static org.clafer.collection.FList.*;
import org.clafer.collection.Pair;

/**
 *
 * @author jimmy
 */
public class PartialIntAnalyzer implements Analyzer {

    @Override
    public Analysis analyze(Analysis analysis) {
        Map<AstRef, int[][]> partialInts = new HashMap<>();

        List<Pair<FList<AstConcreteClafer>, Integer>> assignments = new ArrayList<>();
        for (AstConstraint constraint : analysis.getConstraints()) {
            AstClafer clafer = constraint.getContext();
            if (constraint.isSoft()) {
                continue;
            }
            try {
                Pair<FList<AstConcreteClafer>, Integer> assignment = analyze(constraint.getExpr());
                FList<AstConcreteClafer> path = assignment.getFst();
                Integer value = assignment.getSnd();
                for (AstConcreteClafer concreteClafer : AstUtil.getConcreteSubs(clafer)) {
                    assignments.add(new Pair<>(snoc(path, concreteClafer), value));
                }
            } catch (NotAssignmentException e) {
                // Only analyze assignments
            }
        }
        AssignmentAutomata automata = new AssignmentAutomata(assignments);
        for (AstClafer clafer : analysis.getClafers()) {
            if (clafer.hasRef()) {
                int scope = analysis.getScope(clafer);
                int[][] ints = new int[scope][];
                for (int i = 0; i < scope; i++) {
                    ints[i] = partialInts(i, clafer.getRef(), automata, analysis);
                }
                partialInts.put(clafer.getRef(), ints);
            }
        }
        return analysis.setPartialIntsMap(partialInts);
    }

    private static int[] partialInts(
            final int id, final AstRef ref, final AssignmentAutomata automata,
            final Analysis analysis) {
        TIntArrayList ints = new TIntArrayList();
        AstClafer clafer = ref.getSourceType();
        if (partialInts(new int[]{id}, clafer, automata, analysis, ints)) {
            return ints.toArray();
        }
        return null;
    }

    private static boolean partialInts(
            final int[] ids, final AstClafer clafer, final AssignmentAutomata automata,
            final Analysis analysis,
            final TIntArrayList ints) {
        if (clafer instanceof AstConcreteClafer) {
            AstConcreteClafer concreteClafer = (AstConcreteClafer) clafer;
            final Automata transition = automata.transition(concreteClafer);
            if (transition instanceof FinalAutomata) {
                ints.add(((FinalAutomata) transition).getValue());
                return true;
            }
            if (transition instanceof AssignmentAutomata) {
                AssignmentAutomata next = (AssignmentAutomata) transition;
                assert !AstUtil.isTop(concreteClafer);
                TIntHashSet parentIds = new TIntHashSet();
                for (int id : ids) {
                    parentIds.addAll(analysis.getPartialSolution(clafer).getPossibleParents(id));
                }
                return partialInts(parentIds.toArray(), concreteClafer.getParent(),
                        next, analysis, ints);
            }
            return false;
        }
        assert clafer instanceof AstAbstractClafer;
        Map<AstConcreteClafer, TIntHashSet> parentIdsMap = new HashMap<>();
        for (int id : ids) {
            Pair<AstConcreteClafer, Integer> concreteId = analysis.getConcreteId(clafer, id);
            TIntHashSet parentIds = parentIdsMap.get(concreteId.getFst());
            if (parentIds == null) {
                parentIds = new TIntHashSet();
                parentIdsMap.put(concreteId.getFst(), parentIds);
            }
            parentIds.add(concreteId.getSnd());
        }
        for (Entry<AstConcreteClafer, TIntHashSet> entry : parentIdsMap.entrySet()) {
            if (!partialInts(entry.getValue().toArray(), entry.getKey(), automata,
                    analysis, ints)) {
                return false;
            }
        }
        return true;
    }

    private static Pair< FList<AstConcreteClafer>, Integer> analyze(
            AstBoolExpr exp) throws NotAssignmentException {
        if (exp instanceof AstSetTest) {
            AstSetTest compare = (AstSetTest) exp;
            if (Op.Equal.equals(compare.getOp())) {
                if (compare.getLeft() instanceof AstJoinRef && compare.getRight() instanceof AstConstant) {
                    return analyzeEqual((AstJoinRef) compare.getLeft(), (AstConstant) compare.getRight());
                }
                if (compare.getRight() instanceof AstJoinRef && compare.getLeft() instanceof AstConstant) {
                    return analyzeEqual((AstJoinRef) compare.getRight(), (AstConstant) compare.getLeft());
                }
            }
        }
        throw new NotAssignmentException();
    }

    private static Pair<FList<AstConcreteClafer>, Integer> analyzeEqual(
            AstJoinRef exp, AstConstant constant) throws NotAssignmentException {
        int[] value = constant.getValue();
        if (value.length == 1) {
            return new Pair<>(analyze(exp), value[0]);
        }
        throw new NotAssignmentException();
    }

    private static FList<AstConcreteClafer> analyze(AstJoinRef exp) throws NotAssignmentException {
        return analyze(exp.getDeref());
    }

    private static FList<AstConcreteClafer> analyze(AstSetExpr exp) throws NotAssignmentException {
        if (exp instanceof AstUpcast) {
            return analyze(((AstUpcast) exp).getBase());
        } else if (exp instanceof AstJoin) {
            AstJoin join = ((AstJoin) exp);
            return cons(join.getRight(), analyze(join.getLeft()));
        } else if (exp instanceof AstThis || exp instanceof AstGlobal) {
            return empty();
        }
        throw new NotAssignmentException();
    }

    private static interface Automata {
    }

    private static class AssignmentAutomata implements Automata {

        private final List<Pair<FList<AstConcreteClafer>, Integer>> assignments;

        AssignmentAutomata(List<Pair<FList<AstConcreteClafer>, Integer>> assignments) {
            this.assignments = assignments;
        }

        public Automata transition(AstConcreteClafer symbol) {
            List<Pair<FList<AstConcreteClafer>, Integer>> next = new ArrayList<>();

            for (Pair<FList<AstConcreteClafer>, Integer> assignment : assignments) {
                FList<AstConcreteClafer> path = assignment.getFst();
                Integer value = assignment.getSnd();
                if (symbol.equals(path.getHead())) {
                    if (path.getTail().isEmpty()) {
                        return new FinalAutomata(value.intValue());
                    }
                    next.add(new Pair<>(path.getTail(), value));
                }
            }
            return next.isEmpty() ? null : new AssignmentAutomata(next);
        }
    }

    private static class FinalAutomata implements Automata {

        private final int value;

        FinalAutomata(int value) {
            this.value = value;
        }

        int getValue() {
            return value;
        }
    }

    private static class NotAssignmentException extends Exception {

        NotAssignmentException() {
        }
    }
}
