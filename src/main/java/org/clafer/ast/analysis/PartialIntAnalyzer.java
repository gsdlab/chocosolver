package org.clafer.ast.analysis;

import gnu.trove.set.hash.TIntHashSet;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstBoolArithm;
import org.clafer.ast.AstBoolExpr;
import org.clafer.ast.AstChildRelation;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstConstant;
import org.clafer.ast.AstConstraint;
import org.clafer.ast.AstGlobal;
import org.clafer.ast.AstIntClafer;
import org.clafer.ast.AstJoin;
import org.clafer.ast.AstJoinRef;
import org.clafer.ast.AstRef;
import org.clafer.ast.AstSetExpr;
import org.clafer.ast.AstSetTest;
import org.clafer.ast.AstThis;
import org.clafer.ast.AstUpcast;
import org.clafer.ast.AstUtil;
import org.clafer.collection.FList;
import static org.clafer.collection.FList.*;
import org.clafer.collection.Pair;
import org.clafer.domain.Domain;
import org.clafer.domain.Domains;
import org.clafer.scope.Scope;

/**
 *
 * @author jimmy
 */
public class PartialIntAnalyzer implements Analyzer {

    @Override
    public Analysis analyze(Analysis analysis) {
        Map<AstRef, Domain[]> partialInts = new HashMap<>();

        List<Pair<FList<AstConcreteClafer>, Domain>> assignments = new ArrayList<>();
        for (AstConstraint constraint : analysis.getConstraints()) {
            AstClafer clafer = constraint.getContext();
            if (analysis.isSoft(constraint)) {
                continue;
            }
            try {
                Map<FList<AstConcreteClafer>, Domain> assignment = analyze(analysis.getExpr(constraint));
                for (Entry<FList<AstConcreteClafer>, Domain> entry : assignment.entrySet()) {
                    FList<AstConcreteClafer> path = entry.getKey();
                    Domain value = entry.getValue();
                    for (AstConcreteClafer concreteClafer : AstUtil.getConcreteSubs(clafer)) {
                        assignments.add(new Pair<>(snoc(path, concreteClafer), value));
                    }
                }
            } catch (NotAssignmentException e) {
                // Only analyze assignments
            }
        }
        AssignmentAutomata automata = new AssignmentAutomata(assignments);
        for (AstClafer clafer : analysis.getClafers()) {
            if (clafer.hasRef()) {
                int scope = analysis.getScope(clafer);
                Domain[] domains = new Domain[scope];
                for (int i = 0; i < scope; i++) {
                    domains[i] = partialInts(i, clafer.getRef(), automata, analysis);
                }
                partialInts.put(clafer.getRef(), domains);
            }
        }
        return analysis.setPartialIntsMap(partialInts);
    }

    private static Domain partialInts(
            final int id, final AstRef ref, final AssignmentAutomata automata,
            final Analysis analysis) {
        List<Domain> domains = new ArrayList<>();
        AstClafer source = ref.getSourceType();
        AstClafer target = ref.getTargetType();
        if (!partialInts(new int[]{id}, source, automata, analysis, domains)) {
            Scope scope = analysis.getScope();
            Domain unbounded
                    = target instanceof AstIntClafer
                            ? Domains.boundDomain(scope.getIntLow(), scope.getIntHigh())
                            : Domains.fromToDomain(0, scope.getScope(target));
            return unbounded.union(Domains.union(domains));
        }
        return Domains.union(domains);
    }

    private static boolean partialInts(
            final int[] ids, final AstClafer clafer, final AssignmentAutomata automata,
            final Analysis analysis,
            final List<Domain> domains) {
        if (clafer instanceof AstConcreteClafer) {
            AstConcreteClafer concreteClafer = (AstConcreteClafer) clafer;
            final Automata transition = automata.transition(concreteClafer);
            if (transition instanceof FinalAutomata) {
                domains.add(((FinalAutomata) transition).getValue());
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
                        next, analysis, domains);
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
        boolean covered = true;
        for (Entry<AstConcreteClafer, TIntHashSet> entry : parentIdsMap.entrySet()) {
            covered &= partialInts(entry.getValue().toArray(), entry.getKey(), automata, analysis, domains);
        }
        return covered;
    }

    private static Map<FList<AstConcreteClafer>, Domain> analyze(
            AstBoolExpr expr) throws NotAssignmentException {
        if (expr instanceof AstSetTest) {
            AstSetTest compare = (AstSetTest) expr;
            if (AstSetTest.Op.Equal.equals(compare.getOp())) {
                if (compare.getLeft() instanceof AstJoinRef && compare.getRight() instanceof AstConstant) {
                    return analyzeEqual((AstJoinRef) compare.getLeft(), (AstConstant) compare.getRight());
                }
                if (compare.getRight() instanceof AstJoinRef && compare.getLeft() instanceof AstConstant) {
                    return analyzeEqual((AstJoinRef) compare.getRight(), (AstConstant) compare.getLeft());
                }
            }
        } else if (expr instanceof AstBoolArithm) {
            AstBoolArithm arithm = (AstBoolArithm) expr;
            switch (arithm.getOp()) {
                case Or:
                    Map<FList<AstConcreteClafer>, Domain> map = new HashMap<>();
                    for (AstBoolExpr operand : arithm.getOperands()) {
                        Map<FList<AstConcreteClafer>, Domain> operandMap = analyze(operand);
                        for (Entry<FList<AstConcreteClafer>, Domain> entry : operandMap.entrySet()) {
                            Domain domain = map.get(entry.getKey());
                            domain = domain == null ? entry.getValue() : domain.union(entry.getValue());
                            map.put(entry.getKey(), domain);
                        }
                    }
                    return map;
            }
        }
        throw new NotAssignmentException();
    }

    private static Map<FList<AstConcreteClafer>, Domain> analyzeEqual(
            AstJoinRef exp, AstConstant constant) throws NotAssignmentException {
        if (constant.getType().arity() == 1) {
            int[] value = constant.getValue()[0];
            if (value.length == 1) {
                return Collections.singletonMap(analyze(exp), Domains.constantDomain(value[0]));
            }
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
            AstSetExpr right = join.getRight();
            if (right instanceof AstChildRelation) {
                return cons(((AstChildRelation) right).getChildType(), analyze(join.getLeft()));
            }
        } else if (exp instanceof AstThis || exp instanceof AstGlobal) {
            return empty();
        }
        throw new NotAssignmentException();
    }

    private static interface Automata {
    }

    private static class AssignmentAutomata implements Automata {

        private final List<Pair<FList<AstConcreteClafer>, Domain>> assignments;

        AssignmentAutomata(List<Pair<FList<AstConcreteClafer>, Domain>> assignments) {
            this.assignments = assignments;
        }

        public Automata transition(AstConcreteClafer symbol) {
            List<Pair<FList<AstConcreteClafer>, Domain>> next = new ArrayList<>();

            for (Pair<FList<AstConcreteClafer>, Domain> assignment : assignments) {
                FList<AstConcreteClafer> path = assignment.getFst();
                Domain value = assignment.getSnd();
                if (symbol.equals(path.getHead())) {
                    if (path.getTail().isEmpty()) {
                        return new FinalAutomata(value);
                    }
                    next.add(new Pair<>(path.getTail(), value));
                }
            }
            return next.isEmpty() ? null : new AssignmentAutomata(next);
        }
    }

    private static class FinalAutomata implements Automata {

        private final Domain value;

        FinalAutomata(Domain value) {
            this.value = value;
        }

        Domain getValue() {
            return value;
        }
    }

    private static class NotAssignmentException extends Exception {

        NotAssignmentException() {
        }
    }
}
