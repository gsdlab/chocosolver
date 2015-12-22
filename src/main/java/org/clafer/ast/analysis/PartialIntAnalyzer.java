package org.clafer.ast.analysis;

import gnu.trove.set.hash.TIntHashSet;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Deque;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.Stack;
import java.util.stream.Collectors;
import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstBoolArithm;
import org.clafer.ast.AstBoolExpr;
import org.clafer.ast.AstChildRelation;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstConstant;
import org.clafer.ast.AstConstraint;
import org.clafer.ast.AstExpr;
import org.clafer.ast.AstGlobal;
import org.clafer.ast.AstIntClafer;
import org.clafer.ast.AstJoin;
import org.clafer.ast.AstJoinParent;
import org.clafer.ast.AstJoinRef;
import org.clafer.ast.AstRef;
import org.clafer.ast.AstSetExpr;
import org.clafer.ast.AstSetTest;
import org.clafer.ast.AstTernary;
import org.clafer.ast.AstThis;
import org.clafer.ast.AstUpcast;
import org.clafer.ast.AstUtil;
import org.clafer.collection.FList;
import static org.clafer.collection.FList.*;
import org.clafer.collection.Pair;
import org.clafer.common.Util;
import org.clafer.domain.Domain;
import org.clafer.domain.Domains;
import org.clafer.scope.Scope;

/**
 *
 * @author jimmy
 */
public class PartialIntAnalyzer {

    private final AstClafer context;
    private final Analysis analysis;

    private PartialIntAnalyzer(AstClafer context, Analysis analysis) {
        this.context = context;
        this.analysis = analysis;
    }

    public static Analysis analyze(Analysis analysis) {
        Map<AstRef, Domain[]> partialInts = new HashMap<>();

        List<Pair<FList<AstConcreteClafer>, Domain>> assignments = new ArrayList<>();
        Map<FList<AstConcreteClafer>, Pair<Domain, Set<AstClafer>>> conditionAssignments = new HashMap<>();
        for (Entry<AstConstraint, AstBoolExpr> pair : analysis.getConstraintExprs().entrySet()) {
            AstConstraint constraint = pair.getKey();
            if (analysis.isSoft(constraint)) {
                continue;
            }
            AstClafer clafer = constraint.getContext();
            PartialIntAnalyzer analyzer = new PartialIntAnalyzer(clafer, analysis);
            try {
                Map<Path, Domain> assignment = analyzer.analyze(pair.getValue());
                for (Entry<Path, Domain> entry : assignment.entrySet()) {
                    Path path = entry.getKey();
                    Domain value = entry.getValue();

                    for (FList<AstConcreteClafer> concretePath : concretize(dropConcreteSuffix(path.path))) {
                        if (path.condition.isEmpty()) {
                            assignments.add(new Pair<>(concretePath, value));
                        } else {
                            /**
                             * Condition assignments are when a variable is
                             * assigned under all alternatives. For example,
                             * consider the following model.
                             *
                             * <pre>
                             * A -> int
                             * xor B
                             *     C
                             *         [ A = 3 ]
                             *     D
                             *         [ A = 4 ]
                             * </pre>
                             *
                             * In this model, A must be assigned to either 3 or
                             * 4. Note that if one of the constraints above were
                             * removed, we would know nothing about the values
                             * of A.
                             */
                            conditionAssignments.merge(concretePath, new Pair<>(value, path.condition),
                                    (previous, current) -> new Pair<>(
                                            previous.getFst().union(current.getFst()),
                                            Util.union(previous.getSnd(), current.getSnd())));
                        }
                    }
                }
            } catch (NotAssignmentException e) {
                // Only analyze assignments
            }
        }
        for (Entry<FList<AstConcreteClafer>, Pair<Domain, Set<AstClafer>>> conditionAssignment : conditionAssignments.entrySet()) {
            if (isConditionCovered(conditionAssignment.getValue().getSnd())) {
                assignments.add(new Pair(conditionAssignment.getKey(), conditionAssignment.getValue().getFst()));
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

    private static boolean isConditionCovered(Set<AstClafer> condition) {
        for (Entry<AstClafer, Set<AstClafer>> group
                : condition.stream().collect(Collectors.groupingBy(AstClafer::getParent, Collectors.toSet())).entrySet()) {
            if (AstUtil.getConcreteSubs(group.getKey()).stream().flatMap(x -> x.getChildren().stream()).allMatch(group.getValue()::contains)) {
                return true;
            }
        }
        return false;
    }

    private static FList<AstClafer> dropConcreteSuffix(FList<AstClafer> path) {
        Deque<AstClafer> stack = new ArrayDeque<>();
        path.forEach(stack::push);
        while (stack.size() > 1) {
            AstClafer top = stack.pop();
            if (!top.equals(stack.peek().getParent())) {
                stack.push(top);
                break;
            }
        }
        return FList.fromIterable(stack);
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

    private Map<Path, Domain> analyze(AstBoolExpr expr) throws NotAssignmentException {
        if (expr instanceof AstSetTest) {
            AstSetTest compare = (AstSetTest) expr;
            if (AstSetTest.Op.Equal.equals(compare.getOp())) {
                try {
                    if (compare.getLeft() instanceof AstJoinRef) {
                        return analyzeEqual((AstJoinRef) compare.getLeft(), compare.getRight());
                    }
                } catch (NotAssignmentException e) {
                    if (compare.getRight() instanceof AstJoinRef) {
                        return analyzeEqual((AstJoinRef) compare.getRight(), compare.getLeft());
                    }
                }
            }
        } else if (expr instanceof AstBoolArithm) {
            AstBoolArithm arithm = (AstBoolArithm) expr;
            switch (arithm.getOp()) {
                case Or:
                    AstBoolExpr[] operands = arithm.getOperands();
                    Map<Path, Domain> map = new HashMap<>(analyze(operands[0]));
                    for (int i = 1; i < operands.length; i++) {
                        Map<Path, Domain> operandMap = analyze(operands[i]);
                        map.keySet().retainAll(operandMap.keySet());
                        for (Entry<Path, Domain> entry : operandMap.entrySet()) {
                            if (map.containsKey(entry.getKey())) {
                                map.put(entry.getKey(), map.get(entry.getKey()).union(entry.getValue()));
                            }
                        }
                    }
                    return map;
            }
        }
        throw new NotAssignmentException();
    }

    private Domain analyzeDomain(AstExpr expr) throws NotAssignmentException {
        if (expr instanceof AstConstant) {
            AstConstant constant = (AstConstant) expr;
            if (constant.getType().arity() == 1) {
                int[] constantValue = constant.getValue()[0];
                if (constantValue.length == 1) {
                    return Domains.constantDomain(constantValue[0]);
                }
            }
        } else if (expr instanceof AstTernary) {
            AstTernary ternary = (AstTernary) expr;
            return analyzeDomain(ternary.getConsequent()).union(analyzeDomain(ternary.getAlternative()));
        }
        throw new NotAssignmentException();
    }

    private Map<Path, Domain> analyzeEqual(
            AstJoinRef var, AstExpr value) throws NotAssignmentException {
        return Collections.singletonMap(analyze(var), analyzeDomain(value));
    }

    private Path analyze(AstJoinRef exp) throws NotAssignmentException {
        return analyze(exp.getDeref());
    }

    private Path analyze(AstSetExpr exp) throws NotAssignmentException {
        if (exp instanceof AstUpcast) {
            return analyze(((AstUpcast) exp).getBase());
        } else if (exp instanceof AstJoin) {
            AstJoin join = ((AstJoin) exp);
            AstSetExpr right = join.getRight();
            if (right instanceof AstChildRelation) {
                AstChildRelation relation = (AstChildRelation) right;
                // TODO what if is abstract not concrete?
                if (relation.getChildType() instanceof AstConcreteClafer) {
                    return analyze(join.getLeft()).cons((AstConcreteClafer) relation.getChildType());
                }
            }
        } else if (exp instanceof AstJoinParent) {
            Set<AstClafer> conditions = new HashSet<>(0);
            AstClafer type = analysis.getType(exp).getClaferType();
            do {
                AstJoinParent joinParent = (AstJoinParent) exp;
                exp = joinParent.getChildren();
                AstClafer childType = analysis.getType(exp).getClaferType();
                // TODO what if abstract?
                if (childType instanceof AstConcreteClafer) {
                    AstConcreteClafer concreteType = (AstConcreteClafer) childType;
                    if (concreteType.getCard().getLow() == 0) {
                        if (!concreteType.getParent().hasGroupCard() || concreteType.getParent().getGroupCard().getLow() == 0) {
                            throw new NotAssignmentException();
                        }
                        conditions.add(concreteType);
                    }
                }
            } while (exp instanceof AstJoinParent);
            if (exp instanceof AstThis) {
                return new Path(conditions, single(type));
            }
        } else if (exp instanceof AstThis) {
            return new Path(single(context));
        } else if (exp instanceof AstGlobal || exp instanceof AstConstant) {
            AstClafer type;
            if (exp instanceof AstGlobal) {
                AstGlobal global = (AstGlobal) exp;
                type = global.getType();
            } else {
                AstConstant constant = (AstConstant) exp;
                type = constant.getType().getClaferType();
                if (constant.getValue().length != analysis.getScope(type)) {
                    throw new IllegalArgumentException();
                }
            }
            Set<AstClafer> conditions = new HashSet<>();
            AstClafer current = context;
            // TODO what if abstract?
            List<AstClafer> ancestors = AstUtil.getAncestors(type);
            while (current instanceof AstConcreteClafer && !ancestors.contains(current)) {
                if (((AstConcreteClafer) current).getCard().getLow() == 0) {
                    if (!current.getParent().hasGroupCard() || current.getParent().getGroupCard().getLow() == 0) {
                        throw new NotAssignmentException();
                    }
                    conditions.add(current);
                }
                current = current.getParent();
            }
            return new Path(conditions, single(type));
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

    private static FList<AstConcreteClafer>[] concretize(FList<AstClafer> path) {
        if (path.isEmpty()) {
            return new FList[]{empty()};
        }
        FList<AstConcreteClafer>[] tails = concretize(path.getTail());
        List<AstConcreteClafer> heads = AstUtil.getConcreteSubs(path.getHead());
        FList<AstConcreteClafer>[] paths = new FList[tails.length * heads.size()];
        int i = 0;
        for (AstConcreteClafer head : heads) {
            for (FList<AstConcreteClafer> tail : tails) {
                paths[i++] = cons(head, tail);
            }
        }
        assert i == paths.length;
        return paths;
    }

    private static final Path EMPTY = new Path();

    private static class Path {

        private final Set<AstClafer> condition;
        private final FList<AstClafer> path;

        public Path() {
            this(empty());
        }

        public Path(FList<AstClafer> path) {
            this(Collections.emptySet(), path);
        }

        public Path(Set<AstClafer> condition, FList<AstClafer> concretePath) {
            assert condition.stream().map(AstClafer::getParent).allMatch(AstClafer::hasGroupCard);
            this.condition = condition;
            this.path = concretePath;
        }

        public Path cons(AstClafer clafer) {
            return new Path(condition, FList.cons(clafer, path));
        }

        @Override
        public boolean equals(Object obj) {
            if (obj instanceof Path) {
                Path other = (Path) obj;
                return condition.equals(other.condition) && path.equals(other.path);
            }
            return false;
        }

        @Override
        public int hashCode() {
            return condition.hashCode() ^ path.hashCode();
        }
    }
}
