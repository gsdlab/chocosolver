package org.clafer.ast.analysis;

import gnu.trove.list.array.TIntArrayList;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstRef;
import org.clafer.ast.AstUtil;
import org.clafer.collection.ChainedComparator;
import org.clafer.collection.Pair;
import org.clafer.common.Util;
import org.clafer.graph.GraphUtil;
import org.clafer.graph.KeyGraph;

/**
 * This analyzer determines where symmetry is and is not possible.
 *
 * @author jimmy
 */
public class SymmetryAnalyzer implements Analyzer {

    @Override
    public Analysis analyze(Analysis analysis) {
        return breakableChildren(breakableRefs(analysis));
    }

    /**
     * Find which children need breaking.
     *
     * @param analysis the analysis
     * @return the analysis
     */
    private Analysis breakableChildren(Analysis analysis) {
        Map<AstClafer, AstConcreteClafer[]> breakableChildren = new HashMap<>();
        for (AstAbstractClafer clafer : analysis.getAbstractClafers()) {
            breakableChildren(clafer, breakableChildren, analysis);
        }
        breakableChildren(analysis.getModel(), breakableChildren, analysis);
        return analysis.setBreakableChildrenMap(breakableChildren);
    }

    private boolean breakableChildren(AstClafer clafer, Map<AstClafer, AstConcreteClafer[]> breakableChildren, Analysis analysis) {
        List<AstConcreteClafer> breakables = new ArrayList<>();
        for (AstConcreteClafer child : clafer.getChildren()) {
            if (breakableChildren(child, breakableChildren, analysis)) {
                breakables.add(child);
            }
        }
        breakableChildren.put(clafer, breakables.toArray(new AstConcreteClafer[breakables.size()]));
        AstRef ref = AstUtil.getInheritedRef(clafer);
        return (clafer instanceof AstConcreteClafer && !analysis.getCard((AstConcreteClafer) clafer).isExact())
                || !breakables.isEmpty() || (ref != null && analysis.isBreakableRef(ref));
    }

    /**
     * Find which references need breaking.
     *
     * Does not need breaking.
     * <pre>
     * A
     *     B -> integer
     * </pre>
     *
     * Does need breaking.
     * <pre>
     * A
     *     B -> integer 2
     * </pre>
     *
     * Cannot be broken.
     * <pre>
     * A 2
     *    B -> A
     * </pre>
     *
     * @param analysis the analysis
     * @return the analysis
     */
    private Analysis breakableRefs(final Analysis analysis) {
        // Use this graph to detect when symmetries cannot be broken.
        KeyGraph<AstClafer> graph = new KeyGraph<>();
        List<AstRef> refs = new ArrayList<>();
        for (AstClafer clafer : analysis.getClafers()) {
            if (clafer instanceof AstConcreteClafer) {
                AstConcreteClafer concreteClafer = (AstConcreteClafer) clafer;
                if (concreteClafer.hasParent() && analysis.getScope(concreteClafer.getParent()) > 1) {
                    addDependency(graph, concreteClafer, concreteClafer.getParent(), analysis);
                }
                if (concreteClafer.hasSuperClafer()) {
                    addDependency(graph, concreteClafer, concreteClafer.getSuperClafer(), analysis);
                }
            }
            for (AstConcreteClafer child : clafer.getChildren()) {
                addDependency(graph, clafer, child, analysis);
            }
            if (clafer.hasRef()) {
                refs.add(clafer.getRef());
            }
        }

        Collections.sort(refs, Collections.reverseOrder(new ChainedComparator<>(
                new ConcreteClaferComparator<AstRef>() {

                    @Override
                    AstClafer map(AstRef t) {
                        return t.getSourceType();
                    }

                    @Override
                    int score(AstConcreteClafer clafer) {
                        return analysis.getCard(clafer).getHigh();
                    }
                },
                new ConcreteClaferComparator<AstRef>() {

                    @Override
                    AstClafer map(AstRef t) {
                        return t.getSourceType();
                    }

                    @Override
                    int score(AstConcreteClafer clafer) {
                        return analysis.getCard(clafer).getLow();
                    }
                },
                new Comparator<AstRef>() {

                    @Override
                    public int compare(AstRef o1, AstRef o2) {
                        return Integer.compare(
                                analysis.getScope(o1.getSourceType()),
                                analysis.getScope(o2.getSourceType()));
                    }
                },
                new Comparator<AstRef>() {

                    @Override
                    public int compare(AstRef o1, AstRef o2) {
                        return Integer.compare(
                                analysis.getScope(o1.getTargetType()),
                                analysis.getScope(o2.getTargetType()));
                    }
                }
        )));

        Map<AstRef, int[]> breakableRefsMap = new HashMap<>();
        Map<AstClafer, AstRef[]> breakableTargetsMap = new HashMap<>();
        for (AstRef ref : refs) {
            addDependency(graph, ref.getSourceType(), ref.getTargetType(), analysis);
            if (!GraphUtil.hasPath(
                    graph.getVertex(ref.getTargetType()),
                    graph.getVertex(ref.getSourceType()),
                    graph)) {
                int scope = analysis.getScope(ref.getSourceType());
                TIntArrayList breakableIds = new TIntArrayList();
                for (int i = 0; i < scope; i++) {
                    Pair<AstConcreteClafer, Integer> concreteId = analysis.getConcreteId(ref.getSourceType(), i);
                    AstConcreteClafer concreteClafer = concreteId.getFst();
                    int id = concreteId.getSnd().intValue();

                    if (analysis.getCard(concreteClafer).getHigh() == 1) {
                        /*
                         * It is possible this ref id does not need to be broken.
                         * 
                         * For example:
                         *     abstract Feature
                         *         footprint -> integer
                         *     A : Feature
                         *     B : Feature 2
                         * 
                         * Then the footprints under A do not have symmetry, since
                         * there is only one A. However, the footprints under B
                         * do need to be broken.
                         */
                        int[] possibleParents
                                = analysis.getPartialSolution(concreteClafer).getPossibleParents(id);

                        if (!singleParentScope(concreteClafer.getParent(), possibleParents, analysis)) {
                            breakableIds.add(i);
                        }
                    } else {
                        breakableIds.add(i);
                    }
                }
                if (!breakableIds.isEmpty()) {
                    breakableRefsMap.put(ref, breakableIds.toArray());
                }
            }
            AstRef[] breakableTarget = breakableTargetsMap.get(ref.getTargetType());
            if (breakableTarget == null) {
                breakableTarget = new AstRef[]{ref};
            } else {
                breakableTarget = Util.cons(ref, breakableTarget);
            }
            breakableTargetsMap.put(ref.getTargetType(), breakableTarget);
        }
        Iterator<Entry<AstClafer, AstRef[]>> iter = breakableTargetsMap.entrySet().iterator();
        while (iter.hasNext()) {
            Entry<AstClafer, AstRef[]> entry = iter.next();
            if (singleSourceScope(entry.getValue(), analysis)) {
                iter.remove();
            }
        }
        return analysis.setBreakableRefsMap(breakableRefsMap).setBreakableTargetsMap(breakableTargetsMap);
    }

    private void addDependency(KeyGraph<AstClafer> graph, AstClafer from, AstClafer to, Analysis analysis) {
        if (analysis.getScope(from) > 1 && analysis.getScope(to) > 1) {
            graph.getVertex(from).addNeighbour(graph.getVertex(to));
        }
    }

    private boolean singleParentScope(AstClafer parentType, int[] possibleIds, Analysis analysis) {
        for (int id : possibleIds) {
            Pair<AstConcreteClafer, Integer> concreteId = analysis.getConcreteId(parentType, id);
            if (analysis.getScope(concreteId.getFst()) > 1) {
                return false;
            }
        }
        return true;
    }

    private boolean singleSourceScope(AstRef[] refs, Analysis analysis) {
        int scope = 0;
        for (AstRef ref : refs) {
            scope += analysis.getScope(ref.getSourceType());
            if (scope > 1) {
                return false;
            }
        }
        return true;
    }

    private static abstract class ConcreteClaferComparator<T> implements Comparator<T> {

        @Override
        public int compare(T o1, T o2) {
            return Integer.compare(score(map(o1)), score(map(o2)));
        }

        abstract AstClafer map(T t);

        int score(AstClafer clafer) {
            if (clafer instanceof AstConcreteClafer) {
                return score((AstConcreteClafer) clafer);
            }
            int maxScore = 0;
            for (AstClafer sub : ((AstAbstractClafer) clafer).getSubs()) {
                maxScore = Math.max(maxScore, score(sub));
            }
            return maxScore;
        }

        abstract int score(AstConcreteClafer clafer);
    }
}
