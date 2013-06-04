package org.clafer.ast.analysis;

import gnu.trove.list.array.TIntArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstRef;
import org.clafer.collection.Pair;
import org.clafer.graph.GraphUtil;
import org.clafer.graph.KeyGraph;

/**
 *
 * @author jimmy
 */
public class SymmetryAnalyzer implements Analyzer {

    @Override
    public Analysis analyze(Analysis analysis) {
        breakableChildren(analysis);
        return analysis.setBreakableRefs(breakableRefs(analysis));
    }

    // If the Clafer either needs children or reference to be introduce symmetry.
    private Set<AstClafer> breakableChildren(Analysis analysis) {
        Set<AstConcreteClafer> breakableChildren = new HashSet<AstConcreteClafer>();
        for (AstConcreteClafer clafer : analysis.getConcreteClafers()) {
            if (clafer.hasParent()) {
                for (int i = 0; i < analysis.getScope(clafer); i++) {
                }
            }
        }
        return null;
    }

    private Map<AstRef, int[]> breakableRefs(Analysis analysis) {
        KeyGraph<AstClafer> graph = new KeyGraph<AstClafer>();
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
                addDependency(graph, clafer, clafer.getRef().getTargetType(), analysis);
            }
        }

        Map<AstRef, int[]> breakableRefs = new HashMap<AstRef, int[]>();
        for (AstClafer clafer : analysis.getClafers()) {
            if (clafer.hasRef()) {
                if (!GraphUtil.hasPath(
                        graph.getVertex(clafer.getRef().getTargetType()),
                        graph.getVertex(clafer),
                        graph)) {
                    int scope = analysis.getScope(clafer);
                    TIntArrayList breakableIds = new TIntArrayList();
                    for (int i = 0; i < scope; i++) {
                        Pair<AstConcreteClafer, Integer> concreteId = analysis.getConcreteId(clafer, i);
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
                            int[] possibleParents =
                                    analysis.getPartialSolution(concreteClafer).getPossibleParents(id);

                            if (!singleParentScope(concreteClafer.getParent(), possibleParents, analysis)) {
                                breakableIds.add(i);
                            }
                        } else {
                            breakableIds.add(i);
                        }
                    }
                    if (!breakableIds.isEmpty()) {
                        breakableRefs.put(clafer.getRef(), breakableIds.toArray());
                    }
                }
            }
        }

        return breakableRefs;
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
}
