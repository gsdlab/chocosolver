package org.clafer.ast.analysis;

import java.util.HashSet;
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

    private Set<AstRef> breakableRefs(Analysis analysis) {
        KeyGraph<AstClafer> graph = new KeyGraph<AstClafer>();
        for (AstClafer clafer : analysis.getClafers()) {
            if (clafer instanceof AstConcreteClafer) {
                AstConcreteClafer concreteClafer = (AstConcreteClafer) clafer;
                if (concreteClafer.hasParent() && analysis.getScope(concreteClafer.getParent()) > 1) {
                    graph.getVertex(concreteClafer).addNeighbour(graph.getVertex(concreteClafer.getParent()));
                }
                if (concreteClafer.hasSuperClafer()) {
                    graph.getVertex(concreteClafer).addNeighbour(graph.getVertex(concreteClafer.getSuperClafer()));
                }
            }
            if (analysis.getScope(clafer) > 1) {
                for (AstConcreteClafer child : clafer.getChildren()) {
                    if (analysis.getScope(child) > 1) {
                        graph.getVertex(clafer).addNeighbour(graph.getVertex(child));
                    }
                }
            }
            if (clafer.hasRef()) {
                graph.getVertex(clafer).addNeighbour(graph.getVertex(clafer.getRef().getTargetType()));
            }
        }

        Set<AstRef> breakableRefs = new HashSet<AstRef>();
        for (AstClafer clafer : analysis.getClafers()) {
            if (clafer.hasRef()) {
                if (!GraphUtil.hasPath(
                        graph.getVertex(clafer.getRef().getTargetType()),
                        graph.getVertex(clafer),
                        graph)) {
                    breakableRefs.add(clafer.getRef());
                }
            }
        }

        return breakableRefs;
    }
}
