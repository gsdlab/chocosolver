package org.clafer.ast.analysis;

import java.util.HashSet;
import java.util.Set;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstRef;
import org.clafer.graph.GraphUtil;
import org.clafer.graph.KeyGraph;

/**
 *
 * @author jimmy
 */
public class SymmetryAnalyzer implements Analyzer {

    @Override
    public Analysis analyze(Analysis analysis) {
        KeyGraph<AstClafer> graph = new KeyGraph<AstClafer>();
        for (AstClafer clafer : analysis.getClafers()) {
            if (clafer instanceof AstConcreteClafer) {
                AstConcreteClafer concreteClafer = (AstConcreteClafer) clafer;
                if (concreteClafer.hasParent() && analysis.getScope(concreteClafer.getParent()) > 1) {
                    graph.getVertex(concreteClafer).addNeighbour(graph.getVertex(concreteClafer.getParent()));
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

        return analysis.withBreakableRefs(breakableRefs);
    }
}
