package org.clafer.ast.analysis;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.clafer.scope.Scope;
import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.Card;
import org.clafer.graph.GraphUtil;
import org.clafer.graph.KeyGraph;
import org.clafer.graph.Vertex;

/**
 *
 * @author jimmy
 */
public class ScopeAnalyzer implements Analyzer {

    /*
     * Shrinks the scope if it's greater than the upper global cardinality.
     * Also set abstract scopes.
     */
    @Override
    public Analysis analyze(Analysis analysis) {
        Scope scope = analysis.getScope();
        Map<AstClafer, Integer> optimizedScope = new HashMap<AstClafer, Integer>();
        optimizedScope.put(analysis.getModel(), 1);

        KeyGraph<AstClafer> dependency = new KeyGraph<AstClafer>();
        for (AstAbstractClafer abstractClafer : analysis.getAbstractClafers()) {
            Vertex<AstClafer> node = dependency.getVertex(abstractClafer);
            for (AstClafer sub : abstractClafer.getSubs()) {
                node.addNeighbour(dependency.getVertex(sub));
            }
        }
        for (AstConcreteClafer concreteClafer : analysis.getConcreteClafers()) {
            dependency.getVertex(concreteClafer);
        }
        List<Set<AstClafer>> components = GraphUtil.computeStronglyConnectedComponents(dependency);

        for (Set<AstClafer> component : components) {
            for (AstClafer clafer : component) {
                if (clafer instanceof AstConcreteClafer) {
                    Card globalCard = analysis.getGlobalCard(clafer);
                    optimizedScope.put(clafer, Math.min(scope.getScope(clafer), globalCard.getHigh()));
                } else {
                    int subScopes = 0;
                    for (AstClafer sub : ((AstAbstractClafer) clafer).getSubs()) {
                        if (!optimizedScope.containsKey(sub)) {
                            System.out.println(sub);
                        }
                        subScopes += optimizedScope.containsKey(sub) ? optimizedScope.get(sub) : scope.getDefaultScope();
                    }
                    optimizedScope.put(clafer, subScopes);
                }
            }
        }
        return analysis.setScope(new Scope(optimizedScope, scope.getDefaultScope(), scope.getIntLow(), scope.getIntHigh()));
    }
}
