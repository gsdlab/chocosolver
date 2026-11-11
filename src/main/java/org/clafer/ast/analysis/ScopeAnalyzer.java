package org.clafer.ast.analysis;

import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;
import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.Card;
import org.clafer.scope.Scope;

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
        Map<AstClafer, Integer> optimizedScope = new HashMap<>();
        optimizedScope.put(analysis.getModel().getRoot(), 1);

        for (Set<AstClafer> component : analysis.getClafersInParentAndSubOrder()) {
            // In a recursive structure, e.g. abstract Element with clip : Clip ?, abstract
            // Group : Element and abstract Clip : Group, the clafers of a component depend on
            // each other: the scope of an abstract clafer is the sum of the scopes of its sub
            // clafers, and the scope of a concrete clafer with an exact cardinality is a
            // multiple of its parent's. Each clafer is analyzed once the clafers it depends on
            // are; only a concrete clafer on a cycle is analyzed without its parent's scope.
            // An abstract clafer analyzed before one of its sub clafers would get a smaller
            // scope than its sub clafers together.
            Set<AstClafer> remaining = new LinkedHashSet<>(component);
            while (!remaining.isEmpty()) {
                AstClafer next = null;
                for (AstClafer clafer : remaining) {
                    if (dependenciesAnalyzed(analysis, clafer, optimizedScope, component)) {
                        next = clafer;
                        break;
                    }
                }
                if (next == null) {
                    // A cycle: a concrete clafer whose abstract parent is not analyzed yet.
                    for (AstClafer clafer : remaining) {
                        if (clafer instanceof AstConcreteClafer) {
                            next = clafer;
                            break;
                        }
                    }
                }
                if (next instanceof AstConcreteClafer) {
                    AstConcreteClafer concreteClafer = (AstConcreteClafer) next;
                    Card card = analysis.getCard(concreteClafer);
                    Integer parentScope = concreteClafer.hasParent()
                            ? optimizedScope.get(concreteClafer.getParent()) : null;
                    if (card.isExact() && parentScope != null) {
                        optimizedScope.put(next, parentScope * card.getLow());
                    } else {
                        Card globalCard = analysis.getGlobalCard(next);
                        optimizedScope.put(next, Math.min(scope.getScope(next), globalCard.getHigh()));
                    }
                } else {
                    int subScopes = 0;
                    for (AstClafer sub : ((AstAbstractClafer) next).getSubs()) {
                        subScopes += optimizedScope.containsKey(sub) ? optimizedScope.get(sub) : scope.getDefaultScope();
                    }
                    optimizedScope.put(next, subScopes);
                }
                remaining.remove(next);
            }
        }
        return analysis.setScope(scope.withScopes(optimizedScope));
    }

    private static boolean dependenciesAnalyzed(Analysis analysis, AstClafer clafer,
            Map<AstClafer, Integer> optimizedScope, Set<AstClafer> component) {
        if (clafer instanceof AstConcreteClafer) {
            AstConcreteClafer concreteClafer = (AstConcreteClafer) clafer;
            return !concreteClafer.hasParent()
                    || !analysis.getCard(concreteClafer).isExact()
                    || !component.contains(concreteClafer.getParent())
                    || optimizedScope.containsKey(concreteClafer.getParent());
        }
        for (AstClafer sub : ((AstAbstractClafer) clafer).getSubs()) {
            if (component.contains(sub) && !optimizedScope.containsKey(sub)) {
                return false;
            }
        }
        return true;
    }
}
