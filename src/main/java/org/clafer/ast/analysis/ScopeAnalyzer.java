package org.clafer.ast.analysis;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
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
            for (AstClafer clafer : component) {
                if (clafer instanceof AstConcreteClafer) {
                    AstConcreteClafer concreteClafer = (AstConcreteClafer) clafer;
                    Card card = analysis.getCard(concreteClafer);
                    if (card.isExact() && concreteClafer.hasParent()) {
                        int parentScope = optimizedScope.get(concreteClafer.getParent());
                        optimizedScope.put(clafer, parentScope * card.getLow());
                    } else {
                        Card globalCard = analysis.getGlobalCard(clafer);
                        optimizedScope.put(clafer, Math.min(scope.getScope(clafer), globalCard.getHigh()));
                    }
                }
            }
            for (AstClafer clafer : component) {
                if (clafer instanceof AstAbstractClafer) {
                    int subScopes = 0;
                    for (AstClafer sub : ((AstAbstractClafer) clafer).getSubs()) {
                        subScopes += optimizedScope.containsKey(sub) ? optimizedScope.get(sub) : scope.getDefaultScope();
                    }
                    optimizedScope.put(clafer, subScopes);
                }
            }
        }
        return analysis.setScope(new Scope(
                optimizedScope,
                scope.getDefaultScope(),
                scope.getIntLow(),
                scope.getIntHigh(),
                scope.getMulLow(),
                scope.getMulHigh(),
                scope.getStringLength(),
                scope.getCharLow(),
                scope.getCharHigh()));
    }
}
