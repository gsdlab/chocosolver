package org.clafer.ast.analysis;

import java.util.HashMap;
import java.util.Map;
import org.clafer.scope.Scope;
import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.Card;

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

        for (AstConcreteClafer clafer : analysis.getConcreteClafers()) {
            Card globalCard = analysis.getGlobalCard(clafer);
            optimizedScope.put(clafer, Math.min(scope.getScope(clafer), globalCard.getHigh()));
        }

        for (AstAbstractClafer abstractClafer : analysis.getAbstractClafers()) {
            int subScopes = 0;
            for (AstClafer sub : abstractClafer.getSubs()) {
                subScopes += optimizedScope.containsKey(sub) ? optimizedScope.get(sub) : scope.getDefaultScope();
            }
            optimizedScope.put(abstractClafer, subScopes);
        }

        return analysis.setScope(new Scope(optimizedScope, scope.getDefaultScope(), scope.getIntLow(), scope.getIntHigh()));
    }
}
