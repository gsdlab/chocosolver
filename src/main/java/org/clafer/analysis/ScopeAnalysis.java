package org.clafer.analysis;

import java.util.HashMap;
import java.util.Map;
import org.clafer.Scope;
import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstModel;
import org.clafer.ast.Card;

/**
 * 
 * @author jimmy
 */
public class ScopeAnalysis {

    /*
     * Shrinks the scope if it's greater than the upper global cardinality.
     * Also set abstract scopes.
     */
    public static Scope analyze(AstModel model, Scope scope, Map<AstClafer, Card> globalCards) {
        Map<AstClafer, Integer> optimizedScope = new HashMap<AstClafer, Integer>();

        for (AstClafer clafer : globalCards.keySet()) {
            Card globalCard = globalCards.get(clafer);
            if (globalCard == null) {
                throw new AnalysisException(clafer + " global card analyzed yet");
            }
            optimizedScope.put(clafer, Math.min(scope.getScope(clafer), globalCard.getHigh()));
        }
        scope = new Scope(optimizedScope);

        for (AstAbstractClafer abstractClafer : model.getAbstractClafers()) {
            int subScopes = 0;
            for(AstClafer sub : abstractClafer.getSubs()) {
                subScopes += scope.getScope(sub);
            }
            optimizedScope.put(abstractClafer, subScopes);
        }

        return new Scope(optimizedScope, scope.getDefaultScope());
    }
}
