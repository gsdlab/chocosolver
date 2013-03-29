package org.clafer.analysis;

import java.util.HashMap;
import java.util.Map;
import org.clafer.Scope;
import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstModel;
import org.clafer.ast.Card;

/**
 *
 * @author jimmy
 */
public class GlobalCardAnalysis {

    public static Map<AstClafer, Card> analyze(AstModel model, Scope scope) {
        Map<AstClafer, Card> globalCards = new HashMap<AstClafer, Card>();
        for (AstConcreteClafer topClafer : model.getTopClafers()) {
            analyze(topClafer, new Card(1, 1), scope, globalCards);
        }
        // Abstract clafers that are the super clafer of other abstract clafers are
        // analyzed last. Higher depth clafers go first.
        for (AstAbstractClafer abstractClafer : model.getAbstractClafers()) {
            analyze(abstractClafer, scope, globalCards);
        }
        return globalCards;
    }

    private static void analyze(AstAbstractClafer clafer, Scope scope, Map<AstClafer, Card> globalCards) {
        int lowGlobalCard = 0;
        int highGlobalCard = 0;
        for (AstClafer sub : clafer.getSubs()) {
            Card subGlobalCard = globalCards.get(sub);
            if (subGlobalCard == null) {
                // This is possible if a child of an abstract extends the abstract.
                // Assume the worst possible case.
                subGlobalCard = new Card(0, scope.getScope(sub));
            }
            lowGlobalCard += subGlobalCard.getLow();
            highGlobalCard += subGlobalCard.getHigh();
        }
        System.out.println(clafer);
        Card globalCard = new Card(lowGlobalCard, highGlobalCard);
        globalCards.put(clafer, globalCard);
        for (AstConcreteClafer child : clafer.getChildren()) {
            analyze(child, globalCard, scope, globalCards);
        }
    }

    private static void analyze(AstConcreteClafer clafer, Card parentGlobalCard, Scope scope, Map<AstClafer, Card> globalCards) {
        // Cap by scope
        Card globalCard = parentGlobalCard.mult(clafer.getCard());
        globalCard = new Card(
                Math.min(globalCard.getLow(), scope.getScope(clafer)),
                Math.min(globalCard.getHigh(), scope.getScope(clafer)));
        globalCards.put(clafer, globalCard);
        for (AstConcreteClafer child : clafer.getChildren()) {
            analyze(child, globalCard, scope, globalCards);
        }
    }
}
