package org.clafer.ast.analysis;

import java.util.HashMap;
import java.util.Map;
import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.Card;

/**
 *
 * @author jimmy
 */
public class GlobalCardAnalyzer implements Analyzer {

    @Override
    public Analysis analyze(Analysis analysis) {
        Map<AstClafer, Card> globalCards = new HashMap<AstClafer, Card>();
        globalCards.put(analysis.getModel(), new Card(1, 1));
        for (AstConcreteClafer child : analysis.getModel().getChildren()) {
            analyze(child, new Card(1, 1), analysis, globalCards);
        }
        // Abstract clafers that are the super clafer of other abstract clafers are
        // analyzed last. Higher depth clafers go first.
        for (AstAbstractClafer abstractClafer : analysis.getAbstractClafers()) {
            analyze(abstractClafer, analysis, globalCards);
        }
        return analysis.withGlobalCardMap(globalCards);
    }

    private static void analyze(AstAbstractClafer clafer, Analysis analysis, Map<AstClafer, Card> globalCards) {
        Card globalCard = new Card(0, 0);
        for (AstClafer sub : clafer.getSubs()) {
            Card subGlobalCard = globalCards.get(sub);
            if (subGlobalCard == null) {
                // This is possible if a child of an abstract extends the abstract.
                // Assume the worst possible case.
                subGlobalCard = new Card(0, analysis.getScope(sub));
            }
            globalCard = globalCard.add(subGlobalCard);
        }
        globalCards.put(clafer, globalCard);
        for (AstConcreteClafer child : clafer.getChildren()) {
            analyze(child, globalCard, analysis, globalCards);
        }
    }

    private static void analyze(AstConcreteClafer clafer, Card parentGlobalCard, Analysis analysis, Map<AstClafer, Card> globalCards) {
        // Cap by scope
        Card globalCard = parentGlobalCard.mult(analysis.getCard(clafer));
        globalCard = new Card(
                Math.min(globalCard.getLow(), analysis.getScope(clafer)),
                Math.min(globalCard.getHigh(), analysis.getScope(clafer)));
        globalCards.put(clafer, globalCard);
        for (AstConcreteClafer child : clafer.getChildren()) {
            analyze(child, globalCard, analysis, globalCards);
        }
    }
}
