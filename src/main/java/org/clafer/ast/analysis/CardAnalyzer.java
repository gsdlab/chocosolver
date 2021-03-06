package org.clafer.ast.analysis;

import java.util.HashMap;
import java.util.Map;
import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.Card;

/**
 * Rewrites the model replacing unbounded high cardinalities with bounded
 * effective high cardinalities.
 *
 * @author jimmy
 */
public class CardAnalyzer implements Analyzer {

    @Override
    public Analysis analyze(Analysis analysis) {
        Map<AstConcreteClafer, Card> cardMap = new HashMap<>();
        analyze(analysis.getModel().getAbstractRoot(), cardMap, analysis);
        analyze(analysis.getModel().getRoot(), 1, cardMap, analysis);
        return analysis.setCardMap(cardMap);
    }

    private static void analyze(AstAbstractClafer clafer, Map<AstConcreteClafer, Card> cardMap, Analysis analysis) {
        Card globalCard = analysis.getGlobalCard(clafer);
        for (AstAbstractClafer child : clafer.getAbstractChildren()) {
            analyze(child, cardMap, analysis);
        }
        for (AstConcreteClafer child : clafer.getChildren()) {
            analyze(child, globalCard.getLow(), cardMap, analysis);
        }
    }

    private static void analyze(AstConcreteClafer clafer, int parentlowGlobalCard, Map<AstConcreteClafer, Card> cardMap, Analysis analysis) {
        int low = analysis.getCard(clafer).getLow();
        int high = analysis.getCard(clafer).getHigh();
        Card globalCard = analysis.getGlobalCard(clafer);

        int evenlyDistributed = parentlowGlobalCard * low;
        int rest = globalCard.getHigh() - evenlyDistributed;
        int scope = analysis.getScope(clafer);
        if (low > scope) {
            cardMap.put(clafer, new Card(0, 0));
        } else {
            cardMap.put(clafer, new Card(low, Math.min(scope, Math.min(high, low + rest))));
        }

        for (AstConcreteClafer child : clafer.getChildren()) {
            analyze(child, globalCard.getLow(), cardMap, analysis);
        }
    }
}
