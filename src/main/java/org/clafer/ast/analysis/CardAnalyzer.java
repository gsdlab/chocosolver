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
        Map<AstConcreteClafer, Card> cardMap = new HashMap<AstConcreteClafer, Card>();
        for (AstAbstractClafer abstractClafer : analysis.getAbstractClafers()) {
            analyze(abstractClafer, cardMap, analysis);
        }
        for (AstConcreteClafer child : analysis.getModel().getChildren()) {
            analyze(child, 1, cardMap, analysis);
        }
        return analysis.setCardMap(cardMap);
    }

    private static void analyze(AstAbstractClafer clafer, Map<AstConcreteClafer, Card> cardMap, Analysis analysis) {
        Card globalCard = analysis.getGlobalCard(clafer);
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
        // If rest is less than 0, the model is unsatisfiable, most likely due to
        // insufficient scope. Throwing an exception is a possibility at this point
        // but let's continue on instead.
        cardMap.put(clafer, new Card(low, Math.min(high, low + Math.max(rest, 0))));

        for (AstConcreteClafer child : clafer.getChildren()) {
            analyze(child, globalCard.getLow(), cardMap, analysis);
        }
    }
}
