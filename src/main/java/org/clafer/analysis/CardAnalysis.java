package org.clafer.analysis;

import java.util.Map;
import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstModel;
import org.clafer.tree.Card;

/**
 * Rewrites the model replacing unbounded high cardinalities with bounded effecitive
 * high cardinalities.
 * 
 * @author jimmy
 */
public class CardAnalysis {

    public static void analyze(AstModel model, Map<AstClafer, Card> globalCards) {
        for (AstAbstractClafer abstractClafer : model.getAbstractClafers()) {
            analyze(abstractClafer, globalCards);
        }
        for (AstConcreteClafer topClafer : model.getTopClafers()) {
            analyze(topClafer, 1, globalCards);
        }
    }

    private static void analyze(AstAbstractClafer clafer, Map<AstClafer, Card> globalCards) {
        Card globalCard = AnalysisUtil.notNull(clafer + " global card not analyzed yet", globalCards.get(clafer));
        for (AstConcreteClafer child : clafer.getChildren()) {
            analyze(child, globalCard.getLow(), globalCards);
        }
    }

    private static void analyze(AstConcreteClafer clafer, int parentlowGlobalCard, Map<AstClafer, Card> globalCards) {
        int low = clafer.getCard().getLow();
        int high = clafer.getCard().getHigh();
        Card globalCard = AnalysisUtil.notNull(clafer + " global card not analyzed yet", globalCards.get(clafer));

        int evenlyDistributed = parentlowGlobalCard * low;
        int rest = globalCard.getHigh() - evenlyDistributed;
        // If rest is less than 0, the model is unsatisfiable, most likely due to
        // insufficient scope. Throwing an exception is a possibility at this point
        // but let's continue on instead.
        clafer.withCard(low, Math.min(high, low + Math.max(rest, 0)));

        for (AstConcreteClafer child : clafer.getChildren()) {
            analyze(child, globalCard.getLow(), globalCards);
        }
    }
}
