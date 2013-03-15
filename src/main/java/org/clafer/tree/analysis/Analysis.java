package org.clafer.tree.analysis;

import java.util.Map;
import org.clafer.tree.AbstractClafer;
import org.clafer.tree.AtomicClafer;
import org.clafer.tree.Card;
import org.clafer.tree.ClaferModel;

/**
 *
 * @author jimmy
 */
public class Analysis {

    private final Map<AbstractClafer, Integer> depths;
    private final Map<AtomicClafer, Card> globalCards;

    private Analysis(Map<AbstractClafer, Integer> depths, Map<AtomicClafer, Card> globalCards) {
        this.depths = depths;
        this.globalCards = globalCards;
    }

    public static Analysis analyze(ClaferModel model) {
        Map<AbstractClafer, Integer> depths = TypeHierarchyDepthAnalysis.analyze(model);
        Map<AtomicClafer, Card> globalCards = GlobalCardAnalysis.analyze(model, depths);

        return new Analysis(depths, globalCards);
    }

    public int getDepth(AbstractClafer clafer) {
        Integer depth = depths.get(clafer);
        if (depth == null) {
            throw new IllegalArgumentException("Cannot find depth analysis for " + clafer.getName());
        }
        return depth;
    }

    public Card getGlobalCard(AtomicClafer clafer) {
        Card globalCard = globalCards.get(clafer);
        if (globalCard == null) {
            throw new IllegalArgumentException("Cannot find global card analysis for " + clafer.getName());
        }
        return globalCard;
    }
}
