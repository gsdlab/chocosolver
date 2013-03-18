package org.clafer.tree.analysis;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.clafer.tree.AbstractClafer;
import org.clafer.tree.AtomicClafer;
import org.clafer.tree.Card;
import org.clafer.tree.ClaferModel;
import org.clafer.tree.ConcreteClafer;
import org.clafer.tree.RootClafer;

/**
 *
 * @author jimmy
 */
public class GlobalCardAnalysis {

    public static Map<AtomicClafer, Card> analyze(ClaferModel model, final Map<AbstractClafer, Integer> depths) {
        Map<AtomicClafer, Card> globalCards = new HashMap<AtomicClafer, Card>();
        analyze(model.getRoot(), globalCards);
        // Abstract clafers that are the super clafer of other abstract clafers are
        // analyzed last. Higher depth clafers go first.
        List<AbstractClafer> abstractClafers = new ArrayList<AbstractClafer>(model.getAbstractClafers());
        Collections.sort(abstractClafers,
                new Comparator<AbstractClafer>() {

                    @Override
                    public int compare(AbstractClafer o1, AbstractClafer o2) {
                        Integer depth1 = depths.get(o1);
                        if (depth1 == null) {
                            throw new IllegalArgumentException(o1 + " depth not analyzed yet");
                        }
                        Integer depth2 = depths.get(o2);
                        if (depth2 == null) {
                            throw new IllegalArgumentException(o2 + " depth not analyzed yet");
                        }
                        int $depth1 = depth1.intValue();
                        int $depth2 = depth2.intValue();
                        return $depth1 > $depth2 ? -1 : ($depth1 == $depth2 ? 0 : 1);
                    }
                });
        for (AbstractClafer abstractClafer : abstractClafers) {
            analyze(abstractClafer, globalCards);
        }
        return globalCards;
    }

    private static void analyze(RootClafer clafer, Map<AtomicClafer, Card> globalCards) {
        analyze(clafer, new Card(1, 1), globalCards);
    }

    private static void analyze(AbstractClafer clafer, Map<AtomicClafer, Card> globalCards) {
        Card globalCard = new Card(0, 0);
        for (AtomicClafer sub : clafer.getSubs()) {
            Card subGlobalCard = globalCards.get(sub);
            if (subGlobalCard == null) {
                // TODO: this is possible if a child of an abstract extends the abstract
                throw new IllegalArgumentException(sub.getName() + " does not have it's global card analyzed yet");
            }
            globalCard = globalCard.add(subGlobalCard);
        }
        analyze(clafer, globalCard, globalCards);
    }

    private static void analyze(AtomicClafer clafer, Card globalCard, Map<AtomicClafer, Card> globalCards) {
        // Cap by scope
        globalCard = new Card(
                Math.min(globalCard.getLow(), clafer.getScope()),
                Math.min(globalCard.getHigh(), clafer.getScope()));
        globalCards.put(clafer, globalCard);
        for (ConcreteClafer child : clafer.getChildren()) {
            globalCard = globalCard.mult(child.getCard());
            analyze(child, globalCard, globalCards);
        }
    }
}
