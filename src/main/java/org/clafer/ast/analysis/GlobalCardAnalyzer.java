package org.clafer.ast.analysis;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstRef;
import org.clafer.ast.Card;
import org.clafer.collection.Pair;

/**
 *
 * @author jimmy
 */
public class GlobalCardAnalyzer implements Analyzer {

    @Override
    public Analysis analyze(Analysis analysis) {
        Map<AstClafer, Card> globalCardMap = new HashMap<>();
        globalCardMap.put(analysis.getModel(), new Card(1, 1));
        List<Pair<AstClafer, Integer>> insufficientScopes = new ArrayList<>();

        for (Set<AstClafer> component : analysis.getClafersInParentAndSubOrder()) {
            for (AstClafer clafer : component) {
                if (clafer instanceof AstConcreteClafer) {
                    analyze((AstConcreteClafer) clafer, analysis, globalCardMap, insufficientScopes);
                } else {
                    analyze((AstAbstractClafer) clafer, analysis, globalCardMap, insufficientScopes);
                }
            }
        }
        for (AstClafer clafer : analysis.getClafers()) {
            if (clafer.hasRef()) {
                AstRef ref = clafer.getRef();
                if (!ref.getTargetType().isPrimitive()) {
                    int lowCard = maxLowCard(ref.getSourceType(), globalCardMap);
                    int required = ref.isUnique() ? lowCard : Math.min(1, lowCard);
                    Card targetCard = globalCardMap.get(ref.getTargetType());
                    assert targetCard.hasHigh();
                    if (targetCard.getHigh() < required) {
                        insufficientScopes.add(new Pair<>(ref.getTargetType(), required));
                    }
                }
            }
        }

        if (!insufficientScopes.isEmpty()) {
            throw new InsufficientScopeException(insufficientScopes);
        }
        return analysis.setGlobalCardMap(globalCardMap);
    }

    private static int maxLowCard(AstClafer clafer, Map<AstClafer, Card> globalCardMap) {
        if (!globalCardMap.get(clafer).hasLow()) {
            return 0;
        }
        if (clafer instanceof AstConcreteClafer) {
            return ((AstConcreteClafer) clafer).getCard().getLow();
        }
        AstAbstractClafer abstractClafer = (AstAbstractClafer) clafer;
        int maxLowCard = 0;
        for (AstClafer sub : abstractClafer.getSubs()) {
            maxLowCard = Math.max(maxLowCard, maxLowCard(sub, globalCardMap));
        }
        return maxLowCard;
    }

    private static void analyze(AstAbstractClafer clafer, Analysis analysis,
            Map<AstClafer, Card> globalCardMap, List<Pair<AstClafer, Integer>> insufficientScopes) {
        Card globalCard = new Card(0, 0);
        for (AstClafer sub : clafer.getSubs()) {
            Card subGlobalCard = globalCardMap.get(sub);
            if (subGlobalCard == null) {
                // This is possible if a child of an abstract extends the abstract.
                // Assume the worst possible case.
                subGlobalCard = new Card(0, analysis.getScope(sub));
            }
            globalCard = globalCard.add(subGlobalCard);
        }
        globalCardMap.put(clafer, globalCard);
    }

    private static void analyze(AstConcreteClafer clafer, Analysis analysis,
            Map<AstClafer, Card> globalCardMap, List<Pair<AstClafer, Integer>> insufficientScopes) {
        Card parentGlobalCard;
        if (!clafer.hasParent()) {
            parentGlobalCard = new Card(1, 1);
        } else {
            parentGlobalCard = globalCardMap.get(clafer.getParent());
            if (parentGlobalCard == null) {
                // Not analyzed yet due to cycle.
                parentGlobalCard = new Card(0, analysis.getScope(clafer.getParent()));
            }
        }
        // Cap by scope
        Card globalCard = parentGlobalCard.mult(analysis.getCard(clafer));
        int scope = analysis.getScope(clafer);
        if (scope < globalCard.getLow()) {
            insufficientScopes.add(new Pair<AstClafer, Integer>(clafer, globalCard.getLow()));
        } else {
            globalCard = new Card(
                    globalCard.getLow(),
                    Math.min(globalCard.getHigh(), analysis.getScope(clafer)));
            globalCardMap.put(clafer, globalCard);
        }
    }
}
