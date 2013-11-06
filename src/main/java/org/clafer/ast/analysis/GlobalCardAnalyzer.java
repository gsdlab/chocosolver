package org.clafer.ast.analysis;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstException;
import org.clafer.ast.Card;

/**
 *
 * @author jimmy
 */
public class GlobalCardAnalyzer implements Analyzer {

    @Override
    public Analysis analyze(Analysis analysis) {
        Map<AstClafer, Card> globalCardMap = new HashMap<>();
        globalCardMap.put(analysis.getModel(), new Card(1, 1));

        for (Set<AstClafer> component : analysis.getClafersInParentAndSubOrder()) {
            for (AstClafer clafer : component) {
                if (clafer instanceof AstConcreteClafer) {
                    analyze((AstConcreteClafer) clafer, analysis, globalCardMap);
                } else {
                    analyze((AstAbstractClafer) clafer, analysis, globalCardMap);
                }
            }
        }
        return analysis.setGlobalCardMap(globalCardMap);
    }

    private static void analyze(AstAbstractClafer clafer, Analysis analysis, Map<AstClafer, Card> globalCardMap) {
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

    private static void analyze(AstConcreteClafer clafer, Analysis analysis, Map<AstClafer, Card> globalCardMap) {
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
            throw new AstException("Scope of " + clafer.getName() + " is " + scope + " which is too low, needs to be at least " + globalCard.getLow());
        }
        globalCard = new Card(
                globalCard.getLow(),
                Math.min(globalCard.getHigh(), analysis.getScope(clafer)));
        globalCardMap.put(clafer, globalCard);
    }
}
