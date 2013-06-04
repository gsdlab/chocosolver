package org.clafer.ast.analysis;

import java.util.HashMap;
import java.util.Map;
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
        Map<AstClafer, Card> globalCardMap = new HashMap<AstClafer, Card>();
        globalCardMap.put(analysis.getModel(), new Card(1, 1));
        for (AstConcreteClafer child : analysis.getModel().getChildren()) {
            analyze(child, new Card(1, 1), analysis, globalCardMap);
        }
        // Abstract clafers that are the super clafer of other abstract clafers are
        // analyzed last. Higher depth clafers go first.
        for (AstAbstractClafer abstractClafer : analysis.getAbstractClafers()) {
            analyze(abstractClafer, analysis, globalCardMap);
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
        for (AstConcreteClafer child : clafer.getChildren()) {
            analyze(child, globalCard, analysis, globalCardMap);
        }
    }

    private static void analyze(AstConcreteClafer clafer, Card parentGlobalCard, Analysis analysis, Map<AstClafer, Card> globalCardMap) {
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
        for (AstConcreteClafer child : clafer.getChildren()) {
            analyze(child, globalCard, analysis, globalCardMap);
        }
    }
}
