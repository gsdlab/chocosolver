package org.clafer.analysis;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import org.clafer.analysis.AbstractOffsetAnalysis.Offsets;
import org.clafer.analysis.FormatAnalysis.Format;
import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstModel;
import org.clafer.tree.Card;

/**
 *
 * @author jimmy
 */
public class PartialSolutionAnalysis {

    public static Map<AstClafer, boolean[]> analyze(
            AstModel model,
            Map<AstClafer, Card> globalCards,
            Map<AstClafer, Format> formats,
            Map<AstAbstractClafer, Integer> depths,
            Map<AstAbstractClafer, Offsets> offsets) {
        Map<AstClafer, boolean[]> partialSolutions = new HashMap<AstClafer, boolean[]>();

        for (AstConcreteClafer topClafer : model.getTopClafers()) {
            analyze(topClafer, globalCards, formats, partialSolutions);
        }
        for (AstAbstractClafer abstractClafer : AnalysisUtil.descendingDepths(model.getAbstractClafers(), depths)) {
            analyze(abstractClafer, globalCards, formats, offsets, partialSolutions);
        }

        return partialSolutions;
    }

    private static void analyze(
            AstAbstractClafer clafer,
            Map<AstClafer, Card> globalCards,
            Map<AstClafer, Format> formats,
            Map<AstAbstractClafer, Offsets> offsets,
            Map<AstClafer, boolean[]> partialSolutions) {
        boolean[] partialSolution = new boolean[AnalysisUtil.notNull(clafer + " global card not analyzed yet", globalCards.get(clafer)).getHigh()];
        for (AstClafer sub : clafer.getSubs()) {
            int offset = AnalysisUtil.notNull(clafer + " offset not analyzed yet", offsets.get(clafer)).getOffset(sub);

            boolean[] partialSubSolution = partialSolutions.get(sub);
            if (partialSubSolution == null) {
                // This is possible for partialSubSolution to be null if a child of an abstract
                // extends the abstract. Assume the worst possible case by assuming it is empty.
                partialSubSolution = new boolean[AnalysisUtil.notNull(clafer + " global card not analyzed yet", globalCards.get(sub)).getHigh()];
            }
            System.arraycopy(partialSubSolution, 0, partialSolution, offset, partialSubSolution.length);
            offset += partialSubSolution.length;
        }
        partialSolutions.put(clafer, partialSolution);

        for (AstConcreteClafer child : clafer.getChildren()) {
            analyze(child, globalCards, formats, partialSolutions);
        }
    }

    private static void analyze(
            AstConcreteClafer clafer,
            Map<AstClafer, Card> globalCards,
            Map<AstClafer, Format> formats,
            Map<AstClafer, boolean[]> partialSolutions) {
        Card globalCard = AnalysisUtil.notNull(clafer + " global card not analyzed yet", globalCards.get(clafer));
        Format format = AnalysisUtil.notNull(clafer.getName() + " format not analyzed yet", formats.get(clafer));

        boolean[] partialSolution = new boolean[globalCard.getHigh()];
        switch (format) {
            case LowGroup:
                Arrays.fill(partialSolution, 0, globalCard.getLow(), true);
                break;
            case ParentGroup:
                // assert has a fixed effective cardinality
                // assert clafer.card.low is equal to effective lower cardinality
                if (!clafer.hasParent()) {
                    Arrays.fill(partialSolution, 0, globalCard.getLow(), true);
                } else {
                    boolean[] partialParentSolution = partialSolutions.get(clafer.getParent());
                    for(int i = 0; i < partialParentSolution.length; i++) {
                        for (int j = 0; j < clafer.getCard().getLow(); j++) {
                            partialSolution[i * clafer.getCard().getLow() + j] = partialParentSolution[i];
                        }
                    }
                }
                break;
        }
        partialSolutions.put(clafer, partialSolution);

        for (AstConcreteClafer child : clafer.getChildren()) {
            analyze(child, globalCards, formats, partialSolutions);
        }
    }
}
