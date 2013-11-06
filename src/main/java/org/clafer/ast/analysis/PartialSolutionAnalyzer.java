package org.clafer.ast.analysis;

import gnu.trove.list.array.TIntArrayList;
import java.util.Arrays;
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
public class PartialSolutionAnalyzer implements Analyzer {

    @Override
    public Analysis analyze(Analysis analysis) {
        Map<AstClafer, PartialSolution> partialSolutionMap = new HashMap<>();

        partialSolutionMap.put(analysis.getModel(), new PartialSolution(new boolean[]{true}, new int[0][]));
        for (AstConcreteClafer child : analysis.getModel().getChildren()) {
            analyze(child, analysis, partialSolutionMap);
        }
        for (AstAbstractClafer abstractClafer : analysis.getAbstractClafers()) {
            analyze(abstractClafer, analysis, partialSolutionMap);
        }

        return analysis.setPartialSolutionMap(partialSolutionMap);
    }

    private static void analyze(
            AstAbstractClafer clafer,
            Analysis analysis,
            Map<AstClafer, PartialSolution> partialSolutionMap) {
        Card globalCard = analysis.getGlobalCard(clafer);
        boolean[] solution = new boolean[analysis.getScope(clafer)];
        int[][] parents = new int[globalCard.getHigh()][];
        for (AstClafer sub : clafer.getSubs()) {
            int offset = analysis.getOffsets(clafer).getOffset(sub);

            PartialSolution partialSubSolution = partialSolutionMap.get(sub);
            // This is possible for partialSubSolution to be null if a child of an abstract
            // extends the abstract. Assume the worst possible case by assuming it is empty.
            if (partialSubSolution != null) {
                System.arraycopy(partialSubSolution.getSolution(), 0, solution, offset, partialSubSolution.size());
            }
        }
        partialSolutionMap.put(clafer, new PartialSolution(solution, parents));

        for (AstConcreteClafer child : clafer.getChildren()) {
            analyze(child, analysis, partialSolutionMap);
        }
    }

    private static void analyze(
            AstConcreteClafer clafer,
            Analysis analysis,
            Map<AstClafer, PartialSolution> partialSolutionMap) {
        Card globalCard = analysis.getGlobalCard(clafer);
        Format format = analysis.getFormat(clafer);

        boolean[] solution = new boolean[analysis.getScope(clafer)];
        TIntArrayList[] parents = new TIntArrayList[globalCard.getHigh()];
        for (int i = 0; i < parents.length; i++) {
            parents[i] = new TIntArrayList();
        }

        PartialSolution partialParentSolution = partialSolutionMap.get(clafer.getParent());
        Card card = analysis.getCard(clafer);
        int lowCard = card.getLow();
        int highCard = card.getHigh();
        switch (format) {
            case LowGroup:
                Arrays.fill(solution, 0, globalCard.getLow(), true);
                int low = 0;
                int high = highCard;
                for (int i = 0; i < partialParentSolution.size(); i++) {
                    for (int j = low; j < high && j < parents.length; j++) {
                        parents[j].add(i);
                    }
                    if (partialParentSolution.hasClafer(i)) {
                        low += lowCard;
                    }
                    high += highCard;
                }
                break;
            case ParentGroup:
                assert lowCard == highCard;
                for (int i = 0; i < partialParentSolution.size(); i++) {
                    for (int j = 0; j < lowCard; j++) {
                        solution[i * lowCard + j] = partialParentSolution.hasClafer(i);
                        parents[i * lowCard + j].add(i);
                    }
                }
                break;
            default:
                throw new AnalysisException();
        }
        partialSolutionMap.put(clafer, new PartialSolution(solution, toArray(parents)));

        for (AstConcreteClafer child : clafer.getChildren()) {
            analyze(child, analysis, partialSolutionMap);
        }
    }

    private static int[][] toArray(TIntArrayList[] list) {
        int[][] array = new int[list.length][];
        for (int i = 0; i < array.length; i++) {
            array[i] = list[i].toArray();
        }
        return array;
    }
}
