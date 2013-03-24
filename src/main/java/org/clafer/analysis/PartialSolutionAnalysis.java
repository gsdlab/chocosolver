package org.clafer.analysis;

import gnu.trove.TIntArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import org.clafer.Util;
import org.clafer.analysis.AbstractOffsetAnalysis.Offsets;
import org.clafer.analysis.FormatAnalysis.Format;
import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstModel;
import org.clafer.ast.Card;

/**
 *
 * @author jimmy
 */
public class PartialSolutionAnalysis {

    public static Map<AstClafer, PartialSolution> analyze(
            AstModel model,
            Map<AstClafer, Card> globalCards,
            Map<AstClafer, Format> formats,
            Map<AstAbstractClafer, Integer> depths,
            Map<AstAbstractClafer, Offsets> offsets) {
        Map<AstClafer, PartialSolution> partialSolutions = new HashMap<AstClafer, PartialSolution>();

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
            Map<AstClafer, PartialSolution> partialSolutions) {
        Card globalCard = AnalysisUtil.notNull(clafer + " global card not analyzed yet", globalCards.get(clafer));
        boolean[] solution = new boolean[globalCard.getHigh()];
        int[][] parents = new int[globalCard.getHigh()][];
        for (AstClafer sub : clafer.getSubs()) {
            int offset = AnalysisUtil.notNull(clafer + " offset not analyzed yet", offsets.get(clafer)).getOffset(sub);

            PartialSolution partialSubSolution = partialSolutions.get(sub);
            // This is possible for partialSubSolution to be null if a child of an abstract
            // extends the abstract. Assume the worst possible case by assuming it is empty.
            if (partialSubSolution != null) {
                System.arraycopy(partialSubSolution.solution, 0, solution, offset, partialSubSolution.solution.length);
            }
        }
        partialSolutions.put(clafer, new PartialSolution(solution, parents));

        for (AstConcreteClafer child : clafer.getChildren()) {
            analyze(child, globalCards, formats, partialSolutions);
        }
    }

    private static void analyze(
            AstConcreteClafer clafer,
            Map<AstClafer, Card> globalCards,
            Map<AstClafer, Format> formats,
            Map<AstClafer, PartialSolution> partialSolutions) {
        Card globalCard = AnalysisUtil.notNull(clafer + " global card not analyzed yet", globalCards.get(clafer));
        Format format = AnalysisUtil.notNull(clafer.getName() + " format not analyzed yet", formats.get(clafer));

        boolean[] solution = new boolean[globalCard.getHigh()];
        TIntArrayList[] parents = new TIntArrayList[globalCard.getHigh()];
        for (int i = 0; i < parents.length; i++) {
            parents[i] = new TIntArrayList();
        }

        if (!clafer.hasParent()) {
            Arrays.fill(solution, 0, globalCard.getLow(), true);
        } else {
            PartialSolution partialParentSolution = partialSolutions.get(clafer.getParent());
            int lowCard = clafer.getCard().getLow();
            int highCard = clafer.getCard().getHigh();
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
        }
        partialSolutions.put(clafer, new PartialSolution(solution, toArray(parents)));

        for (AstConcreteClafer child : clafer.getChildren()) {
            analyze(child, globalCards, formats, partialSolutions);
        }
    }

    private static int[][] toArray(TIntArrayList[] list) {
        int[][] array = new int[list.length][];
        for (int i = 0; i < array.length; i++) {
            array[i] = list[i].toNativeArray();
        }
        return array;
    }

    public static class PartialSolution {
        // solution[i] = True <=> i exists
        // solution[i] = null <=> i unknown

        private final boolean[] solution;
        // parent[i] = the list of possible parents
        private final int[][] parent;

        private PartialSolution(boolean[] solution, int[][] parent) {
            this.solution = solution;
            this.parent = parent;
        }

        /**
         * @param id
         * @return - true if id exists, false if unknown.
         */
        public boolean hasClafer(int id) {
            return solution[id];
        }

        public int[] getKnownClafers() {
            return Util.trues(solution);
        }

        public int[] getUnknownClafers() {
            return Util.falses(solution);
        }

        /**
         * @param id
         * @return - id's possible parents
         */
        public int[] getPossibleParents(int id) {
            return parent[id];
        }

        public int size() {
            return solution.length;
        }

        public static PartialSolution rootSolution() {
            return new PartialSolution(new boolean[]{true}, new int[0][]);
        }
    }
}
