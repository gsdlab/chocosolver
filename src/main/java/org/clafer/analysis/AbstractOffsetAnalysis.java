package org.clafer.analysis;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.clafer.Check;
import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstModel;
import org.clafer.ast.Card;

/**
 *
 * @author jimmy
 */
public class AbstractOffsetAnalysis {

    /**
     * What is this optimization?
     * 
     * This optimization is to put more "stable" clafers near the beginning, to reduce
     * the number of backtracking for LowGroups.
     */
    public static Map<AstAbstractClafer, Offsets> analyze(AstModel model, Map<AstClafer, Card> globalCards) {
        Map<AstAbstractClafer, Offsets> offsetMap = new HashMap<AstAbstractClafer, Offsets>();
        for (AstAbstractClafer abstractClafer : model.getAbstractClafers()) {
            Map<AstClafer, Integer> offsets = new HashMap<AstClafer, Integer>();
            int offset = 0;
            List<AstClafer> greedy = AnalysisUtil.descendingGlobalCardRatio(abstractClafer.getSubs(), globalCards);
            for (AstClafer sub : greedy) {
                offsets.put(sub, offset);
                offset += globalCards.get(sub).getHigh();
            }
            offsetMap.put(abstractClafer, new Offsets(abstractClafer, offsets));
        }
        return offsetMap;
    }

    public static class Offsets {

        private final AstAbstractClafer sup;
        private final Map<AstClafer, Integer> offsets;

        public Offsets(AstAbstractClafer sup, Map<AstClafer, Integer> offsets) {
            this.sup = Check.notNull(sup);
            this.offsets = Check.notNull(offsets);
        }

        public int getOffset(AstClafer sub) {
            return AnalysisUtil.notNull(sub + " is not a sub clafer of " + sup, offsets.get(sub)).intValue();
        }
    }
}
