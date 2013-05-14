package org.clafer.analysis;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
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
            List<AstClafer> reverseOffsets = new ArrayList<AstClafer>();
            int offset = 0;
            List<AstClafer> subs = new ArrayList<AstClafer>(abstractClafer.getSubs());
            AnalysisUtil.descendingGlobalCardRatio(subs, globalCards);
            List<AstClafer> greedy = subs;
            for (AstClafer sub : greedy) {
                offsets.put(sub, offset);
                int skip = globalCards.get(sub).getHigh();
                for (int i = 0; i < skip; i++) {
                    reverseOffsets.add(sub);
                }
                offset += skip;

                if (sub instanceof AstAbstractClafer) {
                    Offsets subOffsets = AnalysisUtil.notNull(sub + " offset not analyzed yet", offsetMap.get((AstAbstractClafer) sub));
                    for (Entry<AstClafer, Integer> entry : subOffsets.offsets.entrySet()) {
                        offsets.put(entry.getKey(), entry.getValue() + offset);
                    }
                }
            }
            offsetMap.put(abstractClafer, new Offsets(abstractClafer, offsets, reverseOffsets.toArray(new AstClafer[reverseOffsets.size()])));
        }
        return offsetMap;
    }

    public static class Offsets {

        private final AstAbstractClafer sup;
        private final Map<AstClafer, Integer> offsets;
        private final AstClafer[] reverseOffsets;

        Offsets(AstAbstractClafer sup, Map<AstClafer, Integer> offsets, AstClafer[] reverseOffsets) {
            this.sup = Check.notNull(sup);
            this.offsets = Check.notNull(offsets);
            this.reverseOffsets = Check.noNulls(reverseOffsets);

        }

        public int getOffset(AstClafer sub) {
            return AnalysisUtil.notNull(sub + " is not a sub clafer of " + sup, offsets.get(sub)).intValue();
        }

        public AstClafer getClafer(int offset) {
            return reverseOffsets[offset];
        }

        @Override
        public String toString() {
            return sup + "=>" + offsets;
        }
    }
}
