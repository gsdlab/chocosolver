package org.clafer.ast.analysis;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstModel;
import org.clafer.ast.Card;

/**
 *
 * @author jimmy
 */
public class AbstractOffsetAnalysis {

    private AbstractOffsetAnalysis() {
    }

    public static Map<AstAbstractClafer, Offsets> analyze(AstModel model, Map<AstClafer, Card> globalCards) {
        Map<AstAbstractClafer, Offsets> offsetMap = new HashMap<AstAbstractClafer, Offsets>();
        for (AstAbstractClafer abstractClafer : model.getAbstractClafers()) {
            Map<AstClafer, Integer> offsets = new HashMap<AstClafer, Integer>();
            List<AstClafer> reverseOffsets = new ArrayList<AstClafer>();
            int offset = 0;
            List<AstClafer> subs = new ArrayList<AstClafer>(abstractClafer.getSubs());
            /*
             * What is this optimization?
             *
             * This optimization is to put more "stable" Clafers near the
             * beginning, to reduce the number of backtracking for LowGroups.
             */
            AnalysisUtil.descendingGlobalCardRatio(subs, globalCards);
            List<AstClafer> greedy = subs;
            for (AstClafer sub : greedy) {
                offsets.put(sub, offset);
                int skip = globalCards.get(sub).getHigh();
                for (int i = 0; i < skip; i++) {
                    reverseOffsets.add(sub);
                }
                offset += skip;
            }
            offsetMap.put(abstractClafer, new Offsets(abstractClafer, offsets, reverseOffsets.toArray(new AstClafer[reverseOffsets.size()])));
        }
        return offsetMap;
    }
}
