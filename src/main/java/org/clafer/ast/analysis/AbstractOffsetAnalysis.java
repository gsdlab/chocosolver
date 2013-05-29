package org.clafer.ast.analysis;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstClafer;

/**
 *
 * @author jimmy
 */
public class AbstractOffsetAnalysis implements Analyzer {

    @Override
    public Analysis analyze(Analysis analysis) {
        Map<AstAbstractClafer, Offsets> offsetsMap = new HashMap<AstAbstractClafer, Offsets>();
        for (AstAbstractClafer abstractClafer : analysis.getAbstractClafers()) {
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
            AnalysisUtil.descendingGlobalCardRatio(subs, analysis.getGlobalCardMap());
            List<AstClafer> greedy = subs;
            for (AstClafer sub : greedy) {
                offsets.put(sub, offset);
                int skip = analysis.getGlobalCard(sub).getHigh();
                for (int i = 0; i < skip; i++) {
                    reverseOffsets.add(sub);
                }
                offset += skip;
            }
            offsetsMap.put(abstractClafer, new Offsets(abstractClafer, offsets, reverseOffsets.toArray(new AstClafer[reverseOffsets.size()])));
        }
        return analysis.withOffsetMap(offsetsMap);
    }
}
