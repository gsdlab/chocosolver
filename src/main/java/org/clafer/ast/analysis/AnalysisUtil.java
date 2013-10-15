package org.clafer.ast.analysis;

import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import org.clafer.common.Check;
import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstUtil;
import org.clafer.ast.Card;

/**
 *
 * @author jimmy
 */
@Deprecated
public class AnalysisUtil {

    private AnalysisUtil() {
    }

    public static <T> T notNull(String message, T t) {
        if (t == null) {
            throw new AnalysisException(message);
        }
        return t;
    }

    public static void descendingDepths(List<AstAbstractClafer> abstractClafers) {
        Collections.sort(abstractClafers, DepthComparator);
    }

    public static void descendingGlobalCardRatio(
            List<AstClafer> clafers,
            Map<AstClafer, Card> globalCards) {
        Collections.sort(clafers, new GlobalCardRatioComparator(globalCards));
    }
    private static final Comparator<AstAbstractClafer> DepthComparator = new Comparator<AstAbstractClafer>() {
        @Override
        public int compare(AstAbstractClafer o1, AstAbstractClafer o2) {
            int depth1 = AstUtil.getDepth(o1);
            int depth2 = AstUtil.getDepth(o2);
            return depth1 > depth2 ? -1 : (depth1 == depth2 ? 0 : 1);
        }
    };

    private static class GlobalCardRatioComparator implements Comparator<AstClafer> {

        private final Map<AstClafer, Card> globalCards;

        GlobalCardRatioComparator(Map<AstClafer, Card> globalCards) {
            this.globalCards = Check.notNull(globalCards);
        }

        @Override
        public int compare(AstClafer o1, AstClafer o2) {
            Card card1 = notNull(o1 + " Global card not analyzed yet", globalCards.get(o1));
            Card card2 = notNull(o2 + " Global card not analyzed yet", globalCards.get(o2));
            double ratio1 = ((double) card1.getLow()) / ((double) card1.getHigh());
            double ratio2 = ((double) card2.getLow()) / ((double) card2.getHigh());
            return Double.compare(ratio1, ratio2);
        }
    }
}
