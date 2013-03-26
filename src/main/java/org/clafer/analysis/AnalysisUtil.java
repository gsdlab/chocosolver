package org.clafer.analysis;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.clafer.Check;
import org.clafer.Util;
import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstModel;
import org.clafer.ast.Card;

/**
 *
 * @author jimmy
 */
public class AnalysisUtil {

    public static <T> T notNull(String message, T t) {
        if (t == null) {
            throw new AnalysisException(message);
        }
        return t;
    }

    public static Map<String, AstClafer> getClafersMap(AstModel model) {
        List<AstClafer> clafers = getClafers(model);
        Map<String, AstClafer> map = new HashMap<String, AstClafer>();
        for (AstClafer clafer : clafers) {
            map.put(clafer.getName(), clafer);
        }
        assert map.size() == clafers.size();
        return map;
    }

    public static List<AstClafer> getClafers(AstModel model) {
        List<AstClafer> clafers = new ArrayList<AstClafer>();
        for (AstAbstractClafer abstractClafer : model.getAbstractClafers()) {
            clafers.add(abstractClafer);
            getNestedChildClafers(abstractClafer, clafers);
        }
        for (AstConcreteClafer topClafer : model.getTopClafers()) {
            clafers.add(topClafer);
            getNestedChildClafers(topClafer, clafers);
        }
        return clafers;
    }

    public static List<AstConcreteClafer> getConcreteClafers(AstModel model) {
        List<AstConcreteClafer> clafers = new ArrayList<AstConcreteClafer>();
        for (AstAbstractClafer abstractClafer : model.getAbstractClafers()) {
            getNestedChildClafers(abstractClafer, clafers);
        }
        for (AstConcreteClafer topClafer : model.getTopClafers()) {
            clafers.add(topClafer);
            getNestedChildClafers(topClafer, clafers);
        }
        return clafers;
    }

    public static AstClafer getTopParent(AstClafer clafer) {
        if (clafer instanceof AstConcreteClafer) {
            AstConcreteClafer concrete = (AstConcreteClafer) clafer;
            if (concrete.hasParent()) {
                return getTopParent(((AstConcreteClafer) clafer).getParent());
            }
        }
        return clafer;
    }

    public static List<AstClafer> getNestedClafers(AstClafer clafer) {
        List<AstClafer> clafers = new ArrayList<AstClafer>();
        clafers.add(clafer);
        getNestedChildClafers(clafer, clafers);
        return clafers;
    }

    private static void getNestedChildClafers(AstClafer clafer, List<? super AstConcreteClafer> clafers) {
        for (AstConcreteClafer child : clafer.getChildren()) {
            clafers.add(child);
            getNestedChildClafers(child, clafers);
        }
    }

    public static List<AstConcreteClafer> getConcreteSubs(AstClafer clafer) {
        List<AstConcreteClafer> subs = new ArrayList<AstConcreteClafer>();
        getConcreteSubs(clafer, subs);
        return subs;
    }

    private static void getConcreteSubs(AstClafer sub, Collection<AstConcreteClafer> subs) {
        if (sub instanceof AstAbstractClafer) {
            AstAbstractClafer sup = (AstAbstractClafer) sub;
            for (AstClafer subsub : sup.getSubs()) {
                getConcreteSubs(subsub, subs);
            }
        } else {
            subs.add((AstConcreteClafer) sub);
        }
    }

    public static AstAbstractClafer[] getSupers(final AstClafer clafer) {
        int count = 0;
        AstAbstractClafer sup = clafer.getSuperClafer();
        while (sup != null) {
            count++;
            sup = sup.getSuperClafer();
        }

        AstAbstractClafer[] sups = new AstAbstractClafer[count];
        sup = clafer.getSuperClafer();
        for (int i = 0; i < sups.length; i++) {
            sups[i] = sup;
            sup = sup.getSuperClafer();
        }
        return sups;
    }

    /**
     * @return - to is a super class of from?
     */
    public static boolean isAssignable(AstClafer from, AstClafer to) {
        return to.equals(from) || Util.in(to, getSupers(from));
    }

    public static boolean hasNonEmptyIntersectionType(AstClafer t1, AstClafer t2) {
        if (t1.equals(t2)) {
            return true;
        }
        if (t1 instanceof AstAbstractClafer) {
            return Util.in(t1, getSupers(t2));
        }
        if (t2 instanceof AstAbstractClafer) {
            return Util.in(t2, getSupers(t1));
        }
        return false;
    }

    public static boolean isUnionType(AstClafer union, List<AstClafer> ts) {
        Set<AstConcreteClafer> unionSubs = new HashSet<AstConcreteClafer>();
        getConcreteSubs(union, unionSubs);
        Set<AstConcreteClafer> tSubs = new HashSet<AstConcreteClafer>();
        for (AstClafer t : ts) {
            getConcreteSubs(t, tSubs);
        }
        return unionSubs.equals(tSubs);
    }

    public static void descendingDepths(
            List<AstAbstractClafer> abstractClafers,
            Map<AstAbstractClafer, Integer> depths) {
        Collections.sort(abstractClafers, new DepthComparator(depths));
    }

    public static void descendingGlobalCardRatio(
            List<AstClafer> clafers,
            Map<AstClafer, Card> globalCards) {
        Collections.sort(clafers, new GlobalCardRatioComparator(globalCards));
    }

    private static class DepthComparator implements Comparator<AstAbstractClafer> {

        private final Map<AstAbstractClafer, Integer> depths;

        public DepthComparator(Map<AstAbstractClafer, Integer> depths) {
            this.depths = Check.notNull(depths);
        }

        @Override
        public int compare(AstAbstractClafer o1, AstAbstractClafer o2) {
            int depth1 = notNull(o1 + " depth not analyzed yet", depths.get(o1)).intValue();
            int depth2 = notNull(o2 + " depth not analyzed yet", depths.get(o2)).intValue();
            return depth1 > depth2 ? -1 : (depth1 == depth2 ? 0 : 1);
        }
    }

    private static class GlobalCardRatioComparator implements Comparator<AstClafer> {

        private final Map<AstClafer, Card> globalCards;

        public GlobalCardRatioComparator(Map<AstClafer, Card> globalCards) {
            this.globalCards = Check.notNull(globalCards);
        }

        @Override
        public int compare(AstClafer o1, AstClafer o2) {
            Card card1 = notNull(o1 + " global card not analyzed yet", globalCards.get(o1));
            Card card2 = notNull(o2 + " global card not analyzed yet", globalCards.get(o2));
            double ratio1 = ((double) card1.getLow()) / ((double) card1.getHigh());
            double ratio2 = ((double) card2.getLow()) / ((double) card2.getHigh());
            return (ratio1 > ratio2) ? -1 : ((ratio1 == ratio2) ? 0 : 1);
        }
    }
}
