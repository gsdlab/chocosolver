package org.clafer.analysis;

import gnu.trove.TIntHashSet;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import org.clafer.ast.AstBoolExpression;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstCompare;
import org.clafer.ast.AstCompare.Op;
import org.clafer.ast.AstConstantInt;
import org.clafer.ast.AstExpression;
import org.clafer.ast.AstJoin;
import org.clafer.ast.AstJoinRef;
import org.clafer.ast.AstModel;
import org.clafer.ast.AstRef;
import org.clafer.ast.AstSetExpression;
import org.clafer.ast.AstThis;
import org.clafer.ast.AstUpcast;
import org.clafer.collection.Pair;
import org.clafer.collection.Triple;

/**
 *
 * @author jimmy
 */
public class PartialIntAnalysis {

    private final AstClafer thisType;
    private final Map<AstExpression, AstClafer> types;

    private PartialIntAnalysis(AstClafer thisType, Map<AstExpression, AstClafer> types) {
        this.thisType = thisType;
        this.types = types;
    }

    public static Map<AstRef, int[]> analyze(AstModel model, Map<AstExpression, AstClafer> types) {
        Map<AstRef, int[]> partialInts = new HashMap<AstRef, int[]>();

        Map<AstRef, Pair<List<AstClafer>, TIntHashSet>> subMap = new HashMap<AstRef, Pair<List<AstClafer>, TIntHashSet>>();
        for (AstClafer clafer : AnalysisUtil.getClafers(model)) {
            PartialIntAnalysis analysis = new PartialIntAnalysis(clafer, types);
            for (AstBoolExpression constraint : clafer.getConstraints()) {
                Triple<AstRef, AstClafer, Integer> triple = analysis.analyze(constraint);
                if (triple == null) {
                    continue;
                }
                AstRef key = triple.getFst();
                Pair<List<AstClafer>, TIntHashSet> subs = subMap.get(key);
                if (subs == null) {
                    subs = new Pair<List<AstClafer>, TIntHashSet>(new ArrayList<AstClafer>(), new TIntHashSet());
                    subs.getSnd().add(0);
                    subMap.put(key, subs);
                }
                subs.getFst().add(triple.getSnd());
                subs.getSnd().add(triple.getThd());
            }
        }

        for (Entry<AstRef, Pair<List<AstClafer>, TIntHashSet>> entry : subMap.entrySet()) {
            if (AnalysisUtil.isUnionType(AnalysisUtil.getTopParent(entry.getKey().getSourceType()), entry.getValue().getFst())) {
                partialInts.put(entry.getKey(), entry.getValue().getSnd().toArray());
            }
        }

        return partialInts;
    }

    private Triple<AstRef, AstClafer, Integer> analyze(AstBoolExpression exp) {
        if (exp instanceof AstCompare) {
            AstCompare compare = (AstCompare) exp;
            if (Op.Equal.equals(compare.getOp())) {
                if (compare.getLeft() instanceof AstJoinRef && compare.getRight() instanceof AstConstantInt) {
                    return analyzeEqual((AstJoinRef) compare.getLeft(), (AstConstantInt) compare.getRight());
                }
                if (compare.getRight() instanceof AstJoinRef && compare.getLeft() instanceof AstConstantInt) {
                    return analyzeEqual((AstJoinRef) compare.getRight(), (AstConstantInt) compare.getLeft());
                }
            }
        }
        return null;
    }

    private Triple<AstRef, AstClafer, Integer> analyzeEqual(AstJoinRef exp, AstConstantInt constant) {
        Pair<AstRef, AstClafer> expAnalysis = analyze(exp);
        if (expAnalysis == null) {
            return null;
        }
        return new Triple<AstRef, AstClafer, Integer>(expAnalysis.getFst(), expAnalysis.getSnd(), constant.getValue());
    }

    private Pair<AstRef, AstClafer> analyze(AstJoinRef exp) {
        AstClafer derefAnalysis = analyze(exp.getDeref());
        if (derefAnalysis == null) {
            return null;
        }
        return new Pair<AstRef, AstClafer>(getType(exp.getDeref()).getRef(), derefAnalysis);
    }

    private AstClafer analyze(AstSetExpression exp) {
        AstClafer type = AnalysisUtil.getTopParent(thisType);
        while (true) {
            if (exp instanceof AstThis) {
                return type;
            } else if (exp instanceof AstUpcast) {
                exp = ((AstUpcast) exp).getBase();
                type = getType(exp);
            } else if (exp instanceof AstJoin) {
                exp = ((AstJoin) exp).getLeft();
            } else {
                return null;
            }
        }
    }

    private AstClafer getType(AstExpression exp) {
        return AnalysisUtil.notNull(exp + " type not analyzed yet", types.get(exp));
    }
}
