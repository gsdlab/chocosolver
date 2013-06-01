package org.clafer.ast.analysis;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstConstraint;
import org.clafer.ast.AstExpr;
import org.clafer.ast.AstModel;
import org.clafer.ast.AstRef;
import org.clafer.ast.AstUtil;
import org.clafer.ast.Card;
import org.clafer.collection.Pair;
import org.clafer.scope.Scope;

/**
 *
 * @author jimmy
 */
public class Analysis {

    private final AstModel model;
    private final List<AstClafer> clafers;
    private final List<AstAbstractClafer> abstractClafers;
    private final List<AstConcreteClafer> concreteClafers;
    private final List<AstConstraint> constraints;
    private final Map<AstConcreteClafer, Card> cardMap;
    private final Map<AstClafer, Card> globalCardMap;
    private final Scope scope;
    private final Map<AstAbstractClafer, Integer> depthMap;
    private final Map<AstClafer, Format> formatMap;
    private final Map<AstAbstractClafer, Offsets> offsetMap;
    private final Map<AstClafer, PartialSolution> partialSolutionMap;
    private final Map<AstRef, int[][]> partialIntsMap;
    private final Map<AstExpr, AstClafer> typeMap;

    Analysis(AstModel model, Scope scope) {
        this(model, scope, AstUtil.getConcreteClafers(model));
    }

    Analysis(AstModel model, Scope scope, List<AstConcreteClafer> concreteClafers) {
        this(model, scope, append(model.getAbstractClafers(), concreteClafers), model.getAbstractClafers(), concreteClafers);
    }

    Analysis(AstModel model, Scope scope, List<AstClafer> clafers, List<AstAbstractClafer> abstractClafers, List<AstConcreteClafer> concreteClafers) {
        this(model, AstUtil.getClafers(model), abstractClafers, concreteClafers, AstUtil.getNestedConstraints(model), buildCardMap(clafers), null, scope, null, null, null, null, null, null);
    }

    Analysis(AstModel model, List<AstClafer> clafers, List<AstAbstractClafer> abstractClafers, List<AstConcreteClafer> concreteClafers, List<AstConstraint> constraints, Map<AstConcreteClafer, Card> cardMap, Map<AstClafer, Card> globalCardMap, Scope scope, Map<AstAbstractClafer, Integer> depthMap, Map<AstClafer, Format> formatMap, Map<AstAbstractClafer, Offsets> offsetMap, Map<AstClafer, PartialSolution> partialSolutionMap, Map<AstRef, int[][]> partialIntsMap, Map<AstExpr, AstClafer> typeMap) {
        this.model = model;
        this.clafers = clafers;
        this.abstractClafers = abstractClafers;
        this.concreteClafers = concreteClafers;
        this.constraints = constraints;
        this.cardMap = cardMap;
        this.globalCardMap = globalCardMap;
        this.scope = scope;
        this.depthMap = depthMap;
        this.formatMap = formatMap;
        this.offsetMap = offsetMap;
        this.partialSolutionMap = partialSolutionMap;
        this.partialIntsMap = partialIntsMap;
        this.typeMap = typeMap;
    }

    private static Map<AstConcreteClafer, Card> buildCardMap(List<AstClafer> clafers) {
        Map<AstConcreteClafer, Card> cardMap = new HashMap<AstConcreteClafer, Card>();
        for (AstClafer clafer : clafers) {
            if (clafer instanceof AstConcreteClafer) {
                AstConcreteClafer concreteClafer = (AstConcreteClafer) clafer;
                cardMap.put(concreteClafer, concreteClafer.getCard());
            }
        }
        return cardMap;
    }

    private static List<AstClafer> append(List<? extends AstClafer> abstractClafers, List<? extends AstConcreteClafer> concreteClafers) {
        List<AstClafer> clafers = new ArrayList<AstClafer>(abstractClafers.size() + concreteClafers.size());
        clafers.addAll(abstractClafers);
        clafers.addAll(concreteClafers);
        return clafers;
    }

    public static Analysis analyze(AstModel model, Scope scope, Analyzer... analyzers) {
        Analysis analysis = new Analysis(model, scope);
        for (Analyzer analyzer : analyzers) {
            analysis = analyzer.analyze(analysis);
        }
        return analysis;
    }

    private <T> T notNull(String analysisName, T t) {
        if (t == null) {
            throw new AnalysisException(analysisName + " not yet analyzed.");
        }
        return t;
    }

    private <T> T notNull(String key, String analysisName, T t) {
        if (t == null) {
            throw new AnalysisException(analysisName + " for " + key + " not yet analyzed.");
        }
        return t;
    }

    private <T> T notNull(AstClafer key, String analysisName, T t) {
        return notNull(key.getName(), analysisName, t);
    }

    /**
     * Returns the original model. Analyzers are forbidden to alter the original
     * model.
     *
     * @return the original model
     */
    public AstModel getModel() {
        return model;
    }

    public List<AstClafer> getClafers() {
        return Collections.unmodifiableList(clafers);
    }

    public List<AstAbstractClafer> getAbstractClafers() {
        return Collections.unmodifiableList(abstractClafers);
    }

    public Analysis withAbstractClafers(List<AstAbstractClafer> abstractClafers) {
        return new Analysis(model, append(abstractClafers, concreteClafers), abstractClafers, concreteClafers, constraints, cardMap, globalCardMap, scope, depthMap, formatMap, offsetMap, partialSolutionMap, partialIntsMap, typeMap);
    }

    public List<AstConcreteClafer> getConcreteClafers() {
        return Collections.unmodifiableList(concreteClafers);
    }

    public Analysis withConcreteClafers(List<AstConcreteClafer> concreteClafers) {
        return new Analysis(model, append(abstractClafers, concreteClafers), abstractClafers, concreteClafers, constraints, cardMap, globalCardMap, scope, depthMap, formatMap, offsetMap, partialSolutionMap, partialIntsMap, typeMap);
    }

    public List<AstConstraint> getConstraints() {
        return Collections.unmodifiableList(constraints);
    }

    public Analysis withConstraints(List<AstConstraint> constraints) {
        return new Analysis(model, clafers, abstractClafers, concreteClafers, constraints, cardMap, globalCardMap, scope, depthMap, formatMap, offsetMap, partialSolutionMap, partialIntsMap, typeMap);
    }

    public Card getCard(AstConcreteClafer clafer) {
        return notNull(clafer, "Card", getCardMap().get(clafer));
    }

    public Map<AstConcreteClafer, Card> getCardMap() {
        return notNull("Card", cardMap);
    }

    public Analysis withCardMap(Map<AstConcreteClafer, Card> cardMap) {
        return new Analysis(model, clafers, abstractClafers, concreteClafers, constraints, cardMap, globalCardMap, scope, depthMap, formatMap, offsetMap, partialSolutionMap, partialIntsMap, typeMap);
    }

    public Card getGlobalCard(AstClafer clafer) {
        return notNull(clafer, "GlobalCard", getGlobalCardMap().get(clafer));
    }

    public Map<AstClafer, Card> getGlobalCardMap() {
        return notNull("Global card", globalCardMap);
    }

    public Analysis withGlobalCardMap(Map<AstClafer, Card> globalCardMap) {
        return new Analysis(model, clafers, abstractClafers, concreteClafers, constraints, cardMap, globalCardMap, scope, depthMap, formatMap, offsetMap, partialSolutionMap, partialIntsMap, typeMap);
    }

    public int getScope(AstClafer clafer) {
        return getScope().getScope(clafer);
    }

    public Scope getScope() {
        return notNull("Scope", scope);
    }

    public Analysis withScope(Scope scope) {
        return new Analysis(model, clafers, abstractClafers, concreteClafers, constraints, cardMap, globalCardMap, scope, depthMap, formatMap, offsetMap, partialSolutionMap, partialIntsMap, typeMap);
    }

    public int getDepth(AstAbstractClafer clafer) {
        return notNull(clafer, "Depth", getDepthMap().get(clafer)).intValue();
    }

    public Map<AstAbstractClafer, Integer> getDepthMap() {
        return notNull("Depth", depthMap);
    }

    public Analysis withDepthMap(Map<AstAbstractClafer, Integer> depthMap) {
        return new Analysis(model, clafers, abstractClafers, concreteClafers, constraints, cardMap, globalCardMap, scope, depthMap, formatMap, offsetMap, partialSolutionMap, partialIntsMap, typeMap);
    }

    public Format getFormat(AstClafer clafer) {
        return notNull(clafer, "Format", getFormatMap().get(clafer));
    }

    public Map<AstClafer, Format> getFormatMap() {
        return notNull("Format", formatMap);
    }

    public Analysis withFormatMap(Map<AstClafer, Format> formatMap) {
        return new Analysis(model, clafers, abstractClafers, concreteClafers, constraints, cardMap, globalCardMap, scope, depthMap, formatMap, offsetMap, partialSolutionMap, partialIntsMap, typeMap);
    }

    public Pair<AstConcreteClafer, Integer> getConcreteId(AstClafer clafer, int id) {
        AstClafer subClafer = clafer;
        int curId = id;
        while (subClafer instanceof AstAbstractClafer) {
            Offsets offset = getOffsets((AstAbstractClafer) subClafer);
            subClafer = offset.getClafer(curId);
            curId -= offset.getOffset(subClafer);
        }
        return new Pair<AstConcreteClafer, Integer>((AstConcreteClafer) subClafer, curId);
    }

    public Pair<AstAbstractClafer, Integer> getSuperId(AstClafer clafer, int id) {
        assert clafer.hasSuperClafer();
        int offset = getOffsets(clafer.getSuperClafer()).getOffset(clafer);
        return new Pair<AstAbstractClafer, Integer>(clafer.getSuperClafer(), id + offset);
    }

    public List<Pair<AstAbstractClafer, Integer>> getSuperIds(AstClafer clafer, int id) {
        List<Pair<AstAbstractClafer, Integer>> superIds = new ArrayList<Pair<AstAbstractClafer, Integer>>();
        AstClafer supClafer = clafer;
        int curId = id;
        while (supClafer.hasSuperClafer()) {
            Pair<AstAbstractClafer, Integer> superId = getSuperId(supClafer, curId);
            superIds.add(superId);
            supClafer = superId.getFst();
            curId = superId.getSnd().intValue();
        }
        return superIds;
    }

    public List<Pair<AstClafer, Integer>> getHierarcyOffsets(AstClafer clafer) {
        return getHierarcyIds(clafer, 0);
    }

    public List<Pair<AstClafer, Integer>> getHierarcyIds(AstClafer clafer, int id) {
        List<Pair<AstClafer, Integer>> superIds = new ArrayList<Pair<AstClafer, Integer>>();
        superIds.add(new Pair<AstClafer, Integer>(clafer, id));
        AstClafer supClafer = clafer;
        int curId = id;
        while (supClafer.hasSuperClafer()) {
            Pair<AstAbstractClafer, Integer> superId = getSuperId(supClafer, curId);
            superIds.add(new Pair<AstClafer, Integer>(superId));
            supClafer = superId.getFst();
            curId = superId.getSnd().intValue();
        }
        return superIds;
    }

    public Offsets getOffsets(AstAbstractClafer clafer) {
        return notNull(clafer, "Offset", getOffsetMap().get(clafer));
    }

    public Map<AstAbstractClafer, Offsets> getOffsetMap() {
        return notNull("Offset", offsetMap);
    }

    public Analysis withOffsetMap(Map<AstAbstractClafer, Offsets> offsetMap) {
        return new Analysis(model, clafers, abstractClafers, concreteClafers, constraints, cardMap, globalCardMap, scope, depthMap, formatMap, offsetMap, partialSolutionMap, partialIntsMap, typeMap);
    }

    public PartialSolution getPartialSolution(AstClafer clafer) {
        return notNull(clafer, "Partial solution", getPartialSolutionMap().get(clafer));
    }

    public Map<AstClafer, PartialSolution> getPartialSolutionMap() {
        return notNull("Partial solution", partialSolutionMap);
    }

    public Analysis withPartialSolutionMap(Map<AstClafer, PartialSolution> partialSolutionMap) {
        return new Analysis(model, clafers, abstractClafers, concreteClafers, constraints, cardMap, globalCardMap, scope, depthMap, formatMap, offsetMap, partialSolutionMap, partialIntsMap, typeMap);
    }

    public int[][] getPartialInts(AstRef ref) {
        return notNull(ref.getSourceType(), "Partial integer", getPartialIntsMap().get(ref));
    }

    public Map<AstRef, int[][]> getPartialIntsMap() {
        return notNull("Partial integer", partialIntsMap);
    }

    public Analysis withPartialIntsMap(Map<AstRef, int[][]> partialIntsMap) {
        return new Analysis(model, clafers, abstractClafers, concreteClafers, constraints, cardMap, globalCardMap, scope, depthMap, formatMap, offsetMap, partialSolutionMap, partialIntsMap, typeMap);
    }

    public AstClafer getType(AstExpr expr) {
        return notNull(expr.toString(), "Type", getTypeMap().get(expr));
    }

    public Map<AstExpr, AstClafer> getTypeMap() {
        return notNull("Type", typeMap);
    }

    public Analysis withTypeMap(Map<AstExpr, AstClafer> typeMap) {
        return new Analysis(model, clafers, abstractClafers, concreteClafers, constraints, cardMap, globalCardMap, scope, depthMap, formatMap, offsetMap, partialSolutionMap, partialIntsMap, typeMap);
    }
}
