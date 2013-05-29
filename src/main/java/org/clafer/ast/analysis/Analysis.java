package org.clafer.ast.analysis;

import java.util.ArrayList;
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
    private final Map<AstClafer, AstConstraint[]> constraintsMap;
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
        this(model, AstUtil.getClafers(model), abstractClafers, concreteClafers, buildConstraintsMap(clafers), buildCardMap(clafers), null, scope, null, null, null, null, null, null);
    }

    Analysis(AstModel model, List<AstClafer> clafers, List<AstAbstractClafer> abstractClafers, List<AstConcreteClafer> concreteClafers, Map<AstClafer, AstConstraint[]> constraintsMap, Map<AstConcreteClafer, Card> cardMap, Map<AstClafer, Card> globalCardMap, Scope scope, Map<AstAbstractClafer, Integer> depthMap, Map<AstClafer, Format> formatMap, Map<AstAbstractClafer, Offsets> offsetMap, Map<AstClafer, PartialSolution> partialSolutionMap, Map<AstRef, int[][]> partialIntsMap, Map<AstExpr, AstClafer> typeMap) {
        this.model = model;
        this.clafers = clafers;
        this.abstractClafers = abstractClafers;
        this.concreteClafers = concreteClafers;
        this.constraintsMap = constraintsMap;
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

    private static Map<AstClafer, AstConstraint[]> buildConstraintsMap(List<AstClafer> clafers) {
        Map<AstClafer, AstConstraint[]> constraintsMap = new HashMap<AstClafer, AstConstraint[]>();
        for (AstClafer clafer : clafers) {
            constraintsMap.put(clafer,
                    clafer.getConstraints().toArray(new AstConstraint[clafer.getConstraints().size()]));
        }
        return constraintsMap;
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

    public AstModel getModel() {
        return model;
    }

    public List<AstClafer> getClafers() {
        return clafers;
    }

    public List<AstAbstractClafer> getAbstractClafers() {
        return abstractClafers;
    }

    public Analysis withAbstractClafers(List<AstAbstractClafer> abstractClafers) {
        return new Analysis(model, append(abstractClafers, concreteClafers), abstractClafers, concreteClafers, constraintsMap, cardMap, globalCardMap, scope, depthMap, formatMap, offsetMap, partialSolutionMap, partialIntsMap, typeMap);
    }

    public List<AstConcreteClafer> getConcreteClafers() {
        return concreteClafers;
    }

    public Analysis withConcreteClafers(List<AstConcreteClafer> concreteClafers) {
        return new Analysis(model, append(abstractClafers, concreteClafers), abstractClafers, concreteClafers, constraintsMap, cardMap, globalCardMap, scope, depthMap, formatMap, offsetMap, partialSolutionMap, partialIntsMap, typeMap);
    }

    public AstConstraint[] getConstraints(AstClafer clafer) {
        return notNull(clafer, "Constraint", getConstraintsMap().get(clafer));
    }

    public Map<AstClafer, AstConstraint[]> getConstraintsMap() {
        return notNull("Constraint", constraintsMap);
    }

    public Analysis withConstraintsMap(Map<AstClafer, AstConstraint[]> constraintsMap) {
        return new Analysis(model, clafers, abstractClafers, concreteClafers, constraintsMap, cardMap, globalCardMap, scope, depthMap, formatMap, offsetMap, partialSolutionMap, partialIntsMap, typeMap);
    }

    public Card getCard(AstConcreteClafer clafer) {
        return notNull(clafer, "Card", getCardMap().get(clafer));
    }

    public Map<AstConcreteClafer, Card> getCardMap() {
        return notNull("Card", cardMap);
    }

    public Analysis withCardMap(Map<AstConcreteClafer, Card> cardMap) {
        return new Analysis(model, clafers, abstractClafers, concreteClafers, constraintsMap, cardMap, globalCardMap, scope, depthMap, formatMap, offsetMap, partialSolutionMap, partialIntsMap, typeMap);
    }

    public Card getGlobalCard(AstClafer clafer) {
        return notNull(clafer, "GlobalCard", getGlobalCardMap().get(clafer));
    }

    public Map<AstClafer, Card> getGlobalCardMap() {
        return notNull("Global card", globalCardMap);
    }

    public Analysis withGlobalCardMap(Map<AstClafer, Card> globalCardMap) {
        return new Analysis(model, clafers, abstractClafers, concreteClafers, constraintsMap, cardMap, globalCardMap, scope, depthMap, formatMap, offsetMap, partialSolutionMap, partialIntsMap, typeMap);
    }

    public int getScope(AstClafer clafer) {
        return getScope().getScope(clafer);
    }

    public Scope getScope() {
        return notNull("Scope", scope);
    }

    public Analysis withScope(Scope scope) {
        return new Analysis(model, clafers, abstractClafers, concreteClafers, constraintsMap, cardMap, globalCardMap, scope, depthMap, formatMap, offsetMap, partialSolutionMap, partialIntsMap, typeMap);
    }

    public int getDepth(AstAbstractClafer clafer) {
        return notNull(clafer, "Depth", getDepthMap().get(clafer)).intValue();
    }

    public Map<AstAbstractClafer, Integer> getDepthMap() {
        return notNull("Depth", depthMap);
    }

    public Analysis withDepthMap(Map<AstAbstractClafer, Integer> depthMap) {
        return new Analysis(model, clafers, abstractClafers, concreteClafers, constraintsMap, cardMap, globalCardMap, scope, depthMap, formatMap, offsetMap, partialSolutionMap, partialIntsMap, typeMap);
    }

    public Format getFormat(AstClafer clafer) {
        return notNull(clafer, "Format", getFormatMap().get(clafer));
    }

    public Map<AstClafer, Format> getFormatMap() {
        return notNull("Format", formatMap);
    }

    public Analysis withFormatMap(Map<AstClafer, Format> formatMap) {
        return new Analysis(model, clafers, abstractClafers, concreteClafers, constraintsMap, cardMap, globalCardMap, scope, depthMap, formatMap, offsetMap, partialSolutionMap, partialIntsMap, typeMap);
    }

    public Offsets getOffsets(AstAbstractClafer clafer) {
        return notNull(clafer, "Offset", getOffsetMap().get(clafer));
    }

    public Map<AstAbstractClafer, Offsets> getOffsetMap() {
        return notNull("Offset", offsetMap);
    }

    public Analysis withOffsetMap(Map<AstAbstractClafer, Offsets> offsetMap) {
        return new Analysis(model, clafers, abstractClafers, concreteClafers, constraintsMap, cardMap, globalCardMap, scope, depthMap, formatMap, offsetMap, partialSolutionMap, partialIntsMap, typeMap);
    }

    public PartialSolution getPartialSolution(AstClafer clafer) {
        return notNull(clafer, "Partial solution", getPartialSolutionMap().get(clafer));
    }

    public Map<AstClafer, PartialSolution> getPartialSolutionMap() {
        return notNull("Partial solution", partialSolutionMap);
    }

    public Analysis withPartialSolutionMap(Map<AstClafer, PartialSolution> partialSolutionMap) {
        return new Analysis(model, clafers, abstractClafers, concreteClafers, constraintsMap, cardMap, globalCardMap, scope, depthMap, formatMap, offsetMap, partialSolutionMap, partialIntsMap, typeMap);
    }

    public int[][] getPartialInts(AstRef ref) {
        return notNull(ref.getSourceType(), "Partial integer", getPartialIntsMap().get(ref));
    }

    public Map<AstRef, int[][]> getPartialIntsMap() {
        return notNull("Partial integer", partialIntsMap);
    }

    public Analysis withPartialIntsMap(Map<AstRef, int[][]> partialIntsMap) {
        return new Analysis(model, clafers, abstractClafers, concreteClafers, constraintsMap, cardMap, globalCardMap, scope, depthMap, formatMap, offsetMap, partialSolutionMap, partialIntsMap, typeMap);
    }

    public AstClafer getType(AstExpr expr) {
        return notNull(expr.toString(), "Type", getTypeMap().get(expr));
    }

    public Map<AstExpr, AstClafer> getTypeMap() {
        return notNull("Type", typeMap);
    }

    public Analysis withTypeMap(Map<AstExpr, AstClafer> typeMap) {
        return new Analysis(model, clafers, abstractClafers, concreteClafers, constraintsMap, cardMap, globalCardMap, scope, depthMap, formatMap, offsetMap, partialSolutionMap, partialIntsMap, typeMap);
    }
}
