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
import org.clafer.common.Util;
import org.clafer.scope.Scope;

/**
 *
 * @author jimmy
 */
public class Analysis {

    private final AstModel model;
    private Scope scope;
    private List<AstClafer> clafers;
    private List<AstAbstractClafer> abstractClafers;
    private List<AstConcreteClafer> concreteClafers;
    private List<AstConstraint> constraints;
    private Map<AstConcreteClafer, Card> cardMap;
    private Map<AstClafer, Card> globalCardMap;
    private Map<AstAbstractClafer, Integer> depthMap;
    private Map<AstClafer, Format> formatMap;
    private Map<AstAbstractClafer, Offsets> offsetMap;
    private Map<AstClafer, PartialSolution> partialSolutionMap;
    private Map<AstRef, int[][]> partialIntsMap;
    private Map<AstClafer, AstConcreteClafer[]> breakableChildrenMap;
    private Map<AstRef, int[]> breakableRefsMap;
    private Map<AstClafer, AstRef[]> breakableTargetsMap;
    private Map<AstExpr, Type> typeMap;

    Analysis(AstModel model, Scope scope) {
        this(model, scope, model.getAbstracts(), AstUtil.getConcreteClafers(model));
    }

    Analysis(AstModel model, Scope scope,
            List<AstAbstractClafer> abstractClafers,
            List<AstConcreteClafer> concreteClafers) {
        this(model, scope, append(abstractClafers, concreteClafers), abstractClafers, concreteClafers);
    }

    Analysis(AstModel model, Scope scope,
            List<AstClafer> clafers,
            List<AstAbstractClafer> abstractClafers,
            List<AstConcreteClafer> concreteClafers) {
        this(model, scope, AstUtil.getClafers(model), abstractClafers, concreteClafers, AstUtil.getNestedConstraints(model), buildCardMap(clafers), null, null, null, null, null, null, null, null, null, null);
    }

    Analysis(AstModel model, Scope scope,
            List<AstClafer> clafers,
            List<AstAbstractClafer> abstractClafers,
            List<AstConcreteClafer> concreteClafers,
            List<AstConstraint> constraints,
            Map<AstConcreteClafer, Card> cardMap,
            Map<AstClafer, Card> globalCardMap,
            Map<AstAbstractClafer, Integer> depthMap,
            Map<AstClafer, Format> formatMap,
            Map<AstAbstractClafer, Offsets> offsetMap,
            Map<AstClafer, PartialSolution> partialSolutionMap,
            Map<AstRef, int[][]> partialIntsMap,
            Map<AstClafer, AstConcreteClafer[]> breakableChildrenMap,
            Map<AstRef, int[]> breakableRefsMap,
            Map<AstClafer, AstRef[]> breakableTargetsMap,
            Map<AstExpr, Type> typeMap) {
        this.model = model;
        this.scope = scope;
        this.clafers = clafers;
        this.abstractClafers = abstractClafers;
        this.concreteClafers = concreteClafers;
        this.constraints = constraints;
        this.cardMap = cardMap;
        this.globalCardMap = globalCardMap;
        this.depthMap = depthMap;
        this.formatMap = formatMap;
        this.offsetMap = offsetMap;
        this.partialSolutionMap = partialSolutionMap;
        this.partialIntsMap = partialIntsMap;
        this.breakableChildrenMap = breakableChildrenMap;
        this.breakableRefsMap = breakableRefsMap;
        this.breakableTargetsMap = breakableTargetsMap;
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

    public int getScope(AstClafer clafer) {
        return getScope().getScope(clafer);
    }

    public Scope getScope() {
        return notNull("Scope", scope);
    }

    public Analysis setScope(Scope scope) {
        this.scope = scope;
        return this;
    }

    public List<AstClafer> getClafers() {
        return Collections.unmodifiableList(clafers);
    }

    public List<AstAbstractClafer> getAbstractClafers() {
        return Collections.unmodifiableList(abstractClafers);
    }

    public Analysis setAbstractClafers(List<AstAbstractClafer> abstractClafers) {
        this.clafers = append(abstractClafers, concreteClafers);
        this.abstractClafers = abstractClafers;
        return this;
    }

    public List<AstConcreteClafer> getConcreteClafers() {
        return Collections.unmodifiableList(concreteClafers);
    }

    public Analysis setConcreteClafers(List<AstConcreteClafer> concreteClafers) {
        this.clafers = append(abstractClafers, concreteClafers);
        this.concreteClafers = concreteClafers;
        return this;
    }

    public List<AstConstraint> getConstraints() {
        return Collections.unmodifiableList(constraints);
    }

    public Analysis setConstraints(List<AstConstraint> constraints) {
        this.constraints = constraints;
        return this;
    }

    public Card getCard(AstConcreteClafer clafer) {
        return notNull(clafer, "Card", getCardMap().get(clafer));
    }

    public Map<AstConcreteClafer, Card> getCardMap() {
        return notNull("Card", cardMap);
    }

    public Analysis setCardMap(Map<AstConcreteClafer, Card> cardMap) {
        this.cardMap = cardMap;
        return this;
    }

    public Card getGlobalCard(AstClafer clafer) {
        return notNull(clafer, "GlobalCard", getGlobalCardMap().get(clafer));
    }

    public Map<AstClafer, Card> getGlobalCardMap() {
        return notNull("Global card", globalCardMap);
    }

    public Analysis setGlobalCardMap(Map<AstClafer, Card> globalCardMap) {
        this.globalCardMap = globalCardMap;
        return this;
    }

    public int getDepth(AstAbstractClafer clafer) {
        return notNull(clafer, "Depth", getDepthMap().get(clafer)).intValue();
    }

    public Map<AstAbstractClafer, Integer> getDepthMap() {
        return notNull("Depth", depthMap);
    }

    public Analysis setDepthMap(Map<AstAbstractClafer, Integer> depthMap) {
        this.depthMap = depthMap;
        return this;
    }

    public Format getFormat(AstClafer clafer) {
        return notNull(clafer, "Format", getFormatMap().get(clafer));
    }

    public Map<AstClafer, Format> getFormatMap() {
        return notNull("Format", formatMap);
    }

    public Analysis setFormatMap(Map<AstClafer, Format> formatMap) {
        this.formatMap = formatMap;
        return this;
    }

    public Pair<AstConcreteClafer, Integer> getConcreteId(AstClafer clafer, int id) {
        AstClafer sub = clafer;
        int curId = id;
        while (sub instanceof AstAbstractClafer) {
            Offsets offset = getOffsets((AstAbstractClafer) sub);
            sub = offset.getClafer(curId);
            curId -= offset.getOffset(sub);
        }
        return new Pair<AstConcreteClafer, Integer>((AstConcreteClafer) sub, curId);
    }

    public Pair<AstClafer, Integer> getSubId(AstAbstractClafer clafer, int id) {
        Offsets offsets = getOffsets(clafer);
        AstClafer sub = offsets.getClafer(id);
        return new Pair<AstClafer, Integer>(sub, id - offsets.getOffset(sub));
    }

    public Pair<AstAbstractClafer, Integer> getSuperId(AstClafer clafer, int id) {
        assert clafer.hasSuperClafer();
        int offset = getOffsets(clafer.getSuperClafer()).getOffset(clafer);
        return new Pair<AstAbstractClafer, Integer>(clafer.getSuperClafer(), id + offset);
    }

    public List<Pair<AstAbstractClafer, Integer>> getSuperIds(AstClafer clafer, int id) {
        List<Pair<AstAbstractClafer, Integer>> superIds = new ArrayList<Pair<AstAbstractClafer, Integer>>();
        AstClafer sup = clafer;
        int curId = id;
        while (sup.hasSuperClafer()) {
            Pair<AstAbstractClafer, Integer> superId = getSuperId(sup, curId);
            superIds.add(superId);
            sup = superId.getFst();
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
        AstClafer sup = clafer;
        int curId = id;
        while (sup.hasSuperClafer()) {
            Pair<AstAbstractClafer, Integer> superId = getSuperId(sup, curId);
            superIds.add(new Pair<AstClafer, Integer>(superId));
            sup = superId.getFst();
            curId = superId.getSnd().intValue();
        }
        return superIds;
    }

    public Pair<AstRef, Integer> getInheritedRefId(AstClafer clafer) {
        AstClafer sup = clafer;
        int curId = 0;
        do {
            if (sup.hasRef()) {
                return new Pair<AstRef, Integer>(sup.getRef(), curId);
            }
            if (sup.hasSuperClafer()) {
                curId += getOffsets(sup.getSuperClafer()).getOffset(sup);
            }
            sup = sup.getSuperClafer();
        } while (sup != null);
        return null;
    }

    public Offsets getOffsets(AstAbstractClafer clafer) {
        return notNull(clafer, "Offset", getOffsetMap().get(clafer));
    }

    public Map<AstAbstractClafer, Offsets> getOffsetMap() {
        return notNull("Offset", offsetMap);
    }

    public Analysis setOffsetMap(Map<AstAbstractClafer, Offsets> offsetMap) {
        this.offsetMap = offsetMap;
        return this;
    }

    public PartialSolution getPartialSolution(AstClafer clafer) {
        return notNull(clafer, "Partial solution", getPartialSolutionMap().get(clafer));
    }

    public Map<AstClafer, PartialSolution> getPartialSolutionMap() {
        return notNull("Partial solution", partialSolutionMap);
    }

    public Analysis setPartialSolutionMap(Map<AstClafer, PartialSolution> partialSolutionMap) {
        this.partialSolutionMap = partialSolutionMap;
        return this;
    }

    public int[][] getPartialInts(AstRef ref) {
        return notNull(ref.getSourceType(), "Partial integer", getPartialIntsMap().get(ref));
    }

    public Map<AstRef, int[][]> getPartialIntsMap() {
        return notNull("Partial integer", partialIntsMap);
    }

    public Analysis setPartialIntsMap(Map<AstRef, int[][]> partialIntsMap) {
        this.partialIntsMap = partialIntsMap;
        return this;
    }

    public boolean hasInteritedBreakableChildren(AstClafer clafer) {
        AstClafer sup = clafer;
        do {
            if (hasBreakableChildren(sup)) {
                return true;
            }
            sup = sup.getSuperClafer();
        } while (sup != null);
        return false;
    }

    public boolean hasBreakableChildren(AstClafer clafer) {
        return getBreakableChildren(clafer).length > 0;
    }

    public AstConcreteClafer[] getBreakableChildren(AstClafer clafer) {
        return notNull("Breakable children", getBreakableChildrenMap().get(clafer));
    }

    public Map<AstClafer, AstConcreteClafer[]> getBreakableChildrenMap() {
        return notNull("Breakable children", breakableChildrenMap);
    }

    public Analysis setBreakableChildrenMap(Map<AstClafer, AstConcreteClafer[]> breakableChildren) {
        this.breakableChildrenMap = breakableChildren;
        return this;
    }

    public boolean isBreakableRef(AstRef ref) {
        return getBreakableRefsMap().containsKey(ref);
    }

    public boolean isBreakableRefId(AstRef ref, int id) {
        int[] breakbleIDs = getBreakableRefsMap().get(ref);
        if (breakbleIDs == null) {
            return false;
        }
        return Util.in(id, breakbleIDs);
    }

    public Map<AstRef, int[]> getBreakableRefsMap() {
        return notNull("Breakable ref", breakableRefsMap);
    }

    public Analysis setBreakableRefsMap(Map<AstRef, int[]> breakableRefs) {
        this.breakableRefsMap = breakableRefs;
        return this;
    }

    public boolean isInheritedBreakableTarget(AstClafer clafer) {
        AstClafer sup = clafer;
        do {
            if (isBreakableTarget(sup)) {
                return true;
            }
            sup = sup.getSuperClafer();
        } while (sup != null);
        return false;
    }

    public boolean isBreakableTarget(AstClafer clafer) {
        return getBreakableTargetsMap().containsKey(clafer);
    }

    public AstRef[] getBreakableTarget(AstClafer clafer) {
        AstRef[] ref = getBreakableTargetsMap().get(clafer);
        return ref == null ? new AstRef[0] : ref;
    }

    public Map<AstClafer, AstRef[]> getBreakableTargetsMap() {
        return notNull("Breakable target", breakableTargetsMap);
    }

    public Analysis setBreakableTargetsMap(Map<AstClafer, AstRef[]> breakableTargetsMap) {
        this.breakableTargetsMap = breakableTargetsMap;
        return this;
    }

    public Type getType(AstExpr expr) {
        return notNull(expr.toString(), "Type", getTypeMap().get(expr));
    }
    
    public AstClafer getCommonSupertype(AstExpr expr) {
        return notNull(expr.toString(), "Type", getTypeMap().get(expr)).getCommonSuperType();
    }

    public Map<AstExpr, Type> getTypeMap() {
        return notNull("Type", typeMap);
    }

    public Analysis setTypeMap(Map<AstExpr, Type> typeMap) {
        this.typeMap = typeMap;
        return this;
    }
}
