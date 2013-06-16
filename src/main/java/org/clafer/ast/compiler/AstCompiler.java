package org.clafer.ast.compiler;

import gnu.trove.list.array.TIntArrayList;
import gnu.trove.map.TIntObjectMap;
import org.clafer.ast.AstUtil;
import org.clafer.ast.AstConstraint;
import java.util.Set;
import org.clafer.ast.AstSetTest;
import org.clafer.ast.AstExpr;
import org.clafer.ast.AstGlobal;
import org.clafer.ast.AstSetExpr;
import org.clafer.ast.AstCard;
import org.clafer.ast.AstCompare;
import org.clafer.ast.AstConstant;
import org.clafer.ast.AstJoin;
import org.clafer.ast.AstJoinParent;
import org.clafer.ast.AstJoinRef;
import org.clafer.ast.AstLocal;
import org.clafer.ast.AstQuantify;
import org.clafer.ast.AstThis;
import org.clafer.ast.AstUpcast;
import org.clafer.ir.IrExpr;
import org.clafer.ir.IrIntVar;
import org.clafer.ir.IrSetExpr;
import org.clafer.common.Util;
import org.clafer.ir.IrBoolVar;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.clafer.common.Check;
import org.clafer.scope.Scope;
import org.clafer.ast.analysis.Analysis;
import org.clafer.ast.analysis.Format;
import org.clafer.ast.analysis.PartialSolution;
import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstArithm;
import org.clafer.ast.AstBoolArithm;
import org.clafer.ast.AstBoolExpr;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstDecl;
import org.clafer.ast.AstException;
import org.clafer.ast.AstExprVisitor;
import org.clafer.ast.AstIfThenElse;
import org.clafer.ast.AstIntClafer;
import org.clafer.ast.AstMembership;
import org.clafer.ast.AstMinus;
import org.clafer.ast.AstModel;
import org.clafer.ast.AstNot;
import org.clafer.ast.AstQuantify.Quantifier;
import org.clafer.ast.AstRef;
import org.clafer.ast.AstSetArithm;
import org.clafer.ast.AstTernary;
import org.clafer.ast.Card;
import org.clafer.ast.analysis.AbstractOffsetAnalyzer;
import org.clafer.ast.analysis.Analyzer;
import org.clafer.ast.analysis.CardAnalyzer;
import org.clafer.ast.analysis.FormatAnalyzer;
import org.clafer.ast.analysis.GlobalCardAnalyzer;
import org.clafer.ast.analysis.OptimizerAnalyzer;
import org.clafer.ast.analysis.PartialIntAnalyzer;
import org.clafer.ast.analysis.PartialSolutionAnalyzer;
import org.clafer.ast.analysis.ScopeAnalyzer;
import org.clafer.ast.analysis.SymmetryAnalyzer;
import org.clafer.ast.analysis.TypeAnalyzer;
import org.clafer.ast.analysis.TypeHierarchyDepthAnalyzer;
import org.clafer.collection.Pair;
import org.clafer.collection.Triple;
import org.clafer.graph.KeyGraph;
import org.clafer.graph.Vertex;
import org.clafer.graph.GraphUtil;
import org.clafer.ir.IrBoolExpr;
import org.clafer.ir.IrDomain;
import org.clafer.ir.IrIntExpr;
import org.clafer.ir.IrModule;
import org.clafer.ir.IrSetVar;
import org.clafer.ir.IrUtil;
import static org.clafer.ir.Irs.*;

/**
 * Compile from AST to IR.
 *
 * @author jimmy
 */
public class AstCompiler {

    public static final Analyzer[] DefaultAnalyzers = new Analyzer[]{
        new TypeAnalyzer(),
        new TypeHierarchyDepthAnalyzer(),
        new GlobalCardAnalyzer(),
        new ScopeAnalyzer(),
        new CardAnalyzer(),
        new FormatAnalyzer(),
        new AbstractOffsetAnalyzer(),
        new OptimizerAnalyzer(),
        new PartialSolutionAnalyzer(),
        new PartialIntAnalyzer(),
        new SymmetryAnalyzer(),
        // Reanalyze types
        new TypeAnalyzer()
    };
    private final Analysis analysis;
    private final IrModule module;

    private AstCompiler(AstModel model, Scope scope, IrModule module, Analyzer[] analyzers) {
        this.analysis = Analysis.analyze(model, scope, analyzers);
        this.module = Check.notNull(module);
    }

    public static AstSolutionMap compile(AstModel in, Scope scope, IrModule out) {
        return compile(in, scope, out, DefaultAnalyzers);
    }

    public static AstSolutionMap compile(AstModel in, Scope scope, IrModule out, Analyzer[] analyzers) {
        AstCompiler compiler = new AstCompiler(in, scope, out, analyzers);
        return compiler.compile();
    }

    /**
     * @return the order to initialize regular variables
     */
    private List<AstClafer> initOrder() {
        List<AstAbstractClafer> abstractClafers = analysis.getAbstractClafers();
        List<AstConcreteClafer> concreteClafers = analysis.getConcreteClafers();

        KeyGraph<AstClafer> dependency = new KeyGraph<AstClafer>();
        for (AstAbstractClafer abstractClafer : abstractClafers) {
            Vertex<AstClafer> node = dependency.getVertex(abstractClafer);
            for (AstClafer sub : abstractClafer.getSubs()) {
                node.addNeighbour(dependency.getVertex(sub));
            }
        }
        for (AstConcreteClafer concreteClafer : concreteClafers) {
            Vertex<AstClafer> node = dependency.getVertex(concreteClafer);
            if (Format.ParentGroup.equals(getFormat(concreteClafer))) {
                node.addNeighbour(dependency.getVertex(concreteClafer.getParent()));
            }
        }
        List<Set<AstClafer>> components = GraphUtil.computeStronglyConnectedComponents(dependency);
        List<AstClafer> clafers = new ArrayList<AstClafer>();
        for (Set<AstClafer> component : components) {
            if (component.size() != 1) {
                throw new AstException("Cannot satisfy the cycle " + component);
            }
            clafers.addAll(component);
        }
        return clafers;
    }

    /**
     * @return the order to initialize weight variables for symmetry breaking
     */
    private List<AstClafer> initWeightOrder() {
        List<AstAbstractClafer> abstractClafers = analysis.getAbstractClafers();
        List<AstConcreteClafer> concreteClafers = analysis.getConcreteClafers();

        KeyGraph<AstClafer> dependency = new KeyGraph<AstClafer>();
        for (AstAbstractClafer abstractClafer : abstractClafers) {
            Vertex<AstClafer> node = dependency.getVertex(abstractClafer);
            for (AstClafer sub : abstractClafer.getSubs()) {
                node.addNeighbour(dependency.getVertex(sub));
            }
        }
        for (AstConcreteClafer concreteClafer : concreteClafers) {
            if (concreteClafer.hasParent()) {
                dependency.getVertex(concreteClafer.getParent()).addNeighbour(
                        dependency.getVertex(concreteClafer));
            }
        }
        List<Set<AstClafer>> components = GraphUtil.computeStronglyConnectedComponents(dependency);
        List<AstClafer> clafers = new ArrayList<AstClafer>();
        for (Set<AstClafer> component : components) {
            if (component.size() != 1) {
                throw new AstException("Cannot satisfy the cycle " + component);
            }
            clafers.addAll(component);
        }
        return clafers;
    }

    private AstSolutionMap compile() {
        IrSetVar rootSet = constant(new int[]{0});
        sets.put(analysis.getModel(), rootSet);
        siblingSets.put(analysis.getModel(), new IrSetVar[]{rootSet});
        memberships.put(analysis.getModel(), new IrBoolExpr[]{$(True)});

        List<AstClafer> clafers = initOrder();
        for (AstClafer clafer : clafers) {
            if (clafer instanceof AstConcreteClafer && !AstUtil.isRoot((AstConcreteClafer) clafer)) {
                initConcrete((AstConcreteClafer) clafer);
            } else if (clafer instanceof AstAbstractClafer) {
                initAbstract((AstAbstractClafer) clafer);
            }
        }
        for (AstClafer clafer : initWeightOrder()) {
            if (clafer instanceof AstConcreteClafer && !AstUtil.isRoot((AstConcreteClafer) clafer)) {
                initConcreteWeight((AstConcreteClafer) clafer);
            } else if (clafer instanceof AstAbstractClafer) {
                initAbstractWeight((AstAbstractClafer) clafer);
            }
        }
        for (AstClafer clafer : clafers) {
            if (clafer instanceof AstConcreteClafer && !AstUtil.isRoot((AstConcreteClafer) clafer)) {
                constrainConcrete((AstConcreteClafer) clafer);
            } else if (clafer instanceof AstAbstractClafer) {
                constrainAbstract((AstAbstractClafer) clafer);
            }
            constrainGroupCardinality(clafer);
        }

        // Map the identifiers to the ORIGINAL constraints.
        TIntObjectMap<AstConstraint> constraintMap = AstUtil.getConstraintMap(analysis.getModel());
        List<Pair<AstConstraint, IrBoolVar>> softVars = new ArrayList<Pair<AstConstraint, IrBoolVar>>();
        for (AstConstraint constraint : getConstraints()) {
            AstClafer clafer = constraint.getContext();
            int scope = getScope(clafer);
            if (constraint.isHard()) {
                for (int j = 0; j < scope; j++) {
                    ExpressionCompiler expressionCompiler = new ExpressionCompiler(j);
                    IrBoolExpr thisConstraint = expressionCompiler.compile(constraint.getExpr());
                    module.addConstraint(implies(memberships.get(clafer)[j], thisConstraint));
                }
            } else {
                IrBoolVar soft = bool(constraint.toString());
                AstConstraint originalConstraint = constraintMap.get(constraint.getId());
                softVars.add(new Pair<AstConstraint, IrBoolVar>(originalConstraint, soft));
                for (int j = 0; j < scope; j++) {
                    ExpressionCompiler expressionCompiler = new ExpressionCompiler(j);
                    IrBoolExpr thisConstraint = expressionCompiler.compile(constraint.getExpr());
                    module.addConstraint(ifOnlyIf($(soft), implies(memberships.get(clafer)[j], thisConstraint)));
                }
                module.addBoolVar(soft);
            }
        }
        for (IrSetVar[] childSet : siblingSets.values()) {
            module.addSetVars(childSet);
        }
        for (IrIntVar[] refs : refPointers.values()) {
            module.addIntVars(refs);
        }
        @SuppressWarnings("unchecked")
        Pair<AstConstraint, IrBoolVar>[] softVarPairs = softVars.toArray(new Pair[softVars.size()]);
        return new AstSolutionMap(analysis.getModel(), siblingSets, refPointers, softVarPairs, analysis);
    }

    private void initConcrete(AstConcreteClafer clafer) {
        parentPointers.put(clafer, buildParentPointers(clafer));
        if (clafer.hasRef()) {
            refPointers.put(clafer.getRef(), buildRefPointers(clafer.getRef()));
        }
        switch (getFormat(clafer)) {
            case LowGroup:
                initLowGroupConcrete(clafer);
                break;
            case ParentGroup:
                initParentGroupConcrete(clafer);
                break;
            default:
                throw new AstException();
        }

        IrSetVar[] siblingSet = siblingSets.get(clafer);
        switch (siblingSet.length) {
            case 0:
                sets.put(clafer, EmptySet);
                break;
            case 1:
                sets.put(clafer, siblingSet[0]);
                break;
            default:
                IrSetExpr union = union($(siblingSet));
                IrSetVar set = set(clafer.getName(), union.getEnv(), union.getKer(), union.getCard());
                module.addConstraint(equal($(set), union));
                sets.put(clafer, set);
                break;
        }
    }

    private void initConcreteWeight(AstConcreteClafer clafer) {
        int scope = getScope(clafer);
        int parentScope = getScope(clafer.getParent());
        IrIntExpr[] weight;
        IrIntExpr[][] index;
        AstRef ref = AstUtil.getInheritedRef(clafer);
        // If the Clafer either needs children or reference to be introduce symmetry.
        if (analysis.hasInteritedBreakableChildren(clafer)
                || (ref != null && analysis.isBreakableRef(ref))
                || analysis.isInheritedBreakableTarget(clafer)) {
            weight = new IrIntExpr[scope];
            for (int i = 0; i < weight.length; i++) {
                weight[i] = $(boundInt(clafer.getName() + "#" + i + "@Weight", 0, scope - 1).asNoDecision());
            }
            index = new IrIntExpr[parentScope][getCard(clafer).getHigh()];
            for (int i = 0; i < index.length; i++) {
                for (int j = 0; j < index[i].length; j++) {
                    index[i][j] =
                            $(boundInt(clafer.getName() + "@Index#" + i + "#" + j, -1, scope).asNoDecision());
                }
            }
        } else {
            // Optimize for nonsymmetric nodes. Don't compute the smallest indices, 
            // just use the cardinalities.
            weight = Util.replicate($(Zero), scope);
            IrSetVar[] childSet = siblingSets.get(clafer);
            index = new IrIntExpr[childSet.length][];
            for (int i = 0; i < index.length; i++) {
                index[i] = new IrIntExpr[]{card($(childSet[i]))};
            }
        }
        weights.put(clafer, weight);
        indices.put(clafer, index);
    }

    private void constrainConcrete(AstConcreteClafer clafer) {
        IrSetExpr[] siblingSet = $(siblingSets.get(clafer));
        IrIntExpr[] parents = $(parentPointers.get(clafer));
        if (!getPartialSolution(clafer).parentSolutionKnown()) {
            if (getGlobalCard(clafer).isExact()) {
                // No unused
                module.addConstraint(intChannel(parents, siblingSet));
            } else {
                IrSetVar unused = set(clafer.getName() + "@Unused", getPartialSolution(clafer).getUnknownClafers()).asNoDecision();
                module.addConstraint(intChannel(parents, Util.snoc(siblingSet, $(unused))));
            }
        }

        Pair<AstRef, Integer> refPair = analysis.getInheritedRefId(clafer, 0);
        AstRef ref = refPair == null ? null : refPair.getFst();
        int refOffset = refPair == null ? 0 : refPair.getSnd().intValue();

        IrBoolExpr[] members = memberships.get(clafer);
        if (ref != null) {
            IrIntVar[] refs = Arrays.copyOfRange(refPointers.get(ref),
                    refOffset, refOffset + getScope(clafer));
            if (ref.isUnique() && getCard(clafer).getHigh() > 1) {
                if (getGlobalCard(clafer).isExact()) {
                    assert getGlobalCard(clafer).getLow() == refs.length;
                    module.addConstraint(allDifferent($(refs)));
                } else {
                    for (int i = 0; i < refs.length; i++) {
                        for (int j = i + 1; j < refs.length; j++) {
                            module.addConstraint(
                                    implies(and(members[i], members[j],
                                    equal(parents[i], parents[j])),
                                    notEqual($(refs[i]), $(refs[j]))));
                        }
                    }
                }
                IrIntExpr size =
                        ref.getTargetType() instanceof AstIntClafer
                        ? $(constant(analysis.getScope().getIntHigh() - analysis.getScope().getIntLow() + 1))
                        : card($(sets.get(ref.getTargetType())));
                for(IrSetExpr sibling : siblingSet) {
                    module.addConstraint(lessThanEqual(card(sibling), size));
                }
            }
            assert refs.length == members.length;
            for (int i = 0; i < members.length; i++) {
                module.addConstraint(implies(not(members[i]), equal($(refs[i]), 0)));
            }

            if (!(ref.getTargetType() instanceof AstIntClafer)) {
                IrSetVar targetSet = sets.get(ref.getTargetType());
                for (int i = 0; i < refs.length; i++) {
                    IrIntVar refPointer = refs[i];
                    if (targetSet.getKer().contains(0)) {
                        module.addConstraint(member($(refPointer), $(targetSet)));
                    } else {
                        module.addConstraint(implies(members[i], member($(refPointer), $(targetSet))));
                    }
                }
            }
        }

        // If the Clafer either needs children or reference to be introduce symmetry.
        if (analysis.hasInteritedBreakableChildren(clafer)
                || (ref != null && analysis.isBreakableRef(ref))
                || analysis.isInheritedBreakableTarget(clafer)) {
            IrIntExpr[] weight = weights.get(clafer);
            IrIntExpr[][] index = indices.get(clafer);

            analysis.getHierarcyIds(clafer, refOffset);
            IrIntExpr[][] childIndices = new IrIntExpr[weight.length][];

            List<Pair<AstClafer, Integer>> offsets = analysis.getHierarcyOffsets(clafer);
            for (int i = 0; i < childIndices.length; i++) {
                List<IrIntExpr[]> childIndex = new ArrayList<IrIntExpr[]>();
                for (Pair<AstClafer, Integer> offset : offsets) {
                    for (AstConcreteClafer child : analysis.getBreakableChildren(offset.getFst())) {
                        childIndex.add(indices.get(child)[i + offset.getSnd()]);
                    }
                }
                if (ref != null && analysis.isBreakableRef(ref)) {
                    int refHigh = getScopeHigh(ref.getTargetType());
                    // References need a positive weight, so to use their value as
                    // a weight, need to offset it so that it always positive.
                    childIndex.add(new IrIntExpr[]{
                        analysis.isBreakableRefId(ref, i + refOffset)
                        // The id of the target is the weight.
                        ? sub(mul(asInt(members[i]), refHigh + 1), $(refPointers.get(ref)[i + refOffset]))
                        // If analysis says that this id does not need breaking
                        // then give it a constant weight. Any constant is fine.
                        : $(Zero)
                    });
                }
                if (analysis.isInheritedBreakableTarget(clafer)) {
                    for (Pair<AstClafer, Integer> hierarchy : analysis.getHierarcyIds(clafer, i)) {
                        for (AstRef sourceRef : analysis.getBreakableTarget(hierarchy.getFst())) {
                            IrIntVar[] sourceRefs = refPointers.get(sourceRef);
                            IrBoolExpr[] sourceMembers = memberships.get(sourceRef.getSourceType());

                            IrIntExpr[] array = new IrIntExpr[sourceRefs.length];
                            for (int j = 0; j < array.length; j++) {
                                array[j] = add($(sourceRefs[j]), asInt(sourceMembers[j]));
                            }
                            childIndex.add(new IrIntExpr[]{
                                count(hierarchy.getSnd().intValue() + 1, array)
                            });
                        }
                    }
                }
                childIndices[i] = Util.concat(childIndex.toArray(new IrIntExpr[childIndex.size()][]));
            }
            module.addConstraint(sortChannel(childIndices, weight));
            for (int i = 0; i < siblingSet.length; i++) {
                module.addConstraint(filterString(siblingSet[i], weight, index[i]));
            }
            for (int i = 0; i < parents.length - 1; i++) {
                module.addConstraint(implies(equal(parents[i], parents[i + 1]),
                        greaterThanEqual(weight[i], weight[i + 1])));
            }
        }

        switch (getFormat(clafer)) {
            case LowGroup:
                constrainLowGroupConcrete(clafer);
                break;
            case ParentGroup:
                constrainParentGroupConcrete(clafer);
                break;
            default:
                throw new AstException();
        }
    }

    private void constrainGroupCardinality(AstClafer clafer) {
        Card groupCard = clafer.getGroupCard();
        List<AstConcreteClafer> children = clafer.getChildren();
        if (groupCard.isBounded()) {
            IrBoolExpr[] members = memberships.get(clafer);
            IrSetVar[][] childrenSets = new IrSetVar[children.size()][];
            boolean featureGroup = true;
            for (int i = 0; i < childrenSets.length; i++) {
                AstConcreteClafer child = children.get(i);
                childrenSets[i] = siblingSets.get(child);
                featureGroup &= getCard(child).getHigh() == 1;
            }
            int scope = getScope(clafer);
            for (int i = 0; i < scope; i++) {
                IrIntExpr[] cards = new IrIntExpr[childrenSets.length];
                for (int j = 0; j < cards.length; j++) {
                    cards[j] = card($(childrenSets[j][i]));
                }
                if (featureGroup) {
                    // Translate common cases more efficiently.
                    if (groupCard.getLow() == 0 && groupCard.getHigh() == 1) {
                        // "lone"
                        module.addConstraint(implies(members[i], lone(asBools(cards))));
                        continue;
                    }
                    if (groupCard.getLow() == 1 && !groupCard.hasHigh()) {
                        // "or"
                        module.addConstraint(implies(members[i], or(asBools(cards))));
                        continue;
                    }
                    if (groupCard.isExact() && groupCard.getLow() == 1) {
                        // "xor"
                        // TODO constrain as sum(cards) = card(this)
                        module.addConstraint(implies(members[i], one(asBools(cards))));
                        continue;
                    }
                }
                module.addConstraint(implies(members[i], constrainCard(add(cards), groupCard)));
            }
        }
    }

    private void initLowGroupConcrete(AstConcreteClafer clafer) {
        PartialSolution partialSolution = getPartialSolution(clafer);

        IrSetVar[] childSet = buildChildSet(clafer);
        siblingSets.put(clafer, childSet);

        IrBoolExpr[] members = new IrBoolExpr[getScope(clafer)];
        for (int i = 0; i < members.length; i++) {
            if (partialSolution.hasClafer(i)) {
                members[i] = $(True);
            } else {
                members[i] =
                        childSet.length == 1 && members.length == 1
                        ? asBool(card($(childSet[0])))
                        : $(bool(clafer.getName() + "@Membership#" + i));
            }
        }
        memberships.put(clafer, members);
    }

    private void constrainLowGroupConcrete(AstConcreteClafer clafer) {
        IrBoolExpr[] members = memberships.get(clafer);
        IrSetVar set = sets.get(clafer);

        module.addConstraint(selectN(members, card($(set))));

        IrBoolExpr[] parentMembership = memberships.get(clafer.getParent());
        Card card = getCard(clafer);

        IrSetVar[] childSet = siblingSets.get(clafer);
        for (int i = 0; i < parentMembership.length; i++) {
            IrBoolExpr parentMember = parentMembership[i];
            if (card.isBounded()) {
                // Enforce cardinality.
                module.addConstraint(implies(parentMember, constrainCard(card($(childSet[i])), card)));
            }
            module.addConstraint(implies(not(parentMember), equal($(childSet[i]), $(EmptySet))));
        }


        if (!(childSet.length == 1 && members.length == 1)) {
            module.addConstraint(boolChannel(members, $(set)));
        }

        /**
         * What is this optimization?
         *
         * Force the lower number atoms to choose lower number parents. For
         * example consider the following Clafer model:
         *
         * <pre>
         * Person 2
         *     Hand 2
         * </pre>
         *
         * The constraint forbids the case where Hand0 belongs to Person1 and
         * Hand1 belongs to Person0. Otherwise, the children can swap around
         * creating many isomorphic solutions.
         */
        module.addConstraint(sort($(parentPointers.get(clafer))));
    }

    private void initParentGroupConcrete(AstConcreteClafer clafer) {
        PartialSolution partialParentSolution = getPartialParentSolution(clafer);

        IrSetVar[] children = new IrSetVar[partialParentSolution.size()];
        assert getCard(clafer).getLow() == getCard(clafer).getHigh();
        int lowCard = getCard(clafer).getLow();
        for (int i = 0; i < children.length; i++) {
            if (partialParentSolution.hasClafer(i)) {
                children[i] = constant(Util.fromTo(i * lowCard, i * lowCard + lowCard));
            } else {
                children[i] = set(clafer.getName() + "#" + i, Util.fromTo(i * lowCard, i * lowCard + lowCard));
            }
        }

        siblingSets.put(clafer, children);

        IrBoolExpr[] members = new IrBoolExpr[getScope(clafer)];
        IrBoolExpr[] parentMembership = memberships.get(clafer.getParent());
        if (lowCard == 1) {
            if (members.length == parentMembership.length) {
                members = parentMembership;
            } else {
                System.arraycopy(parentMembership, 0, members, 0, parentMembership.length);
                Arrays.fill(members, parentMembership.length, members.length, $(False));
            }
        } else {
            for (int i = 0; i < parentMembership.length; i++) {
                for (int j = 0; j < lowCard; j++) {
                    members[i * lowCard + j] = parentMembership[i];
                }
            }
        }
        memberships.put(clafer, members);
    }

    private void constrainParentGroupConcrete(AstConcreteClafer clafer) {
        PartialSolution partialParentSolution = getPartialParentSolution(clafer);

        IrSetVar[] children = siblingSets.get(clafer);
        assert getCard(clafer).getLow() == getCard(clafer).getHigh();
        int lowCard = getCard(clafer).getLow();
        for (int i = 0; i < children.length; i++) {
            if (!partialParentSolution.hasClafer(i)) {
                module.addConstraint(implies(memberships.get(clafer.getParent())[i],
                        equal($(children[i]), $(constant(Util.fromTo(i * lowCard, i * lowCard + lowCard))))));
                module.addConstraint(implies(not(memberships.get(clafer.getParent())[i]),
                        equal($(children[i]), $(EmptySet))));
            }
        }
    }

    private void initAbstract(AstAbstractClafer clafer) {
        IrSetVar[] subSets = new IrSetVar[clafer.getSubs().size()];
        IrBoolExpr[] members = new IrBoolExpr[getScope(clafer)];
        for (int i = 0; i < subSets.length; i++) {
            AstClafer sub = clafer.getSubs().get(i);
            subSets[i] = sets.get(sub);
            IrBoolExpr[] subMembers = memberships.get(sub);
            int offset = getOffset(clafer, sub);
            for (int j = 0; j < subMembers.length; j++) {
                assert members[offset + j] == null;
                members[offset + j] = Check.notNull(subMembers[j]);
            }
        }
        if (subSets.length == 1) {
            sets.put(clafer, sets.get(clafer.getSubs().get(0)));
        } else {
            TIntArrayList env = new TIntArrayList();
            TIntArrayList ker = new TIntArrayList();
            for (int i = 0; i < members.length; i++) {
                if (IrUtil.isTrue(members[i])) {
                    ker.add(i);
                }
                if (!IrUtil.isFalse(members[i])) {
                    env.add(i);
                }
            }
            IrSetVar unionSet = set(clafer.getName(), env.toArray(), ker.toArray());
            if (!AstUtil.isTypeRoot(clafer)) {
                module.addConstraint(boolChannel(members, $(unionSet)));
            }
            sets.put(clafer, unionSet);
        }
        Check.noNulls(members);
        memberships.put(clafer, members);

        if (clafer.hasRef()) {
            refPointers.put(clafer.getRef(), buildRefPointers(clafer.getRef()));
        }
    }

    private void initAbstractWeight(AstAbstractClafer clafer) {
//        int scope = getScope(clafer);
//        for (int i = 0; i < scope; i++) {
//            Pair<AstClafer, Integer> subId = analysis.getSubId(clafer, i);
//        }
    }

    private void constrainAbstract(AstAbstractClafer clafer) {
        // Do nothing.
    }
    private final Map<AstClafer, IrSetVar> sets = new HashMap<AstClafer, IrSetVar>();
    private final Map<AstClafer, IrSetVar[]> siblingSets = new HashMap<AstClafer, IrSetVar[]>();
    private final Map<AstClafer, IrBoolExpr[]> memberships = new HashMap<AstClafer, IrBoolExpr[]>();
    private final Map<AstConcreteClafer, IrIntVar[]> parentPointers = new HashMap<AstConcreteClafer, IrIntVar[]>();
    private final Map<AstRef, IrIntVar[]> refPointers = new HashMap<AstRef, IrIntVar[]>();
    private final Map<AstClafer, IrIntExpr[]> weights = new HashMap<AstClafer, IrIntExpr[]>();
    private final Map<AstClafer, IrIntExpr[][]> indices = new HashMap<AstClafer, IrIntExpr[][]>();

    private class ExpressionCompiler implements AstExprVisitor<Void, IrExpr> {

        private final int thisId;
        private final Map<AstLocal, IrIntExpr> locals = new HashMap<AstLocal, IrIntExpr>();

        private ExpressionCompiler(int thisId) {
            this.thisId = thisId;
        }

        private IrExpr compile(AstExpr expr) {
            return expr.accept(this, null);
        }

        private IrExpr[] compile(AstExpr[] exprs) {
            IrExpr[] compiled = new IrExpr[exprs.length];
            for (int i = 0; i < compiled.length; i++) {
                compiled[i] = compile(exprs[i]);
            }
            return compiled;
        }

        private IrBoolExpr compile(AstBoolExpr expr) {
            return (IrBoolExpr) compile((AstExpr) expr);
        }

        private IrBoolExpr[] compile(AstBoolExpr[] exprs) {
            IrBoolExpr[] compiled = new IrBoolExpr[exprs.length];
            for (int i = 0; i < compiled.length; i++) {
                compiled[i] = compile(exprs[i]);
            }
            return compiled;
        }

        private IrIntExpr asInt(IrExpr expr) {
            if (expr instanceof IrIntExpr) {
                return (IrIntExpr) expr;
            }
            if (expr instanceof IrSetExpr) {
                return sum((IrSetExpr) expr);
            }
            // Bug.
            throw new AstException("Should not have passed type checking.");
        }

        private IrIntExpr[] asInts(IrExpr[] exprs) {
            IrIntExpr[] ints = new IrIntExpr[exprs.length];
            for (int i = 0; i < ints.length; i++) {
                ints[i] = asInt(exprs[i]);
            }
            return ints;
        }

        private IrSetExpr asSet(IrExpr expr) {
            if (expr instanceof IrIntExpr) {
                return singleton((IrIntExpr) expr);
            }
            if (expr instanceof IrSetExpr) {
                return (IrSetExpr) expr;
            }
            // Bug.
            throw new AstException("Should not have passed type checking.");
        }

        private IrSetExpr[] asSets(IrExpr[] exprs) {
            IrSetExpr[] sets = new IrSetExpr[exprs.length];
            for (int i = 0; i < sets.length; i++) {
                sets[i] = asSet(exprs[i]);
            }
            return sets;
        }

        @Override
        public IrExpr visit(AstThis ast, Void a) {
            return $(constant(thisId));
        }

        @Override
        public IrExpr visit(AstGlobal ast, Void a) {
            IrSetVar global = sets.get(ast.getType());
            if (global.getEnv().size() == 1) {
                int[] constant = IrUtil.getConstant(global);
                if (constant != null) {
                    return $(constant(constant[0]));
                }
            }
            return $(global);
        }

        @Override
        public IrExpr visit(AstConstant ast, Void a) {
            int[] value = ast.getValue();
            if (value.length == 1) {
                return $(constant(value[0]));
            }
            return $(constant(value));
        }

        @Override
        public IrExpr visit(AstJoin ast, Void a) {
            AstSetExpr left = ast.getLeft();
            AstConcreteClafer right = ast.getRight();

            IrExpr $left = compile(left);
            if ($left instanceof IrIntExpr) {
                IrIntExpr $intLeft = (IrIntExpr) $left;
                if (Format.ParentGroup.equals(getFormat(right)) && getCard(right).getLow() == 1) {
                    assert getCard(right).isExact();
                    return $intLeft;
                }
                // Why empty set? The "take" var can contain unused.
                return joinRelation(singleton($intLeft), $(Util.snoc(siblingSets.get(right), EmptySet)));
            } else if ($left instanceof IrSetExpr) {
                IrSetExpr $setLeft = (IrSetExpr) $left;
                // Why empty set? The "take" var can contain unused.
                return joinRelation($setLeft, $(Util.snoc(siblingSets.get(right), EmptySet)));
            }
            throw new AstException();
        }

        @Override
        public IrExpr visit(AstJoinParent ast, Void a) {
            AstSetExpr children = ast.getChildren();
            AstConcreteClafer childrenType = (AstConcreteClafer) getType(children);

            IrExpr $children = compile(children);
            if ($children instanceof IrIntExpr) {
                // Only one possible parent.
                if (getScope(childrenType.getParent()) == 1) {
//                    return $(Zero);
                    throw new Error();
                }

                IrIntExpr $intChildren = (IrIntExpr) $children;
                switch (getFormat(childrenType)) {
                    case ParentGroup:
                        assert getCard(childrenType).isExact();
                        int lowCard = getCard(childrenType).getLow();
                        return div($intChildren, $(constant(lowCard)));
                    case LowGroup:
                        return element($(parentPointers.get(childrenType)), $intChildren);
                }
            } else if ($children instanceof IrSetExpr) {
                IrSetExpr $setChildren = (IrSetExpr) $children;
                return joinFunction($setChildren, $(parentPointers.get(childrenType)));
            }
            throw new AstException();
        }

        @Override
        public IrExpr visit(AstJoinRef ast, Void a) {
            AstSetExpr deref = ast.getDeref();
            AstClafer derefType = getType(deref);

            IrExpr $deref = compile(deref);
            if ($deref instanceof IrIntExpr) {
                IrIntExpr $intDeref = (IrIntExpr) $deref;
                return element($(refPointers.get(derefType.getRef())), $intDeref);
            } else if ($deref instanceof IrSetExpr) {
                IrSetExpr $setDeref = (IrSetExpr) $deref;
                return joinFunction($setDeref, $(refPointers.get(derefType.getRef())));
            }
            throw new AstException();
        }

        @Override
        public IrExpr visit(AstCard ast, Void a) {
            IrExpr set = compile(ast.getSet());
            if (set instanceof IrIntExpr) {
                return $(One);
            }
            return card((IrSetExpr) set);
        }

        @Override
        public IrExpr visit(AstNot ast, Void a) {
            return not(compile(ast.getExpr()));
        }

        @Override
        public IrExpr visit(AstMinus ast, Void a) {
            return minus(asInt(compile(ast.getExpr())));
        }

        @Override
        public IrExpr visit(AstSetTest ast, Void a) {
            IrExpr left = compile(ast.getLeft());
            IrExpr right = compile(ast.getRight());

            if (left instanceof IrIntExpr && right instanceof IrIntExpr) {
                IrIntExpr $intLeft = (IrIntExpr) left;
                IrIntExpr $intRight = (IrIntExpr) right;

                switch (ast.getOp()) {
                    case Equal:
                        return equal($intLeft, $intRight);
                    case NotEqual:
                        return notEqual($intLeft, $intRight);
                }
            }

            switch (ast.getOp()) {
                case Equal:
                    return equal(asSet(left), asSet(right));
                case NotEqual:
                    return notEqual(asSet(left), asSet(right));
                default:
                    throw new AstException();
            }
        }

        @Override
        public IrExpr visit(AstCompare ast, Void a) {
            IrIntExpr left = asInt(compile(ast.getLeft()));
            IrIntExpr right = asInt(compile(ast.getRight()));
            switch (ast.getOp()) {
                case LessThan:
                    return lessThan(left, right);
                case LessThanEqual:
                    return lessThanEqual(left, right);
                case GreaterThan:
                    return greaterThan(left, right);
                case GreaterThanEqual:
                    return greaterThanEqual(left, right);
                default:
                    throw new AstException();
            }
        }

        @Override
        public IrExpr visit(AstArithm ast, Void a) {
            IrIntExpr[] operands = asInts(compile(ast.getOperands()));
            switch (ast.getOp()) {
                case Add:
                    return add(operands);
                case Sub:
                    return sub(operands);
                case Mul:
                    IrIntExpr product = operands[0];
                    for (int i = 1; i < operands.length; i++) {
                        product = mul(product, operands[i]);
                    }
                    return product;
                case Div:
                    IrIntExpr quotient = operands[0];
                    for (int i = 1; i < operands.length; i++) {
                        quotient = div(quotient, operands[i]);
                    }
                    return quotient;
                default:
                    throw new AstException();
            }
        }

        @Override
        public IrExpr visit(AstBoolArithm ast, Void a) {
            IrBoolExpr[] operands = compile(ast.getOperands());
            switch (ast.getOp()) {
                case And:
                    return and(operands);
                case IfOnlyIf:
                    IrBoolExpr ifOnlyIf = operands[0];
                    for (int i = 1; i < operands.length; i++) {
                        ifOnlyIf = ifOnlyIf(ifOnlyIf, operands[i]);
                    }
                    return ifOnlyIf;
                case Implies:
                    IrBoolExpr implies = operands[0];
                    for (int i = 1; i < operands.length; i++) {
                        implies = implies(implies, operands[i]);
                    }
                    return implies;
                case Or:
                    return or(operands);
                case Xor:
                    IrBoolExpr xor = operands[0];
                    for (int i = 1; i < operands.length; i++) {
                        xor = xor(xor, operands[i]);
                    }
                    return xor;
                default:
                    throw new AstException();
            }
        }

        @Override
        public IrExpr visit(AstSetArithm ast, Void a) {
            IrSetExpr[] operands = asSets(compile(ast.getOperands()));
            switch (ast.getOp()) {
                case Union:
                    return union(operands);
                case Difference:
                    IrSetExpr difference = operands[0];
                    for (int i = 1; i < operands.length; i++) {
                        difference = difference(difference, operands[i]);
                    }
                    return difference;
                case Intersection:
                    return intersection(operands);
                default:
                    throw new AstException();
            }
        }

        @Override
        public IrExpr visit(AstMembership ast, Void a) {
            IrExpr member = compile(ast.getMember());
            IrExpr set = compile(ast.getSet());
            if (member instanceof IrIntExpr && set instanceof IrIntExpr) {
                return AstMembership.Op.In.equals(ast.getOp())
                        ? equal((IrIntExpr) member, (IrIntExpr) set)
                        : notEqual((IrIntExpr) member, (IrIntExpr) set);
            }
            if (member instanceof IrIntExpr && set instanceof IrSetExpr) {
                return AstMembership.Op.In.equals(ast.getOp())
                        ? member((IrIntExpr) member, (IrSetExpr) set)
                        : notMember((IrIntExpr) member, (IrSetExpr) set);
            }
            if (member instanceof IrSetExpr && set instanceof IrIntExpr) {
                return AstMembership.Op.In.equals(ast.getOp())
                        ? equal((IrSetExpr) member, singleton((IrIntExpr) set))
                        : notEqual((IrSetExpr) member, singleton((IrIntExpr) set));
            }
            return AstMembership.Op.In.equals(ast.getOp())
                    ? subsetEq(asSet(member), asSet(set))
                    : not(subsetEq(asSet(member), asSet(set)));
        }

        @Override
        public IrExpr visit(AstTernary ast, Void a) {
            IrBoolExpr antecedent = compile(ast.getAntecedent());
            IrExpr consequent = compile(ast.getConsequent());
            IrExpr alternative = compile(ast.getAlternative());
            if (consequent instanceof IrIntExpr && alternative instanceof IrIntExpr) {
                return ternary(antecedent, (IrIntExpr) consequent, (IrIntExpr) alternative);
            }
            return ternary(antecedent, asSet(consequent), asSet(alternative));
        }

        @Override
        public IrExpr visit(AstIfThenElse ast, Void a) {
            return ifThenElse(compile(ast.getAntecedent()), compile(ast.getConsequent()), compile(ast.getAlternative()));
        }

        @Override
        public IrExpr visit(AstUpcast ast, Void a) {
            AstSetExpr base = ast.getBase();
            int offset = getOffset(ast.getTarget(), getType(base));

            IrExpr $base = compile(ast.getBase());
            if ($base instanceof IrIntExpr) {
                IrIntExpr intBase = (IrIntExpr) $base;
                return add(intBase, $(constant(offset)));
            }
            return offset((IrSetExpr) $base, offset);
        }

        @Override
        public IrExpr visit(AstLocal ast, Void a) {
            return locals.get(ast);
        }

        private Triple<AstLocal, IrIntExpr, IrBoolExpr>[][] compileDecl(AstDecl decl) {
            IrExpr body = compile(decl.getBody());
            if (body instanceof IrIntExpr) {
                IrIntExpr intBody = (IrIntExpr) body;
                @SuppressWarnings("unchecked")
                Triple<AstLocal, IrIntExpr, IrBoolExpr>[] labeledPermutation = new Triple[decl.getLocals().length];
                for (int i = 0; i < labeledPermutation.length; i++) {
                    labeledPermutation[i] = new Triple<AstLocal, IrIntExpr, IrBoolExpr>(
                            decl.getLocals()[i], intBody, $(True));
                }
                @SuppressWarnings("unchecked")
                Triple<AstLocal, IrIntExpr, IrBoolExpr>[][] labeledSequence = new Triple[][]{labeledPermutation};
                return labeledSequence;
            }
            if (body instanceof IrSetExpr) {
                IrSetExpr setBody = (IrSetExpr) body;
                IrDomain env = setBody.getEnv();
                IrDomain ker = setBody.getKer();
                // TODO: need a different strategy otherwise
                assert env.getLowBound() >= 0;
                @SuppressWarnings("unchecked")
                Pair<IrIntExpr, IrBoolExpr>[] members = new Pair[env.getHighBound() + 1];
                for (int i = 0; i < env.getLowBound(); i++) {
                    members[i] = new Pair<IrIntExpr, IrBoolExpr>($(constant(i)), $(False));
                }
                for (int i = env.getLowBound(); i <= env.getHighBound(); i++) {
                    members[i] = new Pair<IrIntExpr, IrBoolExpr>($(constant(i)),
                            $(ker.contains(i) ? True
                            : bool(Util.intercalate("/", AstUtil.getNames(decl.getLocals())) + "#" + i)));
                }
                module.addConstraint(boolChannel(Util.mapSnd(Arrays.asList(members)), setBody));
                Pair<IrIntExpr, IrBoolExpr>[][] sequence = decl.isDisjoint() ? Util.permutations(members,
                        decl.getLocals().length) : Util.sequence(members, decl.getLocals().length);

                @SuppressWarnings("unchecked")
                Triple<AstLocal, IrIntExpr, IrBoolExpr>[][] labeledSequence = new Triple[sequence.length][];
                for (int i = 0; i < labeledSequence.length; i++) {
                    Pair<IrIntExpr, IrBoolExpr>[] permutation = sequence[i];
                    @SuppressWarnings("unchecked")
                    Triple<AstLocal, IrIntExpr, IrBoolExpr>[] labeledPermutation = new Triple[permutation.length];
                    for (int j = 0; j < labeledPermutation.length; j++) {
                        labeledPermutation[j] = new Triple<AstLocal, IrIntExpr, IrBoolExpr>(
                                decl.getLocals()[j], permutation[j]);
                    }
                    labeledSequence[i] = labeledPermutation;
                }
                return labeledSequence;
            }
            throw new AstException();
        }

        // TODO optimize SOME
        @Override
        public IrExpr visit(AstQuantify ast, Void a) {
            AstDecl decls[] = ast.getDecls();
            @SuppressWarnings("unchecked")
            Triple<AstLocal, IrIntExpr, IrBoolExpr>[][][] compiledDecls = new Triple[decls.length][][];
            for (int i = 0; i < compiledDecls.length; i++) {
                compiledDecls[i] = compileDecl(decls[i]);
            }
            compiledDecls = Util.sequence(compiledDecls);

            List<IrBoolExpr> compiled = new ArrayList<IrBoolExpr>();
            for (Triple<AstLocal, IrIntExpr, IrBoolExpr>[][] quants : compiledDecls) {
                List<IrBoolExpr> constraints = new ArrayList<IrBoolExpr>();
                for (Triple<AstLocal, IrIntExpr, IrBoolExpr>[] quantDecls : quants) {
                    for (Triple<AstLocal, IrIntExpr, IrBoolExpr> quantLocals : quantDecls) {
                        constraints.add(quantLocals.getThd());
                        locals.put(quantLocals.getFst(), quantLocals.getSnd());
                    }
                }
                IrBoolExpr compiledBody = compile(ast.getBody());
                if (Quantifier.All.equals(ast.getQuantifier())) {
                    compiled.add(implies(and(constraints), compiledBody));
                } else {
                    constraints.add(compiledBody);
                    compiled.add(and(constraints));
                }
            }

            switch (ast.getQuantifier()) {
                case All:
                    return and(compiled);
                case Lone:
                    return lone(compiled);
                case None:
                    return not(or(compiled));
                case One:
                    return one(compiled);
                case Some:
                    return or(compiled);
                default:
                    throw new AstException();

            }
        }
    };

    /*
     ******************
     * Build functions.
     ******************
     */
    /**
     * Build the child set for the Clafer.
     *
     * @param clafer the Clafer
     * @return the variables to represent the child relation
     */
    private IrSetVar[] buildChildSet(AstConcreteClafer clafer) {
        assert Format.LowGroup.equals(getFormat(clafer));

        int parentScope = getScope(clafer.getParent());
        PartialSolution partialParentSolution = getPartialSolution(clafer.getParent());

        int claferScope = getScope(clafer);
        Card card = getCard(clafer);
        assert card.hasHigh();

        int low = 0;
        int high = card.getHigh();
        int max = claferScope - 1;

        IrSetVar[] skip = new IrSetVar[parentScope];
        for (int i = 0; i < skip.length; i++) {
            if (low <= max) {
                IrDomain env = boundDomain(low, Math.min(high - 1, max));
                IrDomain ker = EmptyDomain;
                int cardLow = 0;
                int cardHigh = card.getHigh();
                if (partialParentSolution.hasClafer(i)) {
                    int prevHigh = high - card.getHigh();
                    int nextLow = low + card.getLow();
                    if (nextLow > prevHigh) {
                        ker = boundDomain(prevHigh, Math.min(nextLow - 1, max));
                    }
                    cardLow = card.getLow();
                }
                cardLow = Math.max(cardLow, ker.size());
                cardHigh = Math.min(cardHigh, env.size());
                skip[i] = set(clafer.getName() + "#" + i, env, ker, boundDomain(cardLow, cardHigh));
            } else {
                skip[i] = EmptySet;
            }
            if (partialParentSolution.hasClafer(i)) {
                low += card.getLow();
            }
            high += card.getHigh();
        }
        return skip;
    }

    /**
     * Create the parent pointers for the Clafer.
     *
     * @param clafer the Clafer
     * @return the variables to represent the parent relation
     */
    private IrIntVar[] buildParentPointers(AstConcreteClafer clafer) {
        PartialSolution solution = getPartialSolution(clafer);
        boolean known = solution.parentSolutionKnown();
        IrIntVar[] pointers = new IrIntVar[solution.size()];
        for (int i = 0; i < pointers.length; i++) {
            int[] possibleParents = solution.getPossibleParents(i);
            pointers[i] = enumInt(clafer.getName() + "@Parent#" + i,
                    solution.hasClafer(i) || known
                    ? possibleParents
                    : Util.snoc(possibleParents, getScope(clafer.getParent())));
        }
        return pointers;
    }

    /**
     * Create the references pointers for the Clafer.
     *
     * @param ref the reference Clafer
     * @return the variables to represent the reference relation
     */
    private IrIntVar[] buildRefPointers(AstRef ref) {
        AstClafer src = ref.getSourceType();
        AstClafer tar = ref.getTargetType();

        PartialSolution partialSolution = getPartialSolution(src);
        int[][] partialInts = getPartialInts(ref);
        IrIntVar[] ivs = new IrIntVar[getScope(src)];
        for (int i = 0; i < ivs.length; i++) {
            if (partialInts[i] == null) {
                int tarLow = getScopeLow(tar);
                int tarHigh = getScopeHigh(tar);
                IrDomain domain = tarLow <= tarHigh ? boundDomain(tarLow, tarHigh) : ZeroDomain;
                if (!partialSolution.hasClafer(i) // <-- this means that ref may need to be zeroed out.
                        && !domain.contains(0) // <-- this means that the domain doesn't allow zeroing out.
                        ) {
                    domain = IrUtil.union(ZeroDomain, domain); // <-- add zero to the domain.
                }
                ivs[i] = domainInt(src.getName() + "@Ref" + i, domain);
            } else {
                if (partialSolution.hasClafer(i)) {
                    ivs[i] = enumInt(src.getName() + "@Ref" + i, partialInts[i]);
                } else {
                    ivs[i] = enumInt(src.getName() + "@Ref" + i, Util.cons(0, partialInts[i]));
                }
            }
        }

        return ivs;
    }

    /**
     * Enforce the size of a set to be within the cardinality.
     *
     * @param setCard the set to constrain
     * @param card the cardinality
     * @return card.low &le; |setCard| &le; card.high
     */
    private IrBoolExpr constrainCard(IrIntExpr setCard, Card card) {
        if (card.isExact()) {
            return equal(setCard, card.getLow());
        }
        if (card.hasLow() && card.hasHigh()) {
            return within(setCard, boundDomain(card.getLow(), card.getHigh()));
        }
        if (card.hasLow()) {
            return greaterThanEqual(setCard, card.getLow());
        }
        if (card.hasHigh()) {
            return lessThanEqual(setCard, card.getHigh());
        }
        return $(True);
    }

    /*
     ************************
     * Convenience functions.
     ************************
     */
    private int getScopeLow(AstClafer clafer) {
        return clafer instanceof AstIntClafer ? analysis.getScope().getIntLow() : 0;
    }

    private int getScopeHigh(AstClafer clafer) {
        return clafer instanceof AstIntClafer ? analysis.getScope().getIntHigh() : getScope(clafer) - 1;
    }

    private int getScope(AstClafer clafer) {
        return analysis.getScope().getScope(clafer);
    }

    private Format getFormat(AstClafer clafer) {
        return analysis.getFormat(clafer);
    }

    private PartialSolution getPartialSolution(AstClafer clafer) {
        return analysis.getPartialSolution(clafer);
    }

    private PartialSolution getPartialParentSolution(AstConcreteClafer clafer) {
        return getPartialSolution(clafer.getParent());
    }

    private int[][] getPartialInts(AstRef ref) {
        return analysis.getPartialInts(ref);
    }

    private int getOffset(AstAbstractClafer sup, AstClafer sub) {
        return analysis.getOffsets(sup).getOffset(sub);
    }

    private Card getCard(AstConcreteClafer clafer) {
        return analysis.getCard(clafer);
    }

    private Card getGlobalCard(AstClafer clafer) {
        return analysis.getGlobalCard(clafer);
    }

    private int getDepth(AstAbstractClafer clafer) {
        return analysis.getDepth(clafer);
    }

    private AstClafer getType(AstExpr expr) {
        return analysis.getType(expr);
    }

    private List<AstConstraint> getConstraints() {
        return analysis.getConstraints();
    }
}
