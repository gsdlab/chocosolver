package org.clafer.ast.compiler;

import gnu.trove.list.array.TIntArrayList;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstArithm;
import org.clafer.ast.AstBoolArithm;
import org.clafer.ast.AstBoolExpr;
import org.clafer.ast.AstCard;
import org.clafer.ast.AstChildRelation;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstCompare;
import org.clafer.ast.AstConcat;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstConstant;
import org.clafer.ast.AstConstraint;
import org.clafer.ast.AstDecl;
import org.clafer.ast.AstDifference;
import org.clafer.ast.AstDowncast;
import org.clafer.ast.AstException;
import org.clafer.ast.AstExpr;
import org.clafer.ast.AstExprVisitor;
import org.clafer.ast.AstGlobal;
import org.clafer.ast.AstIfThenElse;
import org.clafer.ast.AstIntClafer;
import org.clafer.ast.AstIntersection;
import org.clafer.ast.AstInverse;
import org.clafer.ast.AstJoin;
import org.clafer.ast.AstJoinParent;
import org.clafer.ast.AstJoinRef;
import org.clafer.ast.AstLength;
import org.clafer.ast.AstLocal;
import org.clafer.ast.AstMembership;
import org.clafer.ast.AstMinus;
import org.clafer.ast.AstMod;
import org.clafer.ast.AstModel;
import org.clafer.ast.AstNot;
import org.clafer.ast.AstParentRelation;
import org.clafer.ast.AstPrefix;
import org.clafer.ast.AstProduct;
import org.clafer.ast.AstQuantify;
import org.clafer.ast.AstQuantify.Quantifier;
import org.clafer.ast.AstRef;
import org.clafer.ast.AstRefRelation;
import org.clafer.ast.AstSetExpr;
import org.clafer.ast.AstSetTest;
import org.clafer.ast.AstStringClafer;
import org.clafer.ast.AstStringConstant;
import org.clafer.ast.AstSuffix;
import org.clafer.ast.AstSum;
import org.clafer.ast.AstTernary;
import org.clafer.ast.AstThis;
import org.clafer.ast.AstTransitiveClosure;
import org.clafer.ast.AstUnion;
import org.clafer.ast.AstUpcast;
import org.clafer.ast.AstUtil;
import org.clafer.ast.Card;
import org.clafer.ast.JoinSetWithStringException;
import org.clafer.ast.ProductType;
import org.clafer.ast.analysis.AbstractOffsetAnalyzer;
import org.clafer.ast.analysis.Analysis;
import org.clafer.ast.analysis.Analyzer;
import org.clafer.ast.analysis.CardAnalyzer;
import org.clafer.ast.analysis.Format;
import org.clafer.ast.analysis.FormatAnalyzer;
import org.clafer.ast.analysis.GlobalCardAnalyzer;
import org.clafer.ast.analysis.OptimizerAnalyzer;
import org.clafer.ast.analysis.PartialIntAnalyzer;
import org.clafer.ast.analysis.PartialSolution;
import org.clafer.ast.analysis.PartialSolutionAnalyzer;
import org.clafer.ast.analysis.ScopeAnalyzer;
import org.clafer.ast.analysis.SymmetryAnalyzer;
import org.clafer.ast.analysis.Type;
import org.clafer.ast.analysis.TypeAnalyzer;
import org.clafer.collection.DisjointSets;
import org.clafer.collection.Either;
import org.clafer.collection.Monoid;
import org.clafer.collection.Pair;
import org.clafer.collection.Triple;
import org.clafer.common.Check;
import org.clafer.common.Util;
import org.clafer.graph.GraphUtil;
import org.clafer.graph.KeyGraph;
import org.clafer.graph.Vertex;
import org.clafer.ir.IrBoolExpr;
import org.clafer.ir.IrBoolVar;
import org.clafer.domain.Domain;
import org.clafer.domain.Domains;
import static org.clafer.domain.Domains.*;
import org.clafer.ir.IrExpr;
import org.clafer.ir.IrIntArrayExpr;
import org.clafer.ir.IrIntExpr;
import org.clafer.ir.IrIntVar;
import org.clafer.ir.IrModule;
import org.clafer.ir.IrSetArrayExpr;
import org.clafer.ir.IrSetExpr;
import org.clafer.ir.IrSetVar;
import org.clafer.ir.IrStringExpr;
import org.clafer.ir.IrStringVar;
import org.clafer.ir.IrUtil;
import org.clafer.ir.IrVar;
import static org.clafer.ir.Irs.*;
import org.clafer.ir.Product;
import org.clafer.ir.Sum;
import org.clafer.objective.Objective;
import org.clafer.scope.Scope;
import org.clafer.scope.ScopeBuilder;

/**
 * Compile from AST to IR.
 *
 * @author jimmy
 */
public class AstCompiler {

    public static final Analyzer[] DefaultAnalyzers = new Analyzer[]{
        new TypeAnalyzer(),
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
    private final List<Symmetry> symmetries = new ArrayList<>();
    private final boolean fullSymmetryBreaking;

    private AstCompiler(AstModel model, Scope scope, IrModule module, Analyzer[] analyzers, boolean fullSymmetryBreaking) {
        this(model, scope, new Objective[0], module, analyzers, fullSymmetryBreaking);
    }

    private AstCompiler(AstModel model, Scope scope, Objective[] objectives, IrModule module, Analyzer[] analyzers, boolean fullSymmetryBreaking) {
        this.analysis = Analysis.analyze(model, scope, objectives, analyzers);
        this.module = Check.notNull(module);
        this.fullSymmetryBreaking = fullSymmetryBreaking;
    }

    public static AstSolutionMap compile(AstModel in, Scope scope, IrModule out, boolean fullSymmetryBreaking) {
        return compile(in, scope, out, DefaultAnalyzers, fullSymmetryBreaking);
    }

    public static AstSolutionMap compile(AstModel in, Scope scope, IrModule out, Analyzer[] analyzers, boolean fullSymmetryBreaking) {
        AstCompiler compiler = new AstCompiler(in, scope, out, analyzers, fullSymmetryBreaking);
        return compiler.compile();
    }

    public static AstSolutionMap compile(AstModel in, Scope scope, Objective[] objectives, IrModule out, boolean fullSymmetryBreaking) {
        return compile(in, scope, objectives, out, DefaultAnalyzers, fullSymmetryBreaking);
    }

    public static AstSolutionMap compile(AstModel in, Scope scope, Objective[] objectives, IrModule out, Analyzer[] analyzers, boolean fullSymmetryBreaking) {
        AstCompiler compiler = new AstCompiler(in, scope, objectives,
                out, analyzers, fullSymmetryBreaking);
        return compiler.compile();
    }

    /**
     * @return the order to initialize regular variables
     */
    private List<AstClafer> initOrder() {
        List<AstAbstractClafer> abstractClafers = analysis.getAbstractClafers();
        List<AstConcreteClafer> concreteClafers = analysis.getConcreteClafers();

        KeyGraph<AstClafer> dependency = new KeyGraph<>();
        for (AstAbstractClafer abstractClafer : abstractClafers) {
            Vertex<AstClafer> node = dependency.getVertex(abstractClafer);
            for (AstClafer sub : abstractClafer.getSubs()) {
                node.addNeighbour(dependency.getVertex(sub));
            }
        }
        for (AstConcreteClafer concreteClafer : concreteClafers) {
            Vertex<AstClafer> node = dependency.getVertex(concreteClafer);
            if (Format.ParentGroup.equals(getFormat(concreteClafer))) {
                /*
                 * Low group does not create the dependency because it does not
                 * require the parent to initialize first. This allows for
                 * models like the one below.
                 *
                 * abstract Path
                 *     p : Path ?
                 *
                 * If the "?" were a fixed cardinality instead, then an
                 * exception will occur, but the model would not be satisfiable
                 * anyways for any fixed cardinality greater than zero.
                 */
                node.addNeighbour(dependency.getVertex(concreteClafer.getParent()));
            }
        }
        List<Set<AstClafer>> components = GraphUtil.computeStronglyConnectedComponents(dependency);
        List<AstClafer> clafers = new ArrayList<>();
        for (Set<AstClafer> component : components) {
            if (component.size() != 1) {
                // See the above comment about low groups.
                throw new AstException("Cannot satisfy the cycle " + component);
            }
            clafers.addAll(component);
        }
        return clafers;
    }

    private static boolean isThis(AstSetExpr expr) {
        if (expr instanceof AstThis) {
            return true;
        }
        if (expr instanceof AstDowncast) {
            AstDowncast cast = (AstDowncast) expr;
            return isThis(cast.getBase());
        }
        if (expr instanceof AstUpcast) {
            AstUpcast cast = (AstUpcast) expr;
            return isThis(cast.getBase());
        }
        return false;
    }

    /**
     * Checks if the constraint can always be enforced regardless if the
     * attached Clafer exists or not.
     *
     * @param expr
     * @return {@code false} if {@code expr} can always be enforced,
     * {@code true} otherwise or unknown
     */
    private static boolean isConditional(AstBoolExpr expr) {
        // this not in ___
        if (expr instanceof AstMembership) {
            AstMembership membership = (AstMembership) expr;
            switch (membership.getOp()) {
                case NotIn:
                    return !isThis(membership.getMember());
            }
        } else if (expr instanceof AstSetTest) {
            AstSetTest membership = (AstSetTest) expr;
            switch (membership.getOp()) {
                case NotEqual:
                    return !isThis(membership.getLeft()) && !isThis(membership.getRight());
            }
        }
        return true;
    }

    private AstSolutionMap compile() {
        IrSetVar rootSet = constant(new int[]{0});
        sets.put(analysis.getModel(), rootSet);
        siblingSets.put(analysis.getModel(), new IrSetVar[]{rootSet});
        memberships.put(analysis.getModel(), new IrBoolExpr[]{True});

        List<AstClafer> clafers = initOrder();
        for (AstClafer clafer : clafers) {
            if (clafer instanceof AstConcreteClafer && !AstUtil.isRoot((AstConcreteClafer) clafer)) {
                initConcrete((AstConcreteClafer) clafer);
            } else if (clafer instanceof AstAbstractClafer) {
                initAbstract((AstAbstractClafer) clafer);
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

        Map<AstConstraint, IrBoolVar> softVars = new HashMap<>();
        for (AstConstraint constraint : analysis.getConstraints()) {
            AstClafer clafer = constraint.getContext();
            AstBoolExpr expr = analysis.getExpr(constraint);
            int scope = getScope(clafer);
            if (analysis.isHard(constraint)) {
                for (int j = 0; j < scope; j++) {
                    ExpressionCompiler expressionCompiler = new ExpressionCompiler(j);
                    IrBoolExpr thisConstraint = expressionCompiler.compile(expr);
                    IrBoolExpr conditionalConstraint = isConditional(expr)
                            ? implies(memberships.get(clafer)[j], thisConstraint)
                            : thisConstraint;
                    module.addConstraint(conditionalConstraint);
                }
            } else {
                IrBoolVar softVar = bool(constraint.toString());
                softVars.put(constraint, softVar);
                for (int j = 0; j < scope; j++) {
                    ExpressionCompiler expressionCompiler = new ExpressionCompiler(j);
                    IrBoolExpr thisConstraint = expressionCompiler.compile(expr);
                    IrBoolExpr conditionalConstraint = isConditional(expr)
                            ? implies(memberships.get(clafer)[j], thisConstraint)
                            : thisConstraint;
                    module.addConstraint(ifOnlyIf(softVar, conditionalConstraint));
                }
                module.addVariable(softVar);
            }
        }
        IrIntExpr softSum = add(softVars.values());
        IrIntVar sumSoftVars = domainInt("SumSoftVar", softSum.getDomain());
        module.addConstraint(equal(sumSoftVars, softSum));

        for (IrSetVar[] childSet : siblingSets.values()) {
            module.addVariables(childSet);
        }
        for (IrIntVar[] refs : refPointers.values()) {
            module.addVariables(refs);
        }
        for (IrStringVar[] refs : refStrings.values()) {
            module.addVariables(refs);
        }

        for (Set<AstClafer> component : analysis.getClafersInParentAndSubOrder()) {
            if (component.size() > 1) {
                /*
                 * Add additional constraints for to handle cases where a
                 * descendent inherits an ancestor.
                 *
                 * Let A be an abstract Clafer and B be a descendant of A that
                 * inherits A. Let F be a mapping every B to its ancestor A.
                 * Enforce that F is an acyclic function.
                 */
                AstAbstractClafer unionType = (AstAbstractClafer) AstUtil.getLowestCommonSupertype(component);
                IrIntExpr[] edges = new IrIntExpr[getScope(unionType)];
                IrIntExpr uninitialized = constant(edges.length);
                Arrays.fill(edges, uninitialized);
                for (AstClafer clafer : component) {
                    if (clafer instanceof AstConcreteClafer) {
                        AstConcreteClafer concreteChild = (AstConcreteClafer) clafer;
                        IrBoolExpr[] members = memberships.get(concreteChild);
                        IrIntExpr[] parents = parentPointers.get(concreteChild);
                        int offset = getOffset(unionType, concreteChild);
                        int parentOffset = getOffset(unionType, concreteChild.getParent());
                        for (int i = 0; i < members.length; i++) {
                            assert edges[i + offset] == uninitialized;
                            IrIntExpr value = ternary(members[i],
                                    // Add the offset to upcast the parent pointer
                                    add(parents[i], parentOffset),
                                    uninitialized);
                            IrIntVar edge = domainInt(
                                    "Edge@" + concreteChild + "->" + concreteChild.getParent() + "#" + i,
                                    value.getDomain());
                            module.addConstraint(equal(edge, value));
                            edges[i + offset] = edge;
                        }
                    }
                }
                for (AstClafer clafer : component) {
                    if (clafer instanceof AstConcreteClafer) {
                        IrBoolExpr[] members = memberships.get(clafer);
                        int offset = getOffset(unionType, clafer);
                        for (int i = 0; i < members.length; i++) {
                            for (int j = i + 1; j < members.length; j++) {
                                /*
                                 * Symmetry breaking. The lower indexed element
                                 * appears on top of the higher indexed element.
                                 */
                                module.addConstraint(unreachable(edges, i + offset, j + offset));
                            }
                        }
                    }
                }
                module.addConstraint(acyclic(edges));
            }
        }

        ExpressionCompiler expressionCompiler = new ExpressionCompiler(0);
        Map<Objective, IrIntVar> objectiveVars = new HashMap<>();
        Map<Objective, AstSetExpr> objectives = analysis.getObjectiveExprs();
        for (Entry<Objective, AstSetExpr> objective : objectives.entrySet()) {
            IrIntExpr objectiveExpr = expressionCompiler.asInt(
                    expressionCompiler.compile(objective.getValue()));
            IrIntVar objectiveVar = domainInt("Objective" + objective.getKey(),
                    objectiveExpr.getDomain());
            module.addConstraint(equal(objectiveVar, objectiveExpr));
            module.addVariable(objectiveVar);
            objectiveVars.put(objective.getKey(), objectiveVar);
        }

        KeyGraph<Either<IrExpr, IrBoolExpr>> dependencies = new KeyGraph<>();
        for (Symmetry symmetry : symmetries) {
            IrBoolExpr constraint = symmetry.getConstraint();
            Vertex<Either<IrExpr, IrBoolExpr>> constraintNode
                    = dependencies.getVertex(Either.<IrExpr, IrBoolExpr>right(constraint));
            for (IrExpr output : symmetry.getOutput()) {
                dependencies.getVertex(Either.<IrExpr, IrBoolExpr>left(output))
                        .addNeighbour(constraintNode);
            }
            for (IrExpr input : symmetry.getInput()) {
                constraintNode.addNeighbour(
                        dependencies.getVertex(Either.<IrExpr, IrBoolExpr>left(input)));
            }
        }
        Set<IrVar> variables = module.getVariables();
        Set<Vertex<Either<IrExpr, IrBoolExpr>>> start = new HashSet<>();
        for (IrVar variable : variables) {
            Vertex<Either<IrExpr, IrBoolExpr>> vertex
                    = dependencies.getVertexIfPresent(Either.<IrExpr, IrBoolExpr>left(variable));
            if (vertex != null) {
                start.add(vertex);
            }
        }
        Set<Either<IrExpr, IrBoolExpr>> reachables = GraphUtil.reachable(start, dependencies);
        for (Either<IrExpr, IrBoolExpr> reachable : reachables) {
            if (reachable.isRight()) {
                module.addConstraint(reachable.getRight());
            }
        }

        return new AstSolutionMap(analysis.getModel(), siblingSets,
                refPointers, refStrings,
                softVars, sumSoftVars,
                objectiveVars, analysis);
    }

    private void initConcrete(AstConcreteClafer clafer) {
        parentPointers.put(clafer, buildParentPointers(clafer));
        buildRef(clafer);
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
                IrSetExpr union = union(siblingSet, true);
                IrSetVar set = set(clafer.getName(), union.getEnv(), union.getKer(), union.getCard());
                module.addConstraint(equal(set, union));
                sets.put(clafer, set);
                break;
        }

        if (fullSymmetryBreaking) {
            int scope = getScope(clafer);
            int parentScope = getScope(clafer.getParent());
            IrIntExpr[][] index;
            AstRef ref = AstUtil.getInheritedRef(clafer);
            // If the Clafer either needs children or reference to be introduce symmetry.
            if (analysis.hasInteritedBreakableChildren(clafer)
                    || (ref != null && analysis.isBreakableRef(ref))
                    || analysis.isInheritedBreakableTarget(clafer)) {
                index = new IrIntExpr[parentScope][getCard(clafer).getHigh()];
                for (int i = 0; i < index.length; i++) {
                    for (int j = 0; j < index[i].length; j++) {
                        index[i][j]
                                = boundInt(clafer.getName() + "@Index#" + i + "#" + j, -1, scope);
                    }
                }
            } else {
                // Optimize for nonsymmetric nodes. Don't compute the smallest indices, 
                // just use the cardinalities.
                IrSetVar[] childSet = siblingSets.get(clafer);
                index = new IrIntExpr[childSet.length][];
                for (int i = 0; i < index.length; i++) {
                    index[i] = new IrIntExpr[]{card(childSet[i])};
                }
            }
            indices.put(clafer, index);
        }
    }

    private void constrainConcrete(AstConcreteClafer clafer) {
        IrSetExpr[] siblingSet = siblingSets.get(clafer);
        IrIntExpr[] parents = parentPointers.get(clafer);
        if (!getPartialSolution(clafer).parentSolutionKnown()) {
            if (getGlobalCard(clafer).isExact()) {
                // No unused
                module.addConstraint(intChannel(parents, siblingSet));
            } else {
                IrSetVar unused = set(clafer.getName() + "@Unused", getPartialSolution(clafer).getUnknownClafers());
                module.addConstraint(intChannel(parents, Util.snoc(siblingSet, unused)));
            }
        }

        Pair<AstRef, Integer> refPair = analysis.getInheritedRefId(clafer);
        AstRef ref = refPair == null ? null : refPair.getFst();
        int refOffset = refPair == null ? 0 : refPair.getSnd().intValue();

        int scope = getScope(clafer);
        IrBoolExpr[] members = memberships.get(clafer);

        // Two ids a and b are in the same partition if symmetry breaking guarantees
        // that ref[a] and ref[b] ard different.
        DisjointSets<Integer> refPartitions = null;
        // If the Clafer either needs children or reference to be introduce symmetry.
        if (fullSymmetryBreaking && scope > 1
                && (analysis.hasInteritedBreakableChildren(clafer)
                || (ref != null && analysis.isBreakableRef(ref))
                || analysis.isInheritedBreakableTarget(clafer))) {

            IrIntExpr[] weight = new IrIntExpr[scope];
            IrIntExpr[][] index = indices.get(clafer);

            analysis.getHierarcyIds(clafer, refOffset);
            IrIntExpr[][] childIndices = new IrIntExpr[weight.length][];

            List<Pair<AstClafer, Integer>> offsets = analysis.getHierarcyOffsets(clafer);
            Collections.reverse(offsets);
            boolean[] breakableRefIds = new boolean[childIndices.length];
            for (int i = 0; i < childIndices.length; i++) {
                List<IrIntExpr> childIndex = new ArrayList<>();
                for (Pair<AstClafer, Integer> offset : offsets) {
                    for (AstConcreteClafer child : analysis.getBreakableChildren(offset.getFst())) {
                        childIndex.addAll(Arrays.asList(indices.get(child)[i + offset.getSnd()]));
                    }
                }
                if (ref != null && analysis.isBreakableRef(ref)) {
                    breakableRefIds[i] = analysis.isBreakableRefId(ref, i + refOffset);
                    if (ref.getTargetType() instanceof AstStringClafer) {
                        childIndex.addAll(Arrays.asList(IrUtil.pad(
                                refStrings.get(ref)[i + refOffset].getCharVars(),
                                analysis.getScope().getStringLength())));
                        if (ref.isUnique()) {
                            childIndex.add(members[i]);
                        }
                    } else {
                        // References need a positive weight, so to use their value as
                        // a weight, need to offset it so that it always positive.
                        childIndex.add(
                                breakableRefIds[i]
                                        // The id of the target is the weight.
                                        ? minus(refPointers.get(ref)[i + refOffset])
                                        // If analysis says that this id does not need breaking
                                        // then give it a constant weight. Any constant is fine.
                                        : Zero);
                    }
                }
                if (analysis.isInheritedBreakableTarget(clafer)) {
                    for (Pair<AstClafer, Integer> hierarchy : analysis.getHierarcyIds(clafer, i)) {
                        for (AstRef sourceRef : analysis.getBreakableTarget(hierarchy.getFst())) {
                            IrIntVar[] sourceRefs = refPointers.get(sourceRef);

                            IrIntExpr[] array = new IrIntExpr[sourceRefs.length];
                            System.arraycopy(sourceRefs, 0, array, 0, array.length);
                            IrIntExpr count = count(hierarchy.getSnd().intValue(), array);
                            IrIntVar countVar = domainInt("CountVar" + countCount++, count.getDomain());
                            module.addConstraint(equal(countVar, count));
                            childIndex.add(countVar);
                        }
                    }
                }
                childIndices[i] = childIndex.toArray(new IrIntExpr[childIndex.size()]);
            }
            for (int i = 0; i < weight.length; i++) {
                weight[i]
                        = childIndices[i].length == 0 ? Zero
                                : boundInt(clafer.getName() + "#" + i + "@Weight", 0, scope - 1);
            }
            if (getScope(clafer.getParent()) > 1) {
                symmetries.add(new LexChainChannel(childIndices, weight));
                for (int i = 0; i < siblingSet.length; i++) {
                    symmetries.add(new FilterString(siblingSet[i], weight, index[i]));
                }
            }
            if (getCard(clafer).getHigh() > 1) {
                for (int i = 0; i < parents.length - 1; i++) {
                    if (ref != null && analysis.isBreakableRef(ref) && ref.isUnique()) {
                        assert childIndices[i + 1].length == childIndices[i].length;
                        if (breakableRefIds[i]) {
                            if (refPartitions == null) {
                                refPartitions = new DisjointSets<>();
                            }
                            refPartitions.union(i, i + 1);
                        }
                        // Refs are unique and part of the weight. It is impossible for
                        // two weights to be the same. Enforce a strict order.
                        module.addConstraint(implies(and(members[i], equal(parents[i], parents[i + 1])),
                                sortStrict(childIndices[i + 1], childIndices[i])));
                    } else {
                        module.addConstraint(implies(equal(parents[i], parents[i + 1]),
                                sort(childIndices[i + 1], childIndices[i])));
                    }
                }
            }
        }

        if (ref != null) {
            AstClafer tar = ref.getTargetType();
            if (tar instanceof AstStringClafer) {
                IrStringVar[] strings = Arrays.copyOfRange(refStrings.get(ref),
                        refOffset, refOffset + getScope(clafer));
                if (ref.isUnique()) {
                    if (getCard(clafer).getHigh() > 1) {
                        for (int i = 0; i < strings.length - 1; i++) {
                            for (int j = i + 1; j < strings.length; j++) {
                                if (refPartitions == null || !refPartitions.connected(i, j)) {
                                    module.addConstraint(
                                            implies(and(members[i], members[j], equal(parents[i], parents[j])),
                                                    notEqual(strings[i], strings[j])));
                                }
                            }
                        }
                    }
                }
                assert strings.length == members.length;
                for (int i = 0; i < members.length; i++) {
                    module.addConstraint(implies(not(members[i]), equal(strings[i], EmptyString)));
                }
            } else {
                IrIntVar[] refs = Arrays.copyOfRange(refPointers.get(ref),
                        refOffset, refOffset + getScope(clafer));
                if (ref.isUnique()) {
                    if (getCard(clafer).getHigh() > 1) {
                        for (int i = 0; i < refs.length - 1; i++) {
                            for (int j = i + 1; j < refs.length; j++) {
                                if (refPartitions == null || !refPartitions.connected(i, j)) {
                                    module.addConstraint(
                                            implies(and(members[i], equal(parents[i], parents[j])),
                                                    notEqual(refs[i], refs[j])));
                                }
                            }
                        }
                    }
                    IrIntExpr size
                            = ref.getTargetType() instanceof AstIntClafer
                                    ? constant(analysis.getScope().getIntHigh() - analysis.getScope().getIntLow() + 1)
                                    : card(sets.get(ref.getTargetType()));
                    for (IrSetExpr sibling : siblingSet) {
                        module.addConstraint(lessThanEqual(card(sibling), size));
                    }
                }
                assert refs.length == members.length;
                for (int i = 0; i < members.length; i++) {
                    // The ref pointers must point to the special uninitialized value
                    // if the Clafer owning the ref pointers does not exists.
                    module.addConstraint(ifOnlyIf(not(members[i]), equal(refs[i], getUninitalizedRef(tar))));
                }
                if (!ref.getTargetType().isPrimitive()) {
                    IrSetVar targetSet = sets.get(ref.getTargetType());
                    for (int i = 0; i < refs.length; i++) {
                        // The ref pointers must point to a target that exists.
                        module.addConstraint(ifOnlyIf(members[i], member(refs[i], targetSet)));
                    }
                }
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
            for (int i = 0; i < childrenSets.length; i++) {
                AstConcreteClafer child = children.get(i);
                childrenSets[i] = siblingSets.get(child);
            }
            int scope = getScope(clafer);
            for (int i = 0; i < scope; i++) {
                IrIntExpr[] cards = new IrIntExpr[childrenSets.length];
                for (int j = 0; j < cards.length; j++) {
                    cards[j] = card(childrenSets[j][i]);
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
                members[i] = True;
            } else {
                members[i] = bool(clafer.getName() + "@Membership#" + i);
                if (childSet.length == 1 && members.length == 1) {
                    module.addConstraint(equal(members[i], card(childSet[0])));
                }
            }
        }
        Check.noNulls(members);
        memberships.put(clafer, members);
    }

    private void constrainLowGroupConcrete(AstConcreteClafer clafer) {
        IrBoolExpr[] members = memberships.get(clafer);
        IrSetVar set = sets.get(clafer);

        IrBoolExpr[] parentMembership = memberships.get(clafer.getParent());
        Card card = getCard(clafer);
        IrSetVar[] childSet = siblingSets.get(clafer);

        if (fullSymmetryBreaking) {
            module.addConstraint(selectN(members, card(set)));
            module.addConstraint(sort(childSet));
        }

        for (int i = 0; i < parentMembership.length; i++) {
            IrBoolExpr parentMember = parentMembership[i];
            if (card.isBounded()) {
                // Enforce cardinality.
                module.addConstraint(implies(parentMember, constrainCard(card(childSet[i]), card)));
            }
            module.addConstraint(implies(not(parentMember), equal(childSet[i], EmptySet)));
        }

        if (!(childSet.length == 1 && members.length == 1)) {
            module.addConstraint(boolChannel(members, set));
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
        if (fullSymmetryBreaking) {
            module.addConstraint(sort(parentPointers.get(clafer)));
        }
    }

    private void initParentGroupConcrete(AstConcreteClafer clafer) {
        PartialSolution partialParentSolution = getPartialParentSolution(clafer);

        IrSetVar[] children = new IrSetVar[partialParentSolution.size()];
        Card card = getCard(clafer);
        assert card.getLow() == card.getHigh();
        int lowCard = card.getLow();
        for (int i = 0; i < children.length; i++) {
            if (lowCard == 0) {
                children[i] = EmptySet;
            } else if (partialParentSolution.hasClafer(i)) {
                children[i] = constant(Util.fromTo(i * lowCard, i * lowCard + lowCard));
            } else {
                children[i] = set(clafer.getName() + "#" + i,
                        boundDomain(i * lowCard, i * lowCard + lowCard - 1),
                        EmptyDomain,
                        enumDomain(0, lowCard));
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
                Arrays.fill(members, parentMembership.length, members.length, False);
            }
        } else {
            for (int i = 0; i < parentMembership.length; i++) {
                for (int j = 0; j < lowCard; j++) {
                    members[i * lowCard + j] = parentMembership[i];
                }
            }
            Arrays.fill(members, parentMembership.length * lowCard, members.length, False);
        }
        Check.noNulls(members);
        memberships.put(clafer, members);
    }

    private void constrainParentGroupConcrete(AstConcreteClafer clafer) {
        PartialSolution partialParentSolution = getPartialParentSolution(clafer);

        IrSetVar[] children = siblingSets.get(clafer);
        assert getCard(clafer).getLow() == getCard(clafer).getHigh();
        int lowCard = getCard(clafer).getLow();
        for (int i = 0; i < children.length; i++) {
            if (!partialParentSolution.hasClafer(i)) {
                if (lowCard == 1) {
                    module.addConstraint(equal(memberships.get(clafer.getParent())[i],
                            card(children[i])));
                }
                module.addConstraint(implies(memberships.get(clafer.getParent())[i],
                        equal(children[i], constant(Util.fromTo(i * lowCard, i * lowCard + lowCard)))));
                module.addConstraint(implies(not(memberships.get(clafer.getParent())[i]),
                        equal(children[i], EmptySet)));

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
                module.addConstraint(boolChannel(members, unionSet));
            }
            sets.put(clafer, unionSet);
        }
        Check.noNulls(members);
        memberships.put(clafer, members);

        buildRef(clafer);
    }

    private void constrainAbstract(AstAbstractClafer clafer) {
        // Do nothing.
    }
    private final Map<AstClafer, IrSetVar> sets = new HashMap<>();
    private final Map<AstClafer, IrSetVar[]> siblingSets = new HashMap<>();
    private final Map<AstClafer, IrBoolExpr[]> memberships = new HashMap<>();
    private final Map<AstConcreteClafer, IrIntVar[]> parentPointers = new HashMap<>();
    private final Map<AstRef, IrIntVar[]> refPointers = new HashMap<>();
    private final Map<AstRef, IrStringVar[]> refStrings = new HashMap<>();
    private final Map<AstClafer, IrIntExpr[][]> indices = new HashMap<>();
    private int countCount = 0;
    private int concatRefsCount = 0;
    private int localCount = 0;

    private class ExpressionCompiler implements AstExprVisitor<Void, IrExpr> {

        private final int thisId;
        private final Map<AstLocal, IrIntExpr> locals = new HashMap<>();

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

        private IrSetArrayExpr asRelation(IrExpr expr, ProductType type) {
            assert type.arity() == 2;
            if (expr instanceof IrIntArrayExpr) {
                return filterNotEqual((IrIntArrayExpr) expr, getUninitalizedRef(type.get(1)));
            } else if (expr instanceof IrSetArrayExpr) {
                return (IrSetArrayExpr) expr;
            }
            // Bug.
            throw new AstException("Should not have passed type checking.");
        }

        private IrStringExpr asString(IrExpr expr) {
            if (expr instanceof IrStringExpr) {
                return ((IrStringExpr) expr);
            }
            // Bug.
            throw new AstException("Should not have passed type checking.");
        }

        private IrStringExpr[] asString(IrExpr[] exprs) {
            IrStringExpr[] strings = new IrStringExpr[exprs.length];
            for (int i = 0; i < strings.length; i++) {
                strings[i] = asString(exprs[i]);
            }
            return strings;
        }

        @Override
        public IrExpr visit(AstThis ast, Void a) {
            return constant(thisId);
        }

        @Override
        public IrExpr visit(AstGlobal ast, Void a) {
            IrSetVar global = sets.get(ast.getType());
            if (global.getEnv().size() == 1) {
                Domain constant = IrUtil.getConstant(global);
                if (constant != null) {
                    assert constant.size() == 1;
                    // Use an integer representation instead for a singleton set.
                    return constant(constant.getLowBound());
                }
            }
            return global;
        }

        @Override
        public IrExpr visit(AstConstant ast, Void a) {
            int[][] value = ast.getValue();
            if (value.length == 1) {
                int[] set = value[0];
                return set.length == 1 ? constant(set[0]) : constant(set);
            }
            throw new UnsupportedOperationException("TODO");
        }

        @Override
        public IrExpr visit(AstStringConstant ast, Void a) {
            return constant(ast.getValue());
        }

        @Override
        public IrExpr visit(AstJoin ast, Void a) {
            IrExpr left = compile(ast.getLeft());
            if (ast.getRight() instanceof AstChildRelation) {
                return doJoin(left, ((AstChildRelation) ast.getRight()).getChildType());
            }
            IrExpr right = compile(ast.getRight());
            if (right instanceof IrIntArrayExpr) {
                IrIntArrayExpr rightArray = (IrIntArrayExpr) right;
                if (left instanceof IrIntExpr) {
                    IrIntExpr uninitialized = constant(getUninitalizedRef(getType(ast).getClaferType()));
                    // Why uninitialized? The left expression can contain unused.
                    return element(snoc(rightArray, uninitialized), (IrIntExpr) left);
                } else if (left instanceof IrSetExpr) {
                    IrIntExpr uninitialized = constant(getUninitalizedRef(getType(ast).getClaferType()));
                    // Why uninitialized? The left expression can contain unused.
                    return joinFunction((IrSetExpr) left, snoc(rightArray, uninitialized), null);
                } else if (left instanceof IrIntArrayExpr) {
                    IrIntExpr uninitialized = constant(getUninitalizedRef(getType(ast).getCommonSupertype().get(1)));
                    // Why uninitialized? The left expression can contain unused.
                    IrIntArrayExpr leftArray = (IrIntArrayExpr) left;
                    rightArray = snoc(rightArray, uninitialized);
                    IrIntExpr[] join = new IrIntExpr[leftArray.length()];
                    for (int i = 0; i < join.length; i++) {
                        join[i] = element(rightArray, get(leftArray, i));
                    }
                    return array(join);
                } else if (left instanceof IrSetArrayExpr) {
                    IrIntExpr uninitialized = constant(getUninitalizedRef(getType(ast).getCommonSupertype().get(1)));
                    // Why uninitialized? The left expression can contain unused.
                    IrSetArrayExpr leftArray = (IrSetArrayExpr) left;
                    rightArray = snoc(rightArray, uninitialized);
                    IrSetExpr[] join = new IrSetExpr[leftArray.length()];
                    for (int i = 0; i < join.length; i++) {
                        // TODO global cardinality?
                        join[i] = joinFunction(get(leftArray, i), rightArray, null);
                    }
                    return array(join);
                }
            } else if (right instanceof IrSetArrayExpr) {
                // Why empty set? The left expression can contain unused.
                IrSetArrayExpr rightArray = snoc((IrSetArrayExpr) right, EmptySet);
                if (left instanceof IrIntExpr) {
                    // TODO which relations are injective?
                    return element(rightArray, (IrIntExpr) left);
                } else if (left instanceof IrSetExpr) {
                    // TODO which relations are injective?
                    return joinRelation((IrSetExpr) left, rightArray, false);
                } else if (left instanceof IrIntArrayExpr) {
                    IrIntArrayExpr leftArray = (IrIntArrayExpr) left;
                    IrSetExpr[] join = new IrSetExpr[leftArray.length()];
                    for (int i = 0; i < join.length; i++) {
                        // TODO which relations are injective?
                        join[i] = element(rightArray, get(leftArray, i));
                    }
                    return array(join);
                } else if (left instanceof IrSetArrayExpr) {
                    IrSetArrayExpr leftArray = ((IrSetArrayExpr) left);
                    IrSetExpr[] join = new IrSetExpr[leftArray.length()];
                    for (int i = 0; i < join.length; i++) {
                        // TODO which relations are injective?
                        join[i] = joinRelation(get(leftArray, i), rightArray, false);
                    }
                    return array(join);
                }
            }
            throw new AstException();
        }

        private IrExpr doJoin(IrExpr left, AstConcreteClafer right) {
            // Why empty set? The "take" var can contain unused.
            IrSetArrayExpr rightArray = snoc(array(siblingSets.get(right)), EmptySet);
            if (left instanceof IrIntExpr) {
                IrIntExpr $intLeft = (IrIntExpr) left;
                if (Format.ParentGroup.equals(getFormat(right)) && getCard(right).getLow() == 1) {
                    assert getCard(right).isExact();
                    return $intLeft;
                }
                return joinRelation(singleton($intLeft), rightArray, true);
            } else if (left instanceof IrSetExpr) {
                IrSetExpr $setLeft = (IrSetExpr) left;
                return joinRelation($setLeft, rightArray, true);
            } else if (left instanceof IrIntArrayExpr) {
                IrIntArrayExpr leftArray = (IrIntArrayExpr) left;
                IrSetExpr[] join = new IrSetExpr[leftArray.length()];
                for (int i = 0; i < join.length; i++) {
                    // TODO which relations are injective?
                    join[i] = element(rightArray, get(leftArray, i));
                }
                return array(join);
            } else if (left instanceof IrSetArrayExpr) {
                IrSetArrayExpr leftArray = ((IrSetArrayExpr) left);
                IrSetExpr[] join = new IrSetExpr[leftArray.length()];
                for (int i = 0; i < join.length; i++) {
                    join[i] = joinRelation(get(leftArray, i), rightArray, true);
                }
                return array(join);
            }
            throw new AstException();
        }

        @Override
        public IrExpr visit(AstJoinParent ast, Void a) {
            AstConcreteClafer childrenType = (AstConcreteClafer) getCommonSupertype(ast.getChildren()).getClaferType();

            IrExpr children = compile(ast.getChildren());
            if (children instanceof IrIntExpr) {
                IrIntExpr intChildren = (IrIntExpr) children;
                switch (getFormat(childrenType)) {
                    case ParentGroup:
                        assert getCard(childrenType).isExact();
                        int lowCard = getCard(childrenType).getLow();
                        return div(intChildren, constant(lowCard));
                    case LowGroup:
                        IrIntVar[] parents = parentPointers.get(childrenType);
                        if (intChildren.getHighBound() >= parents.length) {
                            parents = Util.snoc(parents, constant(parents.length));
                        }
                        return element(parents, intChildren);
                }
            } else if (children instanceof IrSetExpr) {
                IrSetExpr setChildren = (IrSetExpr) children;
                IrIntVar[] parents = parentPointers.get(childrenType);
                if (setChildren.getEnv().getHighBound() >= parents.length) {
                    parents = Util.snoc(parents, constant(parents.length));
                }
                return joinFunction(setChildren, parents, null);
            }
            throw new AstException();
        }

        @Override
        public IrExpr visit(AstJoinRef ast, Void a) {
            AstSetExpr deref = ast.getDeref();
            AstClafer derefType = getCommonSupertype(deref).getClaferType();

            Integer globalCardinality = null;
            IrExpr $deref;
            if (derefType.getRef().isUnique()) {
                if (deref instanceof AstJoin) {
                    AstJoin join = (AstJoin) deref;
                    if (join.getRight() instanceof AstChildRelation) {
                        IrExpr left = compile(join.getLeft());
                        $deref = doJoin(left, ((AstChildRelation) join.getRight()).getChildType());

                        globalCardinality = left instanceof IrSetExpr
                                ? ((IrSetExpr) left).getCard().getHighBound()
                                : 1;
                    } else {
                        $deref = compile(deref);
                        if (derefType instanceof AstConcreteClafer) {
                            globalCardinality = getScope(((AstConcreteClafer) derefType).getParent());
                        }
                    }
                } else {
                    $deref = compile(deref);
                    if (derefType instanceof AstConcreteClafer) {
                        globalCardinality = getScope(((AstConcreteClafer) derefType).getParent());
                    }
                }
            } else {
                $deref = compile(deref);
            }

            AstRef ref = derefType.getRef();
            if ($deref instanceof IrIntExpr) {
                IrIntExpr $intDeref = (IrIntExpr) $deref;
                if (ref.getTargetType() instanceof AstStringClafer) {
                    // Why empty string? The "take" var can contain unused.
                    return element(Util.snoc(refStrings.get(ref), EmptyString), $intDeref);
                } else {
                    // Why zero? The "take" var can contain unused.
                    return element(Util.snoc(refPointers.get(ref), Zero), $intDeref);
                }
            } else if ($deref instanceof IrSetExpr) {
                IrSetExpr $setDeref = (IrSetExpr) $deref;
                if (ref.getTargetType() instanceof AstStringClafer) {
                    if ($setDeref.getCard().getHighBound() == 1) {
                        IrStringExpr[] refs = Util.snoc(refStrings.get(ref), EmptyString);
                        return element(refs, max($setDeref, refs.length - 1));
                    }
                    throw new JoinSetWithStringException(ast, $setDeref.getCard());
                }
                // Why zero? The "take" var can contain unused.
                return joinFunction($setDeref, Util.snoc(refPointers.get(ref), Zero), globalCardinality);
            }
            throw new AstException();
        }

        @Override
        public IrExpr visit(AstCard ast, Void a) {
            IrExpr set = compile(ast.getSet());
            if (set instanceof IrIntExpr) {
                return One;
            } else if (set instanceof IrSetExpr) {
                return card((IrSetExpr) set);
            } else if (set instanceof IrIntArrayExpr) {
                IrIntArrayExpr array = (IrIntArrayExpr) set;
                Type type = getType(ast.getSet());
                assert type.getCommonSupertype().arity() == 2;
                return countNotEqual(getUninitalizedRef(type.getCommonSupertype().get(1)), array);
            }
            IrSetArrayExpr array = ((IrSetArrayExpr) set);
            IrIntExpr[] cards = new IrIntExpr[array.length()];
            for (int i = 0; i < cards.length; i++) {
                cards[i] = card(get(array, i));
            }
            return add(cards);
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
                IrIntExpr intLeft = (IrIntExpr) left;
                IrIntExpr intRight = (IrIntExpr) right;
                switch (ast.getOp()) {
                    case Equal:
                        return equal(intLeft, intRight);
                    case NotEqual:
                        return notEqual(intLeft, intRight);
                }
            }

            if (left instanceof IrStringExpr && right instanceof IrStringExpr) {
                IrStringExpr stringLeft = (IrStringExpr) left;
                IrStringExpr stringRight = (IrStringExpr) right;
                switch (ast.getOp()) {
                    case Equal:
                        return equal(stringLeft, stringRight);
                    case NotEqual:
                        return notEqual(stringLeft, stringRight);
                }
            }

            if (left instanceof IrIntArrayExpr) {
                IrIntArrayExpr leftArray = (IrIntArrayExpr) left;
                if (right instanceof IrIntArrayExpr) {
                    IrIntArrayExpr rightArray = (IrIntArrayExpr) right;
                    return equal(IrUtil.asArray(leftArray), IrUtil.asArray(rightArray));
                } else if (right instanceof IrSetArrayExpr) {
                    Type type = getType(ast.getLeft());
                    AstClafer returnType = type.getCommonSupertype().get(1);
                    IrSetArrayExpr rightArray = (IrSetArrayExpr) right;
                    return equal(IrUtil.asArray(filterNotEqual(leftArray, getUninitalizedRef(returnType))), IrUtil.asArray(rightArray));
                }
            } else if (left instanceof IrSetArrayExpr) {
                IrSetArrayExpr leftArray = (IrSetArrayExpr) left;
                if (right instanceof IrIntArrayExpr) {
                    Type type = getType(ast.getRight());
                    AstClafer returnType = type.getCommonSupertype().get(1);
                    IrIntArrayExpr rightArray = (IrIntArrayExpr) right;
                    return equal(IrUtil.asArray(leftArray), IrUtil.asArray(filterNotEqual(rightArray, getUninitalizedRef(returnType))));
                } else if (right instanceof IrSetArrayExpr) {
                    IrSetArrayExpr rightArray = (IrSetArrayExpr) right;
                    return equal(IrUtil.asArray(leftArray), IrUtil.asArray(rightArray));
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
                    return mul(operands, getMulRange());
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
        public IrExpr visit(AstMod ast, Void a) {
            return mod(asInt(compile(ast.getDividend())), asInt(compile(ast.getDivisor())));
        }

        private IrIntExpr concatRefs(AstSetExpr set, Monoid<IrIntExpr> monoid) {
            AstClafer setType = getCommonSupertype(set).getClaferType();
            assert setType.hasRef();
            IrIntVar[] refs = refPointers.get(setType.getRef());
            String name = monoid.getClass().getSimpleName();
            int count = concatRefsCount++;

            IrBoolExpr[] members;
            if (set instanceof AstGlobal) {
                members = memberships.get(setType);
            } else {
                IrExpr $set = compile(set);
                if ($set instanceof IrIntExpr) {
                    IrIntExpr intSet = (IrIntExpr) $set;
                    return element(refs, intSet);
                }
                IrSetExpr setSet = (IrSetExpr) $set;
                if (setSet.getEnv().isEmpty()) {
                    return monoid.empty();
                }
                assert setSet.getEnv().getLowBound() >= 0;
                members = new IrBoolExpr[setSet.getEnv().getHighBound() + 1];
                for (int i = 0; i < members.length; i++) {
                    members[i] = bool(name + "Member" + count + "@" + i);
                }
                module.addConstraint(boolChannel(members, setSet));
            }
            assert members.length <= refs.length;

            IrIntVar[] score = new IrIntVar[members.length];
            for (int i = 0; i < members.length; i++) {
                Domain domain = refs[i].getDomain();
                int uninitializedRef = getUninitalizedRef(setType.getRef().getTargetType());
                IrIntExpr identity = monoid.empty();
                domain = domain.remove(uninitializedRef).union(identity.getDomain());
                score[i] = domainInt(name + count + "@" + i, domain);
                module.addConstraint(ifThenElse(members[i],
                        equal(score[i], refs[i]), equal(score[i], identity)));
            }
            return monoid.concat(score);
        }

        @Override
        public IrExpr visit(AstSum ast, Void a) {
            return concatRefs(ast.getSet(), Sum.Singleton);
        }

        @Override
        public IrExpr visit(AstProduct ast, Void a) {
            return concatRefs(ast.getSet(), new Product(getMulRange()));
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
        public IrExpr visit(AstDifference ast, Void a) {
            return difference(
                    asSet(compile(ast.getLeft())),
                    asSet(compile(ast.getRight())));
        }

        @Override
        public IrExpr visit(AstIntersection ast, Void a) {
            return intersection(
                    asSet(compile(ast.getLeft())),
                    asSet(compile(ast.getRight())));
        }

        @Override
        public IrExpr visit(AstUnion ast, Void a) {
            IrExpr left = compile(ast.getLeft());
            IrExpr right = compile(ast.getRight());

            if (getCommonSupertype(ast).arity() == 1) {
                return union(asSet(left), asSet(right));
            }

            IrSetExpr[] leftArray = IrUtil.asArray(asRelation(left, getCommonSupertype(ast.getLeft())));
            IrSetExpr[] rightArray = IrUtil.asArray(asRelation(right, getCommonSupertype(ast.getLeft())));
            assert leftArray.length == rightArray.length;
            IrSetExpr[] union = new IrSetExpr[leftArray.length];
            for (int i = 0; i < union.length; i++) {
                union[i] = union(leftArray[i], rightArray[i]);
            }
            return array(union);
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
        public IrExpr visit(AstDowncast ast, Void a) {
            AstSetExpr base = ast.getBase();
            int offset = getOffset((AstAbstractClafer) getCommonSupertype(base).getClaferType(), ast.getTarget().getClaferType());

            IrExpr $base = compile(ast.getBase());
            if ($base instanceof IrIntExpr) {
                IrIntExpr intBase = (IrIntExpr) $base;
                return sub(intBase, constant(offset));
            }
            return mask((IrSetExpr) $base, offset, offset + getScope(ast.getTarget().getClaferType()));
        }

        @Override
        public IrExpr visit(AstUpcast ast, Void a) {
            IrExpr base = compile(ast.getBase());
            if (base instanceof IrIntExpr) {
                int offset = getOffset((AstAbstractClafer) ast.getTarget().getClaferType(), getCommonSupertype(ast.getBase()).getClaferType());
                return add((IrIntExpr) base, constant(offset));
            } else if (base instanceof IrSetExpr) {
                int offset = getOffset((AstAbstractClafer) ast.getTarget().getClaferType(), getCommonSupertype(ast.getBase()).getClaferType());
                return offset((IrSetExpr) base, offset);
            } else if (base instanceof IrIntArrayExpr) {
                int paramOffset = getOffset(ast.getTarget().get(0), getCommonSupertype(ast.getBase()).get(0));
                int returnOffset = getOffset(ast.getTarget().get(1), getCommonSupertype(ast.getBase()).get(1));
                int scope = getScope(ast.getTarget().get(0));
                int uninitializedRef = getUninitalizedRef(ast.getTarget().get(1));
                IrIntArrayExpr baseArray = (IrIntArrayExpr) base;
                IrIntExpr[] array = new IrIntExpr[scope];
                Arrays.fill(array, constant(uninitializedRef));
                for (int i = 0; i < baseArray.length(); i++) {
                    array[i + paramOffset] = add(get(baseArray, i), returnOffset);
                }
                return array(array);
            } else if (base instanceof IrSetArrayExpr) {
                int paramOffset = getOffset(ast.getTarget().get(0), getCommonSupertype(ast.getBase()).get(0));
                int returnOffset = getOffset(ast.getTarget().get(1), getCommonSupertype(ast.getBase()).get(1));
                int scope = getScope(ast.getTarget().get(0));
                IrSetArrayExpr baseArray = (IrSetArrayExpr) base;
                IrSetExpr[] array = new IrSetExpr[scope];
                Arrays.fill(array, EmptySet);
                for (int i = 0; i < baseArray.length(); i++) {
                    array[i + paramOffset] = offset(get(baseArray, i), returnOffset);
                }
                return array(array);
            }
            throw new AstException();
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
                    labeledPermutation[i] = new Triple<>(
                            decl.getLocals()[i], intBody, True);
                }
                @SuppressWarnings("unchecked")
                Triple<AstLocal, IrIntExpr, IrBoolExpr>[][] labeledSequence = new Triple[][]{labeledPermutation};
                return labeledSequence;
            }
            if (body instanceof IrSetExpr) {
                IrSetExpr setBody = (IrSetExpr) body;
                Domain env = setBody.getEnv();
                Domain ker = setBody.getKer();
                // TODO: need a different strategy otherwise
                assert env.getLowBound() >= 0;
                @SuppressWarnings("unchecked")
                Pair<IrIntExpr, IrBoolExpr>[] members = new Pair[env.getHighBound() + 1];
                for (int i = 0; i < env.getLowBound(); i++) {
                    members[i] = new Pair<>(constant(i), False);
                }
                for (int i = env.getLowBound(); i <= env.getHighBound(); i++) {
                    members[i] = new Pair<>(constant(i),
                            ker.contains(i) ? True
                                    : bool(Util.intercalate("/", AstUtil.getNames(decl.getLocals())) + "#" + i + "#" + localCount++));
                }
                module.addConstraint(boolChannel(Pair.mapSnd(members), setBody));
                if (decl.isDisjoint() && members.length < decl.getLocals().length) {
                    return null;
                }
                Pair<IrIntExpr, IrBoolExpr>[][] sequence = decl.isDisjoint() ? Util.permutations(members,
                        decl.getLocals().length) : Util.sequence(members, decl.getLocals().length);

                @SuppressWarnings("unchecked")
                Triple<AstLocal, IrIntExpr, IrBoolExpr>[][] labeledSequence = new Triple[sequence.length][];
                for (int i = 0; i < labeledSequence.length; i++) {
                    Pair<IrIntExpr, IrBoolExpr>[] permutation = sequence[i];
                    @SuppressWarnings("unchecked")
                    Triple<AstLocal, IrIntExpr, IrBoolExpr>[] labeledPermutation = new Triple[permutation.length];
                    for (int j = 0; j < labeledPermutation.length; j++) {
                        labeledPermutation[j] = new Triple<>(
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
                if (compiledDecls[i] == null) {
                    // UNSAT
                    return False;
                }
            }
            compiledDecls = Util.sequence(compiledDecls);

            List<IrBoolExpr> compiled = new ArrayList<>();
            for (Triple<AstLocal, IrIntExpr, IrBoolExpr>[][] quants : compiledDecls) {
                List<IrBoolExpr> constraints = new ArrayList<>();
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

        @Override
        public IrExpr visit(AstLength ast, Void a) {
            return length(asString(compile(ast.getString())));
        }

        @Override
        public IrExpr visit(AstConcat ast, Void a) {
            return concat(asString(compile(ast.getLeft())),
                    asString(compile(ast.getRight())));
        }

        @Override
        public IrExpr visit(AstPrefix ast, Void a) {
            return prefix(asString(compile(ast.getPrefix())),
                    asString(compile(ast.getWord())));
        }

        @Override
        public IrExpr visit(AstSuffix ast, Void a) {
            return suffix(asString(compile(ast.getSuffix())),
                    asString(compile(ast.getWord())));
        }

        @Override
        public IrExpr visit(AstChildRelation ast, Void a) {
            return array(siblingSets.get(ast.getChildType()));
        }

        @Override
        public IrExpr visit(AstParentRelation ast, Void a) {
            return array(parentPointers.get(ast.getParentRelation()));
        }

        @Override
        public IrExpr visit(AstRefRelation ast, Void a) {
            return array(refPointers.get(ast.getRef()));
        }

        @Override
        public IrExpr visit(AstInverse ast, Void a) {
            Type type = getType(ast.getRelation());
            AstClafer returnType = type.getCommonSupertype().get(1);
            IrExpr relation = compile(ast.getRelation());
            if (relation instanceof IrIntArrayExpr) {
                return inverse(filterNotEqual((IrIntArrayExpr) relation, getUninitalizedRef(returnType)), getScope(returnType));
            }
            if (relation instanceof IrSetArrayExpr) {
                return inverse((IrSetArrayExpr) relation, getScope(returnType));
            }
            // Bug.
            throw new AstException("Should not have passed type checking.");
        }

        @Override
        public IrExpr visit(AstTransitiveClosure ast, Void a) {
            IrExpr relation = compile(ast.getRelation());
            if (relation instanceof IrIntArrayExpr) {
                Type type = getType(ast.getRelation());
                AstClafer returnType = type.getCommonSupertype().get(1);
                return transitiveClosure(filterNotEqual((IrIntArrayExpr) relation, getUninitalizedRef(returnType)), ast.isReflexive());
            }
            if (relation instanceof IrSetArrayExpr) {
                return transitiveClosure((IrSetArrayExpr) relation, ast.isReflexive());
            }
            // Bug.
            throw new AstException("Should not have passed type checking.");
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
                Domain env = boundDomain(low, Math.min(high - 1, max));
                Domain ker = EmptyDomain;
                if (partialParentSolution.hasClafer(i)) {
                    int prevHigh = high - card.getHigh();
                    int nextLow = low + card.getLow();
                    if (nextLow > prevHigh) {
                        ker = boundDomain(prevHigh, Math.min(nextLow - 1, max));
                    }
                }
                int cardLow = Math.max(card.getLow(), ker.size());
                int cardHigh = Math.min(card.getHigh(), env.size());
                Domain cardDomain = cardLow <= cardHigh
                        ? boundDomain(cardLow, cardHigh)
                        // Cannot exist
                        : EmptyDomain;
                cardDomain = partialParentSolution.hasClafer(i) ? cardDomain : cardDomain.insert(0);
                skip[i] = set(clafer.getName() + "#" + i, env, ker, cardDomain);
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

    private void buildRef(AstClafer clafer) {
        if (clafer.hasRef()) {
            AstRef ref = clafer.getRef();
            if (ref.getTargetType() instanceof AstStringClafer) {
                refStrings.put(ref, buildStrings(ref));
            } else {
                refPointers.put(ref, buildRefPointers(ref));
            }
        }
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

        assert !(tar instanceof AstStringClafer);

        PartialSolution partialSolution = getPartialSolution(src);
        Domain[] partialInts = getPartialInts(ref);
        IrIntVar[] ivs = new IrIntVar[getScope(src)];
        for (int i = 0; i < ivs.length; i++) {
            if (partialSolution.hasClafer(i)) {
                ivs[i] = domainInt(src.getName() + "@Ref" + i, partialInts[i]);
            } else {
                ivs[i] = domainInt(src.getName() + "@Ref" + i, partialInts[i].insert(getUninitalizedRef(tar)));
            }
        }

        return ivs;
    }

    private IrStringVar[] buildStrings(AstRef ref) {
        AstClafer src = ref.getSourceType();
        AstClafer tar = ref.getTargetType();

        assert tar instanceof AstStringClafer;

        int stringLength = analysis.getScope().getStringLength();
        char charLow = analysis.getScope().getCharLow();
        char charHigh = analysis.getScope().getCharHigh();
        Domain charDomain = boundDomain(charLow, charHigh).insert(0);

        IrStringVar[] svs = new IrStringVar[getScope(src)];
        for (int i = 0; i < svs.length; i++) {
            IrIntVar[] chars = new IrIntVar[stringLength];
            for (int j = 0; j < chars.length; j++) {
                chars[j] = domainInt(src.getName() + "@String" + i + "[" + j + "]", charDomain);
            }
            svs[i] = string(src.getName(), chars,
                    boundInt(src.getName() + "@Length" + i, 0, stringLength));
        }

        return svs;
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
        return True;
    }

    /*
     ************************
     * Convenience functions.
     ************************
     */
    private int getUninitalizedRef(AstClafer clafer) {
        return clafer instanceof AstIntClafer
                ? analysis.getScope().getIntHigh() + 1
                : getScope(clafer);
    }

    private int getScope(AstClafer clafer) {
        return analysis.getScope().getScope(clafer);
    }

    private Domain getIntRange() {
        Scope scope = analysis.getScope();
        return Domains.boundDomain(scope.getIntLow(), scope.getIntHigh());
    }

    private Domain getMulRange() {
        Scope scope = analysis.getScope();
        return Domains.boundDomain(scope.getMulLow(), scope.getMulHigh());
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

    private Domain[] getPartialInts(AstRef ref) {
        return analysis.getPartialInts(ref);
    }

    private int getOffset(AstClafer sup, AstClafer sub) {
        if (sup instanceof AstConcreteClafer) {
            assert sup.equals(sub);
            return 0;
        }
        return getOffset((AstAbstractClafer) sup, sub);
    }

    private int getOffset(AstAbstractClafer sup, AstClafer sub) {
        int offset = 0;
        for (AstClafer cur = sub; !sup.equals(cur); cur = cur.getSuperClafer()) {
            if (!cur.hasSuperClafer()) {
                throw new AstException(sub + " is not a sub clafer of " + sup);
            }
            offset += analysis.getOffsets(cur.getSuperClafer()).getOffset(cur);
        }
        return offset;
    }

    private Card getCard(AstConcreteClafer clafer) {
        return analysis.getCard(clafer);
    }

    private Card getGlobalCard(AstClafer clafer) {
        return analysis.getGlobalCard(clafer);
    }

    private Type getType(AstExpr expr) {
        return analysis.getType(expr);
    }

    private ProductType getCommonSupertype(AstExpr expr) {
        return analysis.getCommonSupertype(expr);
    }

    private static interface Symmetry {

        IrExpr[] getInput();

        IrExpr[] getOutput();

        IrBoolExpr getConstraint();
    }

    private static class FilterString implements Symmetry {

        private final IrSetExpr set;
        private final IrIntExpr[] string;
        private final IrIntExpr[] result;

        FilterString(IrSetExpr set, IrIntExpr[] string, IrIntExpr[] result) {
            this.set = set;
            this.string = string;
            this.result = result;
        }

        @Override
        public IrExpr[] getInput() {
            IrExpr[] input = new IrExpr[string.length + 1];
            input[0] = set;
            System.arraycopy(string, 0, input, 1, string.length);
            return input;
        }

        @Override
        public IrExpr[] getOutput() {
            return result;
        }

        @Override
        public IrBoolExpr getConstraint() {
            return filterString(set, string, result);
        }
    }

    private static class LexChainChannel implements Symmetry {

        private final IrIntExpr[][] strings;
        private final IrIntExpr[] ints;

        LexChainChannel(IrIntExpr[][] strings, IrIntExpr[] ints) {
            this.strings = strings;
            this.ints = ints;
        }

        @Override
        public IrExpr[] getInput() {
            return Util.concat(strings);
        }

        @Override
        public IrExpr[] getOutput() {
            return ints;
        }

        @Override
        public IrBoolExpr getConstraint() {
            return sortChannel(strings, ints);
        }
    }
}
