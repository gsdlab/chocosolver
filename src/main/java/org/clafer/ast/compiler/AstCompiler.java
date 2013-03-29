package org.clafer.ast.compiler;

import org.clafer.ast.AstExpr;
import java.util.Map;
import org.clafer.ast.AstSetExpr;
import org.clafer.ast.AstCard;
import org.clafer.ast.AstCompare;
import org.clafer.ast.AstConstant;
import org.clafer.ast.AstJoin;
import org.clafer.ast.AstJoinParent;
import org.clafer.ast.AstJoinRef;
import org.clafer.ast.AstLocal;
import org.clafer.ast.AstNone;
import org.clafer.ast.AstQuantify;
import org.clafer.ast.AstThis;
import org.clafer.ast.AstUpcast;
import org.clafer.ir.IrExpr;
import org.clafer.compiler.ClaferCompiler;
import org.clafer.ast.Asts;
import org.clafer.compiler.ClaferSolver;
import org.clafer.collection.ReadWriteHashMap;
import java.util.Arrays;
import org.clafer.ir.IrIntVar;
import org.clafer.ir.IrSetExpr;
import org.clafer.Util;
import org.clafer.ir.IrBoolVar;
import java.util.ArrayList;
import java.util.List;
import org.clafer.Check;
import org.clafer.Scope;
import org.clafer.analysis.Analysis;
import org.clafer.analysis.AnalysisUtil;
import org.clafer.analysis.FormatAnalysis.Format;
import org.clafer.analysis.PartialSolutionAnalysis.PartialSolution;
import org.clafer.analysis.TypeAnalysis;
import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstBoolExpr;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstException;
import org.clafer.ast.AstExprVisitor;
import org.clafer.ast.AstIntClafer;
import org.clafer.ast.AstModel;
import org.clafer.ast.AstRef;
import org.clafer.ast.Card;
import org.clafer.ir.IrBoolExpr;
import org.clafer.ir.IrIntExpr;
import org.clafer.ir.IrModule;
import org.clafer.ir.IrSetVar;
import static org.clafer.ir.Irs.*;

/**
 * Compile from AST -> IR
 * 
 * @author jimmy
 */
public class AstCompiler {

    public static void main(String[] args) {
        AstModel model = Asts.newModel();
        AstConcreteClafer person = model.addTopClafer("person");
        AstConcreteClafer name = person.addChild("name").withCard(1, 1).refTo(Asts.IntType);
        person.addConstraint(Asts.equal(Asts.joinRef(Asts.join(Asts.$this(), name)), Asts.constant(1)));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.builder().defaultScope(5).intLow(-1).intHigh(1).toScope());

        while (solver.find()) {
            System.out.println(solver.instance());
        }
        System.out.println(solver.getMeasures().getSolutionCount());
    }
    private final AstModel model;
    private final Analysis analysis;
    private final IrModule module;

    private AstCompiler(AstModel model, Scope scope, IrModule module) {
        this.model = Check.notNull(model);
        this.analysis = Analysis.analyze(model, scope);
        this.module = Check.notNull(module);
    }

    public static AstSolutionMap compile(AstModel in, Scope scope, IrModule out) {
        AstCompiler compiler = new AstCompiler(in, scope, out);
        return compiler.compile();
    }

    private AstSolutionMap compile() {
        List<AstAbstractClafer> abstractClafers = model.getAbstractClafers();
        List<AstConcreteClafer> concreteClafers = AnalysisUtil.getConcreteClafers(model);
        List<AstClafer> clafers = new ArrayList<AstClafer>(abstractClafers.size() + concreteClafers.size());
        clafers.addAll(abstractClafers);
        clafers.addAll(concreteClafers);

        for (AstConcreteClafer clafer : concreteClafers) {
            initConcrete(clafer);
        }
        for (AstConcreteClafer clafer : concreteClafers) {
            if (Format.LowGroup.equals(getFormat(clafer))) {
                initLowGroupConcrete(clafer);
            }
        }
        for (AstConcreteClafer clafer : concreteClafers) {
            if (Format.ParentGroup.equals(getFormat(clafer))) {
                initParentGroupConcrete(clafer);
            }
        }
        for (AstAbstractClafer clafer : abstractClafers) {
            initAbstract(clafer);
        }
        for (AstConcreteClafer clafer : concreteClafers) {
            constrainConcrete(clafer);
        }
        for (AstConcreteClafer clafer : concreteClafers) {
            if (Format.LowGroup.equals(getFormat(clafer))) {
                constrainLowGroupConcrete(clafer);
            }
        }
        for (AstConcreteClafer clafer : concreteClafers) {
            if (Format.ParentGroup.equals(getFormat(clafer))) {
                constrainParentGroupConcrete(clafer);
            }
        }
        for (AstAbstractClafer clafer : abstractClafers) {
            constrainAbstract(clafer);
        }

        for (AstClafer clafer : clafers) {
            int scope = getScope(clafer);
            for (int i = 0; i < scope; i++) {
                ExpressionCompiler expressionCompiler = new ExpressionCompiler(clafer, i);
                for (AstBoolExpr constraint : clafer.getConstraints()) {
                    IrBoolExpr thisConstraint = (IrBoolExpr) constraint.accept(expressionCompiler, null);
                    module.addConstraint(implies(membership.get(clafer)[i], thisConstraint));
                }
            }
        }

        for (IrSetVar[] childSet : childrenSet.getValues()) {
            module.addSetVars(childSet);
        }
        for (IrIntVar[] refs : refPointers.getValues()) {
            module.addIntVars(refs);
        }
        return new AstSolutionMap(model, childrenSet, refPointers, analysis);
    }

    private void initConcrete(AstConcreteClafer clafer) {
        if (clafer.hasParent()) {
            parentPointers.put(clafer, buildParentPointers(clafer));
        }
        if (clafer.hasRef()) {
            refPointers.put(clafer.getRef(), buildRefPointers(clafer.getRef()));
        }
    }

    private void constrainConcrete(AstConcreteClafer clafer) {
        if (clafer.hasParent()) {
            IrIntVar[] parents = parentPointers.get(clafer);
            Card globalCard = getGlobalCard(clafer);
            if (globalCard.isExact()) {
                // No unused
                module.addConstraint(intChannel(parents, childrenSet.get(clafer)));
            } else {
                IrSetVar unused = set(clafer.getName() + "@Unused", getPartialSolution(clafer).getUnknownClafers());
                module.addConstraint(intChannel(parents, Util.cons(childrenSet.get(clafer), unused)));
            }
        }
        if (clafer.hasRef()) {
            AstRef ref = clafer.getRef();
            IrIntVar[] refs = refPointers.get(ref);
            if (ref.isUnique() && clafer.getCard().getHigh() > 1) {
                if (!clafer.hasParent()) {
                    if (clafer.getCard().isExact()) {
                        assert clafer.getCard().getLow() == refs.length;
                        module.addConstraint(allDifferent(refs));
                    } else {
                        IrBoolVar[] members = membership.get(clafer);
                        for (int i = 0; i < refs.length; i++) {
                            for (int j = i + 1; j < refs.length; j++) {
                                module.addConstraint(
                                        implies(and(members[i], members[j]), notEqual(refs[i], refs[j])));
                            }
                        }
                    }
                } else {
                    IrIntVar[] parents = parentPointers.get(clafer);
                    int unused = getScope(clafer.getParent());
                    for (int i = 0; i < refs.length; i++) {
                        for (int j = i + 1; j < refs.length; j++) {
                            module.addConstraint(
                                    implies(and(notEqual(parents[i], unused), equal(parents[i], parents[j])),
                                    notEqual(refs[i], refs[j])));
                        }
                    }
                }
            } else {
                IrBoolVar[] members = membership.get(clafer);
                assert refs.length == members.length;
                for (int i = 0; i < members.length; i++) {
                    module.addConstraint(implies(not(members[i]), equal(refs[i], 0)));
                }
            }
        }

        /**
         * What is this optimization?
         * 
         * This optimization is to remove isomorphic solutions due to swapping of children.
         * 
         * Consider the following Clafer model.
         * 
         *   Diner 2
         *     Burger 1..*
         * 
         * Let the scope be {Diner=2, Food=3}. What this says is that we have 2 Diners and 3
         * Burgers but each Diner gets at least one serving. Logically, there are two solutions:
         * 
         *   1. Each Diner gets 1 Burger each (the last Burger is unused)
         *   2. One Diner gets 2 Burgers and the other Diner gets 1 Burger
         * 
         * There are two unique solutions, other isomorphic solutions may arise from the solver,
         * but ideally we want to eliminate the isomorphic duplicates. For example, here are
         * two isomorphic instances that will arrise with symmetry breaking:
         * 
         *   Diner0
         *     Burger0
         *     Burger1
         *   Diner1
         *     Burger2
         * 
         *  
         *   Diner0
         *     Burger0
         *   Diner1
         *     Burger1
         *     Burger2
         * 
         * We add the constraint |Diner0.Burger| >= |Diner1.Burger| to break the symmetry.
         * This is how it works when Diner only has one type as a child. This optimization
         * generalizes to multi-children. For example, consider the Clafer model:
         * 
         *   Diner 2
         *     Burger 1..*
         *     Drink 1..*
         * 
         * Let the scope be {Diner=2, Food=3, Drink=4}. First we'll see a wrong generalization.
         * Adding the two constraints DO NOT WORK:
         * 
         *   1. |Diner0.Burger| >= |Diner1.Burger|
         *   2. |Diner0.Drink| >= |Diner1.Drink|
         * 
         * The two constraints above DO NOT WORK because it rules out the case where one Diner has
         * more Burgers but the other Diner has more drinks. Instead, we want tuple comparison like
         * the following constraint (tuple comparision as implemented in Haskell, ie. compare the
         * first indices, then use the second index to break ties, then use the third index to break
         * the next tie, etc.):
         * 
         *   (|Diner0.Burger|, |Diner0.Drink|) >= (|Diner1.Burger|, |Diner1.Drink|)
         * 
         * However, Choco does not implement tuple comparision but it's we can simulate it with
         * the equation constraint since the size of the sets are bounded.
         * 
         *   5 * |Diner0.Burger| + 1 * |Diner0.Drink| >= 5 * |Diner1.Burger| + 1 * |Diner1.Drink|
         * 
         * The "5" is coefficient comes from the fact that scope(Drink) = 4.
         */
        IrIntExpr[][] terms = new IrIntExpr[getScope(clafer)][];
        for (int i = 0; i < terms.length; i++) {
            List<IrIntExpr> string = new ArrayList<IrIntExpr>();
            for (AstConcreteClafer child : clafer.getChildren()) {
                // Children with exact cards will always contribute the same score hence it
                // is a waste of computation time to include them in the scoring constraints.
                if (!child.getCard().isExact()) {
                    string.add(setCard(childrenSet.get(child)[i]));
                }
            }
            AstClafer sup = clafer;
            int offset = 0;
            while (sup.hasSuperClafer()) {
                offset += getOffset(sup.getSuperClafer(), sup);
                for (AstConcreteClafer child : sup.getSuperClafer().getChildren()) {
                    if (!child.getCard().isExact()) {
                        string.add(setCard(childrenSet.get(child)[i + offset]));
                    }
                }
                sup = sup.getSuperClafer();
            }
            if (string.isEmpty()) {
                break;
            }
            terms[i] = string.toArray(new IrIntExpr[string.size()]);
        }
        /*
         * What is this optimization?
         *
         * Reversing is so that earlier children have higher scores. Not necessary,
         * but the solutions will have instances that are similar closer together.
         * Technically not an optimization.
         */
        if (terms[0] != null) {
            Util.reverse(terms);
            module.addConstraint(sort(terms));
        }
    }

    private void initLowGroupConcrete(AstConcreteClafer clafer) {
        PartialSolution partialSolution = getPartialSolution(clafer);

        IrSetVar[] children = skipCards(clafer);
        childrenSet.put(clafer, children);
        set.put(clafer, union(children));

        IrBoolVar[] members = new IrBoolVar[getScope(clafer)];
        for (int i = 0; i < members.length; i++) {
            members[i] = partialSolution.hasClafer(i) ? True : bool(clafer.getName() + "@Membership#" + i);
        }
        membership.put(clafer, members);
    }

    private void constrainLowGroupConcrete(AstConcreteClafer clafer) {
        IrBoolVar[] members = membership.get(clafer);

        if (!clafer.hasParent()) {
            module.addConstraint(selectN(members, setCard(set.get(clafer))));
        }

        PartialSolution partialParentSolution = getPartialParentSolution(clafer);
        Card card = clafer.getCard();

        IrSetVar[] children = childrenSet.get(clafer);
        for (int i = 0; i < partialParentSolution.size(); i++) {
            if (partialParentSolution.hasClafer(i)) {
                module.addConstraint(constrainCard(setCard(children[i]), card));
            } else {
                if (card.isBounded()) {
                    module.addConstraint(implies(membership.get(clafer.getParent())[i],
                            constrainCard(setCard(children[i]), card)));
                }
                module.addConstraint(implies(not(membership.get(clafer.getParent())[i]),
                        equal(children[i], EmptySet)));
            }
        }

        IrSetExpr claferSet = set.get(clafer);

        module.addConstraint(boolChannel(members, claferSet));

        /**
         * What is this optimization?
         * 
         * Force the lower number atoms to choose lower number parents. For example consider
         * the following clafer model:
         * 
         *   Person 2
         *     Hand 2
         * 
         * The constraint forbids the case where Hand0 belongs to Person1 and Hand1 belongs
         * to Person0. Otherwise, the children can swap around creating many isomorphic
         * solutions.
         */
        if (clafer.hasParent()) {
            module.addConstraint(sort(parentPointers.get(clafer)));
        }
    }

    private void initParentGroupConcrete(AstConcreteClafer clafer) {
        PartialSolution partialParentSolution = getPartialParentSolution(clafer);

        IrSetVar[] children = new IrSetVar[partialParentSolution.size()];
        assert clafer.getCard().getLow() == clafer.getCard().getHigh();
        int lowCard = clafer.getCard().getLow();
        for (int i = 0; i < children.length; i++) {
            if (partialParentSolution.hasClafer(i)) {
                children[i] = constant(Util.range(i * lowCard, i * lowCard + lowCard));
            } else {
                children[i] = set(clafer.getName() + "#" + i, Util.range(i * lowCard, i * lowCard + lowCard));
            }
        }

        childrenSet.put(clafer, children);
        set.put(clafer, union(children));

        IrBoolVar[] members = new IrBoolVar[getScope(clafer)];
        if (!clafer.hasParent()) {
            Arrays.fill(members, 0, lowCard, True);
            Arrays.fill(members, lowCard, members.length, False);
        } else {
            IrBoolVar[] parentMembership = membership.get(clafer.getParent());
            if (lowCard == 1) {
                members = parentMembership;
            } else {
                for (int i = 0; i < parentMembership.length; i++) {
                    for (int j = 0; j < lowCard; j++) {
                        members[i * lowCard + j] = parentMembership[i];
                    }
                }
            }
        }
        membership.put(clafer, members);
    }

    private void constrainParentGroupConcrete(AstConcreteClafer clafer) {
        PartialSolution partialParentSolution = getPartialParentSolution(clafer);

        IrSetVar[] children = childrenSet.get(clafer);
        assert clafer.getCard().getLow() == clafer.getCard().getHigh();
        int lowCard = clafer.getCard().getLow();
        for (int i = 0; i < children.length; i++) {
            if (!partialParentSolution.hasClafer(i)) {
                module.addConstraint(implies(membership.get(clafer.getParent())[i],
                        equal(children[i], constant(Util.range(i * lowCard, i * lowCard + lowCard)))));
                module.addConstraint(implies(not(membership.get(clafer.getParent())[i]),
                        equal(children[i], EmptySet)));
            }
        }
    }

    private void initAbstract(AstAbstractClafer clafer) {
        set.put(clafer, set(clafer.getName(), 0, getScope(clafer) - 1));

        IrBoolVar[] members = new IrBoolVar[getScope(clafer)];
        for (AstClafer sub : clafer.getSubs()) {
            IrBoolVar[] subMembers = membership.get(sub);
            int offset = getOffset(clafer, sub);
            for (int i = 0; i < subMembers.length; i++) {
                assert members[offset + i] == null;
                members[offset + i] = Check.notNull(subMembers[i]);
            }
        }
        Check.noNulls(members);
        membership.put(clafer, members);

        if (clafer.hasRef()) {
            refPointers.put(clafer.getRef(), buildRefPointers(clafer.getRef()));
        }
    }

    private void constrainAbstract(AstAbstractClafer clafer) {
        // Do nothing
    }
    private final ReadWriteHashMap<AstClafer, IrSetExpr> set = new ReadWriteHashMap<AstClafer, IrSetExpr>();
    private final ReadWriteHashMap<AstClafer, IrSetVar[]> childrenSet = new ReadWriteHashMap<AstClafer, IrSetVar[]>();
    private final ReadWriteHashMap<AstClafer, IrBoolVar[]> membership = new ReadWriteHashMap<AstClafer, IrBoolVar[]>();
    private final ReadWriteHashMap<AstConcreteClafer, IrIntVar[]> parentPointers = new ReadWriteHashMap<AstConcreteClafer, IrIntVar[]>();
    private final ReadWriteHashMap<AstRef, IrIntVar[]> refPointers = new ReadWriteHashMap<AstRef, IrIntVar[]>();

    private class ExpressionCompiler implements AstExprVisitor<Void, IrExpr> {

        private final int thisId;
        private final Map<AstExpr, AstClafer> types;

        private ExpressionCompiler(AstClafer thisType, int thisId) {
            this.thisId = thisId;
            this.types = TypeAnalysis.analyze(thisType);
        }

        @Override
        public IrExpr visit(AstThis ast, Void a) {
            return constant(thisId);
        }

        @Override
        public IrExpr visit(AstConstant ast, Void a) {
            return constant(ast.getValue());
        }

        @Override
        public IrExpr visit(AstJoin ast, Void a) {
            AstSetExpr left = ast.getLeft();
            AstConcreteClafer right = ast.getRight();

            IrExpr $left = left.accept(this, a);
            if ($left instanceof IrIntExpr) {
                IrIntExpr $intLeft = (IrIntExpr) $left;
                switch (getFormat(right)) {
                    case ParentGroup:
                        assert right.getCard().isExact();
                        int lowCard = right.getCard().getLow();
                        if (lowCard == 1) {
                            return $intLeft;
                        }
                    // fallthrough
                    case LowGroup:
                        return join(singleton($intLeft), childrenSet.get(right));
                }
            } else if ($left instanceof IrSetExpr) {
                IrSetExpr $setLeft = (IrSetExpr) $left;
                return join($setLeft, childrenSet.get(right));
            }
            throw new AstException();
        }

        @Override
        public IrExpr visit(AstJoinParent ast, Void a) {
            AstSetExpr children = ast.getChildren();
            AstConcreteClafer childrenType = (AstConcreteClafer) types.get(children);

            IrExpr $children = children.accept(this, a);
            if ($children instanceof IrIntExpr) {
                IrIntExpr $intChildren = (IrIntExpr) $children;
                switch (getFormat(childrenType)) {
                    case ParentGroup:
                        assert childrenType.getCard().isExact();
                        int lowCard = childrenType.getCard().getLow();
                        return div($intChildren, constant(lowCard));
                    case LowGroup:
                        return element(parentPointers.get(childrenType), $intChildren);
                }
            } else if ($children instanceof IrSetExpr) {
                IrSetExpr $setChildren = (IrSetExpr) $children;
                return joinRef($setChildren, parentPointers.get(childrenType));
            }
            throw new AstException();
        }

        @Override
        public IrExpr visit(AstJoinRef ast, Void a) {
            AstSetExpr deref = ast.getDeref();
            AstClafer derefType = types.get(deref);

            IrExpr $deref = deref.accept(this, a);
            if ($deref instanceof IrIntExpr) {
                IrIntExpr $intDeref = (IrIntExpr) $deref;
                return element(refPointers.get(derefType.getRef()), $intDeref);
            } else if ($deref instanceof IrSetExpr) {
                IrSetExpr $setDeref = (IrSetExpr) $deref;
                return joinRef($setDeref, refPointers.get(derefType.getRef()));
            }
            throw new AstException();
        }

        @Override
        public IrExpr visit(AstCard ast, Void a) {
            throw new UnsupportedOperationException("Not supported yet.");
        }

        @Override
        public IrExpr visit(AstCompare ast, Void a) {
            AstSetExpr left = ast.getLeft();
            AstSetExpr right = ast.getRight();

            IrExpr $left = left.accept(this, a);
            IrExpr $right = right.accept(this, a);

            if ($left instanceof IrIntExpr && $right instanceof IrIntExpr) {
                IrIntExpr $intLeft = (IrIntExpr) $left;
                IrIntExpr $intRight = (IrIntExpr) $right;

                switch (ast.getOp()) {
                    case Equal:
                        return equal($intLeft, $intRight);
                }
            }
            throw new AstException();
        }

        @Override
        public IrExpr visit(AstUpcast ast, Void a) {
            throw new UnsupportedOperationException("Not supported yet.");
        }

        @Override
        public IrExpr visit(AstNone ast, Void a) {
            throw new UnsupportedOperationException("Not supported yet.");
        }

        @Override
        public IrExpr visit(AstLocal ast, Void a) {
            throw new UnsupportedOperationException("Not supported yet.");
        }

        @Override
        public IrExpr visit(AstQuantify ast, Void a) {
            throw new UnsupportedOperationException("Not supported yet.");
        }
    };

    /*************************
     * Optimization functions.
     *************************/
    private IrSetVar[] skipCards(AstConcreteClafer clafer) {
        int parentScope = clafer.hasParent() ? getScope(clafer.getParent()) : 1;
        PartialSolution partialParentSolution = clafer.hasParent() ? getPartialSolution(clafer.getParent()) : PartialSolution.rootSolution();

        int claferScope = getScope(clafer);
        Card card = clafer.getCard();
        assert card.hasHigh();

        int low = 0;
        int high = card.getHigh();
        int max = claferScope - 1;

        IrSetVar[] skip = new IrSetVar[parentScope];
        for (int i = 0; i < skip.length; i++) {
            if (low <= max) {
                skip[i] = set(clafer.getName() + "#" + i, low, Math.min(high - 1, max));
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

    private IrIntVar[] buildParentPointers(AstConcreteClafer clafer) {
        PartialSolution solution = getPartialSolution(clafer);
        IrIntVar[] pointers = new IrIntVar[solution.size()];
        for (int i = 0; i < pointers.length; i++) {
            pointers[i] = enumInt(clafer.getName() + "@Parent#" + i,
                    solution.hasClafer(i)
                    ? solution.getPossibleParents(i)
                    : Util.cons(solution.getPossibleParents(i), getScope(clafer.getParent())));
        }
        return pointers;
    }

    private IrIntVar[] buildRefPointers(AstRef ref) {
        AstClafer src = ref.getSourceType();
        AstClafer tar = ref.getTargetType();

        int[] partialInts = getPartialInts(ref);
        IrIntVar[] ivs = new IrIntVar[getScope(src)];
        for (int i = 0; i < ivs.length; i++) {
            Integer instantiate = analysis.getPartialRefInts(ref, i);
            if (instantiate == null) {
                if (partialInts == null) {
                    ivs[i] = boundInt(src.getName() + "@Ref" + i, getScopeLow(tar), getScopeHigh(tar));
                } else {
                    ivs[i] = enumInt(src.getName() + "@Ref" + i, partialInts);
                }
            } else {
                ivs[i] = enumInt(src.getName() + "@Ref" + i, new int[]{0, instantiate});
            }
        }

        return ivs;
    }

    /*************************
     * Convenience functions.
     *************************/
    int getScopeLow(AstClafer clafer) {
        return clafer instanceof AstIntClafer ? analysis.getScope().getIntLow() : 0;
    }

    int getScopeHigh(AstClafer clafer) {
        return clafer instanceof AstIntClafer ? analysis.getScope().getIntHigh() : getScope(clafer) - 1;
    }

    public int getScope(AstClafer clafer) {
        return analysis.getScope().getScope(clafer);
    }

    public Format getFormat(AstClafer clafer) {
        return analysis.getFormat(clafer);
    }

    public PartialSolution getPartialSolution(AstClafer clafer) {
        return analysis.getPartialSolution(clafer);
    }

    public PartialSolution getPartialParentSolution(AstConcreteClafer clafer) {
        return clafer.hasParent() ? getPartialSolution(clafer.getParent()) : PartialSolution.rootSolution();
    }

    public int[] getPartialInts(AstRef ref) {
        return analysis.getPartialInts(ref);
    }

    public int getOffset(AstAbstractClafer sup, AstClafer sub) {
        return analysis.getOffset(sup, sub);
    }

    public Card getGlobalCard(AstClafer clafer) {
        return analysis.getGlobalCard(clafer);
    }

    public int getDepth(AstAbstractClafer clafer) {
        return analysis.getDepth(clafer);
    }

    private IrBoolExpr constrainCard(IrIntExpr setCard, Card card) {
        if (card.isExact()) {
            return equal(setCard, card.getLow());
        }
        List<IrBoolExpr> exprs = new ArrayList<IrBoolExpr>(2);
        if (card.hasLow()) {
            exprs.add(greaterThanEqual(setCard, card.getLow()));
        }
        if (card.hasHigh()) {
            exprs.add(lessThanEqual(setCard, card.getHigh()));
        }
        return and(exprs);
    }
}
