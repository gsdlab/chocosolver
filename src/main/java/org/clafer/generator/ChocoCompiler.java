//package org.clafer.generator;
//
//import choco.Choco;
//import static org.clafer.ChocoUtil.*;
//import static choco.Choco.*;
//import choco.Options;
//import choco.cp.model.CPModel;
//import choco.cp.solver.CPSolver;
//import choco.kernel.common.logging.ChocoLogging;
//import choco.kernel.model.Model;
//import choco.kernel.model.constraints.Constraint;
//import choco.kernel.model.variables.integer.IntegerExpressionVariable;
//import choco.kernel.model.variables.integer.IntegerVariable;
//import choco.kernel.model.variables.set.SetVariable;
//import choco.kernel.solver.Solver;
//import java.util.ArrayList;
//import java.util.Arrays;
//import java.util.Collections;
//import java.util.HashMap;
//import java.util.Iterator;
//import java.util.List;
//import java.util.Map;
//import org.clafer.Check;
//import org.clafer.ChocoUtil;
//import org.clafer.Scope;
//import org.clafer.ScopeBuilder;
//import org.clafer.Util;
//import org.clafer.analysis.Analysis;
//import org.clafer.analysis.FormatAnalysis.Format;
//import org.clafer.analysis.PartialSolutionAnalysis.PartialSolution;
//import org.clafer.ast.Ast;
//import org.clafer.ast.AstAbstractClafer;
//import org.clafer.ast.AstBoolExpression;
//import org.clafer.ast.AstCard;
//import org.clafer.ast.AstClafer;
//import org.clafer.ast.AstCompare;
//import org.clafer.ast.AstConcreteClafer;
//import org.clafer.ast.AstConstantInt;
//import org.clafer.ast.AstDecl;
//import org.clafer.ast.AstExpressionVisitor;
//import org.clafer.ast.AstIntClafer;
//import org.clafer.ast.AstJoin;
//import org.clafer.ast.AstJoinParent;
//import org.clafer.ast.AstJoinRef;
//import org.clafer.ast.AstLocal;
//import org.clafer.ast.AstModel;
//import org.clafer.ast.AstNone;
//import org.clafer.ast.AstQuantify;
//import org.clafer.ast.AstRef;
//import org.clafer.ast.AstThis;
//import org.clafer.ast.AstUpcast;
//import org.clafer.collection.IntPair;
//import org.clafer.collection.Pair;
//import org.clafer.constraint.BoolChannelManager;
//import org.clafer.constraint.IncreasingManager;
//import org.clafer.constraint.JoinManager;
//import org.clafer.constraint.JoinRefManager;
//import org.clafer.constraint.SelectNManager;
//import org.clafer.constraint.SetLexManager;
//import org.clafer.constraint.SingletonManager;
//import org.clafer.constraint.UniqRefManager;
//import org.clafer.constraint.UpcastManager;
//import org.clafer.constraint.ZeroOutManager;
//import org.clafer.func.FP;
//import org.clafer.func.Func;
//import org.clafer.ast.Card;
//
///**
// *
// * @author jimmy
// */
//public class ChocoCompiler {
//
//    public static void main(String[] args) {
//        AstModel m = Ast.newModel();
////        AstAbstractClafer person = m.addAbstractClafer("person");
////        AstConcreteClafer name = person.addChild("name").withCard(1, 1);
//
//        AstClafer jim = m.addTopClafer("Jimmy").withCard(3, 3);
//        jim.addChild("jam").withCard(1, 1);
//
//        Map<AstClafer, Integer> scopes = new HashMap<AstClafer, Integer>();
//        scopes.put(jim, 100);
////        AstModel m = Ast.newModel();
////
////        AstConcreteClafer jim = m.addTopClafer("Jimmy").withCard(1, 1);
//////        jim.addConstraint(new AstCompare(new AstJoinRef(new AstJoin(new AstThis(), name)), AstCompare.Op.Equal, new AstConstant(3)));
////        AstConcreteClafer food = jim.addChild("food").withCard(2, 3);
////        food.addChild("Hamburger").withCard(2, 3);
////        food.addChild("Milk").withCard(2, 3);
////
////        Map<AstClafer, Integer> scopes = new HashMap<AstClafer, Integer>();
//
//        ChocoCompiler c = new ChocoCompiler(m, new Scope(scopes, 100));
//        c.compile();
//        ChocoLogging.toSearch();
//        {
//            Iterator<SetVariable> iter = c.model.getSetVarIterator();
//            while (iter.hasNext()) {
//                System.out.println(iter.next());
//            }
//        }
//        Iterator<IntegerVariable> iter = c.model.getIntVarIterator();
//        while (iter.hasNext()) {
//            System.out.println(iter.next());
//        }
//
//        Solver s = new CPSolver();
//        s.read(c.model);
//
//        if (s.solve()) {
//            do {
//                System.out.println(c.newPrinter(s).printToString());
//            } while (s.nextSolution());
//        }
//
//        System.out.println(s.runtimeStatistics());
//    }
//    private final Analysis analysis;
//    private final AstModel ast;
//    private final Scope scope;
//    private final Model model = new CPModel();
//
//    ChocoCompiler(AstModel ast, ScopeBuilder scope) {
//        this(ast, scope.toScope());
//    }
//
//    ChocoCompiler(AstModel ast, Scope scope) {
//        this.analysis = Analysis.analyze(ast, scope);
//        this.ast = ast;
//        this.scope = scope;
//    }
//
//    public static ChocoCompiler compiler(AstModel ast, Scope scope) {
//        ChocoCompiler compiler = new ChocoCompiler(ast, scope);
//        compiler.compile();
//        return compiler;
//    }
//
//    public static ChocoCompiler compiler(AstModel ast, Scope scope, AstRef objective) {
//        ChocoCompiler compiler = new ChocoCompiler(ast, scope);
//        compiler.compile();
//        IntegerVariable[] ivs = compiler.refs.get(objective);
//        IntegerExpressionVariable sum = ChocoUtil.sum(ivs);
//        IntegerVariable score;
//        if (sum instanceof IntegerVariable) {
//            score = (IntegerVariable) sum;
//            score.addOption(Options.V_OBJECTIVE);
//        } else {
//            score = makeIntVar("@Objective", sum.getLowB(), sum.getUppB(), Options.V_BOUND, Options.V_OBJECTIVE, Options.V_NO_DECISION);
//            compiler.model.addConstraint(eq(score, sum));
//        }
//        return compiler;
//    }
//
//    public Printer compileTo(Solver solver) {
//        solver.read(model);
//        return newPrinter(solver);
//    }
//
//    Printer newPrinter(Solver solver) {
//        return new Printer(ast, solver, childrenSet, refs, new OffsetSolutionMap(analysis));
//    }
//
//    void compile() {
//        for (LazyCompiler<?, ?> compiler : compilers) {
//            compiler.unlock();
//        }
//        for (AstConcreteClafer topClafer : ast.getTopClafers()) {
//            compile(topClafer);
//        }
//        for (AstAbstractClafer abstractClafer : ast.getAbstractClafers()) {
//            compile(abstractClafer);
//        }
//        for (LazyCompiler<?, ?> compiler : compilers) {
//            compiler.lock();
//        }
//        for (LazyCompiler<?, ?> compiler : compilers) {
//            compiler.finish();
//        }
//    }
//
//    void compile(AstConcreteClafer clafer) {
//        childrenSet.get(clafer);
//        if (getScope(clafer) > 1) {
//            // Break symmetry
//            if (Format.LowGroup.equals(getFormat(clafer))) {
//                // TODO: Don't do this if membership hasn't been created?
//                // Although this constraint is redundant, it lowers the number of backtracks.
//                model.addConstraint(SelectNManager.selectN(membership.get(clafer), setCard.get(clafer)));
//
//                /**
//                 * What is this optimization?
//                 * 
//                 * Force the lower number atoms to choose lower number parents. For example consider
//                 * the following clafer model:
//                 * 
//                 *   Person 2
//                 *     Hand 2
//                 * 
//                 * The constraint forbids the case where Hand0 belongs to Person1 and Hand1 belongs
//                 * to Person0. Otherwise, the children can swap around creating many isomorphic
//                 * solutions.
//                 */
//                if (clafer.hasParent()) {
//                    model.addConstraint(IncreasingManager.increasing(parentPointers.get(clafer)));
//                }
//            }
//
//            /**
//             * What is this optimization?
//             * 
//             * This optimization is to remove isomorphic solutions due to swapping of children.
//             * 
//             * Consider the following Clafer model.
//             * 
//             *   Diner 2
//             *     Burger 1..*
//             * 
//             * Let the scope be {Diner=2, Food=3}. What this says is that we have 2 Diners and 3
//             * Burgers but each Diner gets at least one serving. Logically, there are two solutions:
//             * 
//             *   1. Each Diner gets 1 Burger each (the last Burger is unused)
//             *   2. One Diner gets 2 Burgers and the other Diner gets 1 Burger
//             * 
//             * There are two unique solutions, other isomorphic solutions may arise from the solver,
//             * but ideally we want to eliminate the isomorphic duplicates. For example, here are
//             * two isomorphic instances that will arrise with symmetry breaking:
//             * 
//             *   Diner0
//             *     Burger0
//             *     Burger1
//             *   Diner1
//             *     Burger2
//             * 
//             *  
//             *   Diner0
//             *     Burger0
//             *   Diner1
//             *     Burger1
//             *     Burger2
//             * 
//             * We add the constraint |Diner0.Burger| >= |Diner1.Burger| to break the symmetry.
//             * This is how it works when Diner only has one type as a child. This optimization
//             * generalizes to multi-children. For example, consider the Clafer model:
//             * 
//             *   Diner 2
//             *     Burger 1..*
//             *     Drink 1..*
//             * 
//             * Let the scope be {Diner=2, Food=3, Drink=4}. First we'll see a wrong generalization.
//             * Adding the two constraints DO NOT WORK:
//             * 
//             *   1. |Diner0.Burger| >= |Diner1.Burger|
//             *   2. |Diner0.Drink| >= |Diner1.Drink|
//             * 
//             * The two constraints above DO NOT WORK because it rules out the case where one Diner has
//             * more Burgers but the other Diner has more drinks. Instead, we want tuple comparison like
//             * the following constraint (tuple comparision as implemented in Haskell, ie. compare the
//             * first indices, then use the second index to break ties, then use the third index to break
//             * the next tie, etc.):
//             * 
//             *   (|Diner0.Burger|, |Diner0.Drink|) >= (|Diner1.Burger|, |Diner1.Drink|)
//             * 
//             * However, Choco does not implement tuple comparision but it's we can simulate it with
//             * the equation constraint since the size of the sets are bounded.
//             * 
//             *   5 * |Diner0.Burger| + 1 * |Diner0.Drink| >= 5 * |Diner1.Burger| + 1 * |Diner1.Drink|
//             * 
//             * The "5" is coefficient comes from the fact that scope(Drink) = 4.
//             */
//            List<Term> terms = new ArrayList<Term>();
//            for (AstConcreteClafer child : clafer.getChildren()) {
//                // Children with exact cards will always contribute the same score hence it
//                // is a waste of computation time to include them in the scoring constraints.
//                if (!child.getCard().isExact()) {
//                    terms.add(new Term(child));
//                }
//            }
//            AstClafer sup = clafer;
//            int offset = 0;
//            while (sup.hasSuperClafer()) {
//                offset += getOffset(sup.getSuperClafer(), sup);
//                for (AstConcreteClafer child : sup.getSuperClafer().getChildren()) {
//                    if (!child.getCard().isExact()) {
//                        terms.add(new Term(child, offset, offset + getScope(clafer)));
//                    }
//                }
//                sup = sup.getSuperClafer();
//            }
//            if (!terms.isEmpty()) {
//                /*
//                 * What is this optimization?
//                 *
//                 * Reversing is so that earlier children have higher scores. Not necessary,
//                 * but the solutions will have instances that are similar closer together.
//                 * Technically not an optimization.
//                 */
//                Collections.reverse(terms);
//                model.addConstraint(IncreasingManager.decreasing(score(clafer, model, terms)));
//            }
//        }
//        compile((AstClafer) clafer);
//    }
//
//    void compile(AstClafer clafer) {
//        if (clafer.hasRef()) {
//            compile(clafer.getRef());
//        }
//        if (clafer.hasGroupCard()
//                && clafer.getGroupCard().isBounded()
//                && clafer.hasChildren()) {
//            // build group card
//            int claferScope = getScope(clafer);
//            Card groupCard = clafer.getGroupCard();
//            PartialSolution partialSolution = getPartialSolution(clafer);
//            for (int i = 0; i < claferScope; i++) {
//                IntegerVariable[] cards = new IntegerVariable[clafer.getChildren().size()];
//                for (int j = 0; j < cards.length; j++) {
//                    cards[j] = childrenSet.get(clafer.getChildren().get(j))[i].getCard();
//                }
//                IntegerExpressionVariable cardSum = ChocoUtil.sum(cards);
//
//                if (partialSolution.hasClafer(i) || !groupCard.hasLow()) {
//                    model.addConstraint(ChocoUtil.betweenCard(cardSum, groupCard));
//                } else {
//                    model.addConstraint(Choco.implies(
//                            Choco.eq(membership.get(clafer)[i], 1),
//                            ChocoUtil.betweenCard(cardSum, groupCard)));
//                }
//            }
//        }
//        for (AstConcreteClafer child : clafer.getChildren()) {
//            compile(child);
//        }
//        for (AstBoolExpression constraint : clafer.getConstraints()) {
//            compile(clafer, constraint);
//        }
//    }
//
//    void compile(AstRef ref) {
//        refs.get(ref);
//    }
//
//    void compile(AstClafer clafer, AstBoolExpression constraint) {
//        PartialSolution partialSolution = getPartialSolution(clafer);
//        for (int id = 0; id < partialSolution.size(); id++) {
//            ExpressionCompiler expressionCompiler;
//            if (partialSolution.hasClafer(id)) {
//                // We know these ids exist so always optimize.
//                expressionCompiler = new ExpressionCompiler(new IntVarExpr(clafer, constant(id)));
//                BoolVarExpr boolVar = (BoolVarExpr) constraint.accept(expressionCompiler, null);
//                model.addConstraint(boolVar.getValue());
//            } else {
//                expressionCompiler = new ExpressionCompiler(new IntVarExpr(clafer, constant(id)));
//                BoolVarExpr boolVar = (BoolVarExpr) constraint.accept(expressionCompiler, null);
//                if (!expressionCompiler.getGlobalConstraints().isEmpty()) {
//                    // Unoptimize
//                    expressionCompiler = new ExpressionCompiler(
//                            new SetVarExpr(clafer, thisSet.get(new Pair<AstClafer, Integer>(clafer, id))));
//                    boolVar = (BoolVarExpr) constraint.accept(expressionCompiler, null);
//                }
//                model.addConstraint(implies(eq(membership.get(clafer)[id], 1), boolVar.getValue()));
//            }
//            for (Constraint globalConstraint : expressionCompiler.getGlobalConstraints()) {
//                model.addConstraint(globalConstraint);
//            }
//        }
//    }
//    /**
//     * Lazy compilers
//     */
//    private final LazyCompiler<AstClafer, SetVariable> set = new LazyCompiler<AstClafer, SetVariable>() {
//
//        @Override
//        SetVariable compile(AstClafer a) {
//            if (a instanceof AstConcreteClafer) {
//                AstConcreteClafer c = (AstConcreteClafer) a;
//                SetVariable[] children = childrenSet.get(c);
//                if (!c.hasParent() || getScope(c.getParent()) == 1) {
//                    return children[0];
//                }
//                SetVariable sv = makeSetVar(a.getName(), 0, getScope(a) - 1);
//                // Because they are disjoint
//                model.addConstraint(eq(ChocoUtil.sum(FP.mapped(children, FP.getCard)), sv.getCard()));
//                model.addConstraint(setUnion(children, sv));
//                return sv;
//            }
//            SetVariable sv = makeSetVar(a.getName(), 0, getScope(a) - 1);
//            model.addVariable(sv);
//            return sv;
//        }
//    };
//    /**
//     * The set of clafers of a given type.
//     */
//    private final LazyCompiler<AstClafer, IntegerVariable> setCard = new LazyCompiler<AstClafer, IntegerVariable>() {
//
//        @Override
//        IntegerVariable compile(AstClafer a) {
//            return set.get(a).getCard();
//        }
//    };
//    /**
//     * Channeled sets.
//     */
//    private final LazyCompiler<AstClafer, IntegerVariable[]> membership = new LazyCompiler<AstClafer, IntegerVariable[]>() {
//
//        @Override
//        IntegerVariable[] compile(AstClafer a) {
//            IntegerVariable[] ivs = new IntegerVariable[getScope(a)];
//            if (a instanceof AstAbstractClafer) {
//                AstAbstractClafer sup = (AstAbstractClafer) a;
//                for (AstClafer sub : sup.getSubs()) {
//                    IntegerVariable[] subMembership = membership.get(sub);
//                    int offset = getOffset(sup, sub);
//                    for (int i = 0; i < subMembership.length; i++) {
//                        assert ivs[offset + i] == null;
//                        // TODO: remove check if compiler will ensure not null in future.
//                        ivs[offset + i] = Check.notNull(subMembership[i]);
//                    }
//                }
//                return ivs;
//            }
//            PartialSolution partialSolution = getPartialSolution(a);
//            switch (getFormat(a)) {
//                case LowGroup:
//                    for (int i = 0; i < ivs.length; i++) {
//                        ivs[i] = partialSolution.hasClafer(i)
//                                ? ONE
//                                : makeBooleanVar(a.getName() + "@Membership#" + i);
//                    }
//                    model.addConstraint(BoolChannelManager.boolChannel(ivs, set.get(a)));
//                    return ivs;
//                case ParentGroup:
//                    AstConcreteClafer concrete = (AstConcreteClafer) a;
//                    int lowCard = concrete.getCard().getLow();
//                    if (!concrete.hasParent()) {
//                        Arrays.fill(ivs, 0, lowCard, ONE);
//                        Arrays.fill(ivs, lowCard, ivs.length, ZERO);
//                        return ivs;
//                    }
//                    IntegerVariable[] parentMembership = membership.get(concrete.getParent());
//                    ;
//                    if (lowCard == 1) {
//                        return parentMembership;
//                    }
//                    for (int i = 0; i < parentMembership.length; i++) {
//                        for (int j = 0; j < lowCard; j++) {
//                            ivs[i * lowCard + j] = parentMembership[i];
//                        }
//                    }
//                    return ivs;
//                default:
//                    throw new CompilerException();
//            }
//        }
//    };
//    /**
//     * parent_i has childSet[i] as its set of children. Guaranteed to be disjoint.
//     */
//    private final LazyCompiler<AstConcreteClafer, SetVariable[]> childrenSet = new LazyCompiler<AstConcreteClafer, SetVariable[]>() {
//
//        @Override
//        SetVariable[] compile(AstConcreteClafer a) {
//            PartialSolution partialParentSolution = getPartialParentSolution(a);
//            switch (getFormat(a)) {
//                case LowGroup:
//                    SetVariable[] svs = skipCards(a);
//                    // Variables are added in the order that is most cooperative with symmetry breaking.
//                    model.addVariables(svs);
//                    Card card = a.getCard();
//                    for (int i = 0; i < partialParentSolution.size(); i++) {
//                        if (partialParentSolution.hasClafer(i)) {
//                            if (card.isBounded()) {
//                                model.addConstraint(ChocoUtil.betweenCard(svs[i], card));
//                            }
//                        } else {
//                            if (card.isBounded()) {
//                                model.addConstraint(implies(eq(membership.get(a.getParent())[i], 1),
//                                        ChocoUtil.betweenCard(svs[i], card)));
//                            }
//                            model.addConstraint(implies(eq(membership.get(a.getParent())[i], 0),
//                                    eq(svs[i], emptySet())));
//                        }
//                    }
//
//                    // TODO: optimize with partial solution?
//                    model.addConstraint(SetLexManager.setLex(svs, card));
//                    return svs;
//                case ParentGroup:
//                    svs = new SetVariable[partialParentSolution.size()];
//                    assert a.getCard().getLow() == a.getCard().getHigh();
//                    int lowCard = a.getCard().getLow();
//                    for (int i = 0; i < svs.length; i++) {
//                        if (partialParentSolution.hasClafer(i)) {
//                            svs[i] = constant(Util.range(i * lowCard, i * lowCard + lowCard));
//                        } else {
//                            svs[i] = makeSetVar(a.getName() + "#" + i, Util.range(i * lowCard, i * lowCard + lowCard));
//                            model.addConstraint(implies(eq(membership.get(a.getParent())[i], 1),
//                                    eq(svs[i], constant(Util.range(i * lowCard, i * lowCard + lowCard)))));
//                            model.addConstraint(implies(eq(membership.get(a.getParent())[i], 0),
//                                    eq(svs[i], emptySet())));
//                        }
//                    }
//                    model.addVariables(svs);
//                    return svs;
//                default:
//                    throw new CompilerException();
//            }
//        }
//
//        @Override
//        void finish(AstConcreteClafer a, SetVariable[] c) {
//            // If parentPointers then already disjoint because of inverse set constraint.
//            // Otherwise, enforce it.
//            if (a.hasParent() && getScope(a.getParent()) > 1 && !parentPointers.has(a)) {
//                model.addConstraint(setDisjoint(c));
//            }
//        }
//    };
//    /**
//     * clafer_i is under parentPointer[i]. For clafers that do not exist, the parentPointer is
//     * undefined and can point to anything. (TODO: update UniqRef before actually doing this!)
//     */
//    private final LazyCompiler<AstConcreteClafer, IntegerVariable[]> parentPointers = new LazyCompiler<AstConcreteClafer, IntegerVariable[]>() {
//
//        @Override
//        IntegerVariable[] compile(AstConcreteClafer a) {
//            if (!a.hasParent()) {
//                throw new CompilerException(a + " cannot have parent pointers");
//            }
//            Card globalCard = getGlobalCard(a);
//            IntegerVariable[] ivs = buildParentPointers(a);
//            if (globalCard.isExact()) {
//                // No unused
//                model.addConstraint(inverseSet(ivs, childrenSet.get(a)));
//            } else {
//                SetVariable unused = makeSetVar(a.getName() + "@Unused", getPartialSolution(a).getUnknownClafers());
//                model.addConstraint(eq(plus(setCard.get(a), unused.getCard()), getScope(a)));
//                model.addConstraint(inverseSet(ivs, Util.cons(childrenSet.get(a), unused)));
//            }
//            return ivs;
//        }
//    };
//    private final LazyCompiler<AstRef, IntegerVariable[]> refs = new LazyCompiler<AstRef, IntegerVariable[]>() {
//
//        @Override
//        IntegerVariable[] compile(AstRef a) {
//            AstClafer src = a.getSourceType();
//            AstClafer tar = a.getTargetType();
//            int scope = getScope(a.getTargetType());
//
//            int[] partialInts = getPartialInts(a);
//            IntegerVariable[] ivs = new IntegerVariable[getScope(src)];
//            for (int i = 0; i < ivs.length; i++) {
//                Integer instantiate = analysis.getPartialRefInts(a, i);
//                if (instantiate == null) {
//                    if (partialInts == null) {
//                        String option = a.getTargetType() instanceof AstIntClafer && scope > 100 ? Options.V_BOUND : Options.V_ENUM;
//                        ivs[i] = makeIntVar(src.getName() + "@Ref", getScopeLow(tar), getScopeHigh(tar), option);
//                    } else {
//                        ivs[i] = makeIntVar(src.getName() + "@Ref", partialInts);
//                    }
//                } else {
//                    ivs[i] = makeIntVar(src.getName() + "@Ref" + i, new int[]{0, instantiate});
//                }
//            }
//
//            // TODO: Abstract refs
//            // If cardinality is one, then it is already unique even without an explicit constraint.
//            if (a.isUnique()
//                    && src instanceof AstConcreteClafer
//                    && ((AstConcreteClafer) src).getCard().getHigh() > 1) {
//                model.addConstraint(UniqRefManager.uniqRef(parentPointers.get((AstConcreteClafer) src), ivs));
//            } else {
//                // Zeroes out any refs that belong to a dead parents to remove bad isomorphisms.
//                // The uniqRef constraint does this also as a side effect.
//                model.addConstraint(ZeroOutManager.zeroOut(membership.get(src), ivs));
//            }
//            return ivs;
//        }
//    };
//    private final LazyCompiler<Pair<AstClafer, Integer>, SetVariable> thisSet = new LazyCompiler<Pair<AstClafer, Integer>, SetVariable>() {
//
//        @Override
//        SetVariable compile(Pair<AstClafer, Integer> a) {
//            AstClafer thisType = a.getFst();
//            int id = a.getSnd();
//            SetVariable thisV = makeSetVar("constraint" + id + " under " + thisType, id, id, Options.V_NO_DECISION);
//            model.addConstraint(eq(membership.get(thisType)[id], thisV.getCard()));
//            return thisV;
//        }
//    };
//    private LazyCompiler<?, ?>[] compilers = new LazyCompiler<?, ?>[]{set, setCard, membership, childrenSet, parentPointers, refs, thisSet};
//    private int variableNum = 0;
//
//    private SetVariable numSetVar(String name, int low, int high, String... options) {
//        return Choco.makeSetVar(Check.notNull(name) + (variableNum++), low, high, options);
//    }
//
//    private IntegerVariable numIntVar(String name, int low, int high, String... options) {
//        return Choco.makeIntVar(Check.notNull(name) + (variableNum++), low, high, options);
//    }
//
//    /**
//     * Compiles expressions. Assumes that everything has already been type checked.
//     */
//    private class ExpressionCompiler implements AstExpressionVisitor<Void, VarExpr> {
//
//        private final VarExpr thisExpr;
//        private final List<Constraint> globalConstraints = new ArrayList<Constraint>();
//        private final Map<AstLocal, VarExpr> localExprs;
//
//        ExpressionCompiler(VarExpr thisExpr) {
//            this.thisExpr = Check.notNull(thisExpr);
//            this.localExprs = new HashMap<AstLocal, VarExpr>();
//        }
//
//        ExpressionCompiler(VarExpr thisExpr, Map<AstLocal, VarExpr> localExprs) {
//            this.thisExpr = Check.notNull(thisExpr);
//            this.localExprs = Check.notNull(localExprs);
//        }
//
//        void addGlobalConstraint(Constraint constraint) {
//            globalConstraints.add(constraint);
//        }
//
//        void addGlobalConstraints(List<Constraint> constraints) {
//            globalConstraints.addAll(constraints);
//        }
//
//        List<Constraint> getGlobalConstraints() {
//            return globalConstraints;
//        }
//
//        SetVarExpr intToSet(IntVarExpr iv) {
//            SetVariable sv = numSetVar("intToSet", iv.getValue().getLowB(), iv.getValue().getLowB());
//            addGlobalConstraint(SingletonManager.singleton(iv.getValue(), sv));
//            return new SetVarExpr(iv.getType(), sv);
//        }
//
//        @Override
//        public VarExpr visit(AstThis ast, Void a) {
//            return thisExpr;
//        }
//
//        @Override
//        public VarExpr visit(AstConstantInt ast, Void a) {
//            return new IntVarExpr(Ast.IntType, constant(ast.getValue()));
//        }
//
//        @Override
//        public VarExpr visit(AstJoin ast, Void a) {
//            VarExpr left = ast.getLeft().accept(this, a);
//            if (left instanceof SetVarExpr) {
//                return visitJoin((SetVarExpr) left, ast.getRight());
//            } else if (left instanceof IntVarExpr) {
//                return visitJoin((IntVarExpr) left, ast.getRight());
//            }
//            throw new CompilerException();
//        }
//
//        VarExpr visitJoin(SetVarExpr left, AstConcreteClafer right) {
//            SetVariable take = left.getValue();
//            SetVariable[] children = childrenSet.get(right);
//
//            IntPair lowHigh = minMax(take.getLowB(), take.getUppB() + 1, children);
//            int low = lowHigh.getFst();
//            int high = lowHigh.getSnd();
//
//            SetVariable to = numSetVar("join", low, high, Options.V_NO_DECISION);
//            addGlobalConstraint(JoinManager.join(take, children, to));
//
//            return new SetVarExpr(right, to);
//        }
//
//        VarExpr visitJoin(IntVarExpr left, AstConcreteClafer right) {
//            Integer constant = getConstant(left.getValue());
//            switch (getFormat(right)) {
//                case LowGroup:
//                    if (constant != null) {
//                        return new SetVarExpr(right, childrenSet.get(right)[constant]);
//                    }
//                    break;
//                case ParentGroup:
//                    int lowCard = right.getCard().getLow();
//                    if (lowCard == 1) {
//                        return new IntVarExpr(right, left.getValue());
//                    }
//                    if (constant != null) {
//                        return new SetVarExpr(right, constant(Util.range(constant * lowCard, constant * lowCard + lowCard)));
//                    }
//                    break;
//                default:
//                    throw new CompilerException();
//            }
//            return visitJoin(intToSet(left), right);
//        }
//
//        @Override
//        public VarExpr visit(AstJoinParent ast, Void a) {
//            VarExpr children = ast.getChildren().accept(this, a);
//            if (children instanceof SetVarExpr) {
//                return visitJoinParent((SetVarExpr) children);
//            } else if (children instanceof IntVarExpr) {
//                return visitJoinParent((IntVarExpr) children);
//            }
//            throw new CompilerException();
//        }
//
//        VarExpr visitJoinParent(SetVarExpr children) {
//            SetVariable take = children.getValue();
//            AstConcreteClafer childrenType = (AstConcreteClafer) children.getType();
//            AstClafer parentType = childrenType.getParent();
//
//            SetVariable to = numSetVar("joinParent", getScopeLow(parentType), getScopeHigh(parentType), Options.V_NO_DECISION);
//
//            addGlobalConstraint(JoinRefManager.joinRef(take, parentPointers.get(childrenType), to));
//
//            return new SetVarExpr(parentType, to);
//        }
//
//        VarExpr visitJoinParent(IntVarExpr children) {
//            IntegerVariable take = children.getValue();
//            AstConcreteClafer childrenType = (AstConcreteClafer) children.getType();
//            AstClafer parentType = childrenType.getParent();
//
//            if (getScope(parentType) == 1) {
//                return new IntVarExpr(parentType, constant(0));
//            }
//
//            IntegerVariable[] parentVars = parentPointers.get(childrenType);
//
//            Integer constant = getConstant(children.getValue());
//            if (constant != null) {
//                switch (getFormat(childrenType)) {
//                    case LowGroup:
//                        return new IntVarExpr(parentType, parentVars[constant]);
//                    case ParentGroup:
//                        return new IntVarExpr(parentType, constant(constant / childrenType.getCard().getLow()));
//                    default:
//                        throw new CompilerException();
//                }
//            }
//
//            IntegerVariable to = numIntVar("joinParent", getScopeLow(parentType), getScopeHigh(parentType), Options.V_NO_DECISION);
//            addGlobalConstraint(Choco.nth(take, parentVars, to));
//
//            return new IntVarExpr(parentType, to);
//        }
//
//        @Override
//        public VarExpr visit(AstJoinRef ast, Void a) {
//            VarExpr deref = ast.getDeref().accept(this, a);
//            if (deref instanceof SetVarExpr) {
//                return visitJoinRef((SetVarExpr) deref);
//            } else if (deref instanceof IntVarExpr) {
//                return visitJoinRef((IntVarExpr) deref);
//            }
//            throw new CompilerException();
//        }
//
//        VarExpr visitJoinRef(SetVarExpr deref) {
//            SetVariable take = deref.getValue();
//            AstClafer type = deref.getType();
//            AstRef ref = type.getRef();
//            AstClafer targetType = ref.getTargetType();
//
//            SetVariable to = numSetVar("joinRef", getScopeLow(targetType), getScopeHigh(targetType), Options.V_NO_DECISION);
//
//            addGlobalConstraint(JoinRefManager.joinRef(take, refs.get(ref), to));
//
//            return new SetVarExpr(targetType, to);
//        }
//
//        VarExpr visitJoinRef(IntVarExpr deref) {
//            IntegerVariable take = deref.getValue();
//            AstClafer type = deref.getType();
//            AstRef ref = type.getRef();
//            AstClafer targetType = ref.getTargetType();
//
//            IntegerVariable[] refVars = refs.get(ref);
//
//            Integer constant = getConstant(deref.getValue());
//            if (constant != null) {
//                return new IntVarExpr(targetType, refVars[constant]);
//            }
//
//            IntegerVariable to = numIntVar("joinRef", getScopeLow(targetType), getScopeHigh(targetType), Options.V_NO_DECISION);
//            addGlobalConstraint(Choco.nth(take, refVars, to));
//
//            return new IntVarExpr(targetType, to);
//        }
//
//        @Override
//        public VarExpr visit(AstCard ast, Void a) {
//            VarExpr set = ast.getSet().accept(this, a);
//            if (set instanceof SetVarExpr) {
//                return new IntVarExpr(Ast.IntType, ((SetVarExpr) set).getValue().getCard());
//            }
//            return new IntVarExpr(Ast.IntType, constant(1));
//        }
//
//        @Override
//        public VarExpr visit(AstCompare ast, Void a) {
//            VarExpr left = ast.getLeft().accept(this, a);
//            VarExpr right = ast.getRight().accept(this, a);
//
//            switch (ast.getOp()) {
//                case Equal:
//                    if (left instanceof SetVarExpr && right instanceof SetVarExpr) {
//                        return visitEq((SetVarExpr) left, (SetVarExpr) right);
//                    } else if (left instanceof SetVarExpr && right instanceof IntVarExpr) {
//                        return visitEq((SetVarExpr) left, (IntVarExpr) right);
//                    } else if (left instanceof IntVarExpr && right instanceof SetVarExpr) {
//                        return visitEq((IntVarExpr) left, (SetVarExpr) right);
//                    } else if (left instanceof IntVarExpr && right instanceof IntVarExpr) {
//                        return visitEq((IntVarExpr) left, (IntVarExpr) right);
//                    }
//                    break;
//                default:
//            }
//            throw new CompilerException();
//        }
//
//        VarExpr visitEq(SetVarExpr left, SetVarExpr right) {
//            return new BoolVarExpr(Choco.eq(left.getValue(), right.getValue()));
//        }
//
//        VarExpr visitEq(SetVarExpr left, IntVarExpr right) {
//            // TODO: use singleton but we need a "not singleton" constraint for opposite
//            return new BoolVarExpr(and(eqCard(left.getValue(), 1), member(right.getValue(), left.getValue())));
//        }
//
//        VarExpr visitEq(IntVarExpr left, SetVarExpr right) {
//            return visitEq(right, left);
//        }
//
//        VarExpr visitEq(IntVarExpr left, IntVarExpr right) {
//            return new BoolVarExpr(Choco.eq(left.getValue(), right.getValue()));
//        }
//
//        @Override
//        public VarExpr visit(AstUpcast ast, Void a) {
//            VarExpr base = ast.getBase().accept(this, a);
//            if (base instanceof SetVarExpr) {
//                return visitUpcast((SetVarExpr) base, ast.getTarget());
//            } else if (base instanceof IntVarExpr) {
//                return visitUpcast((IntVarExpr) base, ast.getTarget());
//            }
//            throw new CompilerException();
//        }
//
//        VarExpr visitUpcast(SetVarExpr base, AstAbstractClafer target) {
//            int offset = getOffset(target, base.getType());
//
//            SetVariable to = numSetVar("upcast", base.getValue().getLowB() + offset, base.getValue().getUppB() + offset);
//            addGlobalConstraint(UpcastManager.upcast(base.getValue(), to, offset));
//            return new SetVarExpr(target, to);
//        }
//
//        VarExpr visitUpcast(IntVarExpr base, AstAbstractClafer target) {
//            IntegerVariable from = base.getValue();
//            int offset = getOffset(target, base.getType());
//
//            Integer constant = ChocoUtil.getConstant(from);
//            if (constant != null) {
//                return new IntVarExpr(target, constant(constant + offset));
//            }
//
//            IntegerVariable to = numIntVar("upcast", from.getLowB(), from.getUppB());
//            // TODO: local constraint
//            addGlobalConstraint(eq(to, plus(from, offset)));
//            return new IntVarExpr(target, to);
//        }
//
//        @Override
//        public VarExpr visit(AstNone ast, Void a) {
//            SetVarExpr set = (SetVarExpr) ast.getSet().accept(this, a);
//            return new BoolVarExpr(eq(set.getValue(), emptySet()));
//        }
//
//        @Override
//        public VarExpr visit(AstLocal ast, Void a) {
//            VarExpr var = localExprs.get(ast);
//            if (var == null) {
//                throw new IllegalStateException();
//            }
//            return var;
//        }
//
//        @Override
//        public VarExpr visit(AstQuantify ast, Void a) {
//            switch (ast.getQuantifier()) {
//                case All:
//                    AstDecl[] decls = ast.getDecls();
//
//                    for (AstDecl decl : decls) {
//                        // TODO decl.isDisjoint
//                        VarExpr $body = decl.getBody().accept(this, a);
//                    }
//            }
//            return null;
//        }
//    }
//
//    /*************************
//     * Convenience functions.
//     *************************/
//    int getScopeLow(AstClafer clafer) {
//        return clafer instanceof AstIntClafer ? scope.getIntLow() : 0;
//    }
//
//    int getScopeHigh(AstClafer clafer) {
//        return clafer instanceof AstIntClafer ? scope.getIntHigh() : getScope(clafer) - 1;
//    }
//
//    public int getScope(AstClafer clafer) {
//        return analysis.getScope(clafer);
//    }
//
//    public Format getFormat(AstClafer clafer) {
//        return analysis.getFormat(clafer);
//    }
//
//    public PartialSolution getPartialSolution(AstClafer clafer) {
//        return analysis.getPartialSolution(clafer);
//    }
//
//    public PartialSolution getPartialParentSolution(AstConcreteClafer clafer) {
//        return clafer.hasParent() ? getPartialSolution(clafer.getParent()) : PartialSolution.rootSolution();
//    }
//
//    public int[] getPartialInts(AstRef ref) {
//        return analysis.getPartialInts(ref);
//    }
//
//    public int getOffset(AstAbstractClafer sup, AstClafer sub) {
//        return analysis.getOffset(sup, sub);
//    }
//
//    public Card getGlobalCard(AstClafer clafer) {
//        return analysis.getGlobalCard(clafer);
//    }
//
//    public int getDepth(AstAbstractClafer clafer) {
//        return analysis.getDepth(clafer);
//    }
//
//    /*************************
//     * 
//     * Optimizations convenience functions
//     * 
//     *************************/
//    /**
//     * @param from
//     * @param to
//     * @param svs
//     * @return 
//     * 
//     */
//    private IntPair minMax(int from, int to, SetVariable... svs) {
//        if (from > to) {
//            throw new IllegalArgumentException();
//        }
//        if (to > svs.length) {
//            throw new IllegalArgumentException(to + ">=" + svs.length);
//        }
//        int min = svs[from].getLowB();
//        int max = svs[from].getUppB();
//        for (int i = from + 1; i < to; i++) {
//            min = Math.min(min, svs[i].getLowB());
//            max = Math.max(max, svs[i].getUppB());
//        }
//        return new IntPair(min, max);
//    }
//
//    private SetVariable[] skipCards(AstConcreteClafer clafer) {
//        int parentScope = clafer.hasParent() ? getScope(clafer.getParent()) : 1;
//        PartialSolution partialParentSolution = clafer.hasParent() ? getPartialSolution(clafer.getParent()) : PartialSolution.rootSolution();
//
//        int claferScope = getScope(clafer);
//        Card card = clafer.getCard();
//        assert card.hasHigh();
//
//        int low = 0;
//        int high = card.getHigh();
//        int max = claferScope - 1;
//
//        SetVariable[] skip = new SetVariable[parentScope];
//        for (int i = 0; i < skip.length; i++) {
//            if (low <= max) {
//                skip[i] = makeSetVar(clafer.getName() + "#" + i, low, Math.min(high - 1, max));
//            } else {
//                skip[i] = emptySet();
//            }
//            if (partialParentSolution.hasClafer(i)) {
//                low += card.getLow();
//            }
//            high += card.getHigh();
//        }
//        return skip;
//    }
//
//    private IntegerVariable[] buildParentPointers(AstConcreteClafer clafer) {
//        PartialSolution solution = getPartialSolution(clafer);
//        IntegerVariable[] pointers = new IntegerVariable[solution.size()];
//        for (int i = 0; i < pointers.length; i++) {
//            pointers[i] = makeIntVar(clafer.getName() + "@Parent#" + i,
//                    solution.hasClafer(i)
//                    ? solution.getPossibleParents(i)
//                    : Util.cons(solution.getPossibleParents(i), getScope(clafer.getParent())));
//        }
//        return pointers;
//    }
//
//    private IntegerVariable[] score(AstClafer clafer, Model model, List<Term> terms) {
//        assert FP.same(FP.mapped(terms, FP.compose(FP.<SetVariable>length(), getChildSet)));
//
//        switch (terms.size()) {
//            case 0:
//                return new IntegerVariable[0];
//            case 1:
//                Term term = terms.get(0);
//                IntegerVariable[] score = new IntegerVariable[term.getChildSet().length];
//                for (int i = 0; i < score.length; i++) {
//                    score[i] = term.getChildSet()[i].getCard();
//                }
//                return score;
//            default:
//                long max = 1;
//                int[] scales = new int[terms.size()];
//                for (int i = 0; i < scales.length; i++) {
//                    scales[i] = (int) max;
//                    term = terms.get(i);
//
//                    max *= term.getMax() + 1;
//                    if (max > Integer.MAX_VALUE) {
//                        // It's possible to overflow
//                        // TODO: What's the best option in this case?
//                        throw new RuntimeException("Overflow");
//                    }
//                }
//
//                score = Choco.makeIntVarArray(clafer.getName() + "@Score", getScope(clafer), 0, (int) max);
//                for (int i = 0; i < score.length; i++) {
//                    IntegerVariable[] termVars = new IntegerVariable[terms.size()];
//                    for (int j = 0; j < termVars.length; j++) {
//                        termVars[j] = terms.get(j).getChildSet()[i].getCard();
//                    }
//                    model.addConstraint(equation(score[i], termVars, scales));
//                }
//                return score;
//        }
//    }
//
//    private class Term {
//
//        // TODO: use min
//        @Deprecated
//        private final int min;
//        // The max card of each child set
//        private final int max;
//        private final SetVariable[] childSet;
//
//        private Term(AstConcreteClafer clafer) {
//            this(0,
//                    Math.min(clafer.getCard().getHigh(), getScope(clafer)),
//                    childrenSet.get(clafer));
//        }
//
//        private Term(AstConcreteClafer clafer, int from, int to) {
//            this(0,
//                    Math.min(clafer.getCard().getHigh(), getScope(clafer)),
//                    Arrays.copyOfRange(childrenSet.get(clafer), from, to));
//        }
//
//        private Term(int min, int max, SetVariable[] childSet) {
//            this.min = min;
//            this.max = max;
//            this.childSet = childSet;
//        }
//
//        private int getMin() {
//            return min;
//        }
//
//        private int getMax() {
//            return max;
//        }
//
//        private SetVariable[] getChildSet() {
//            return childSet;
//        }
//    }
//    private static final Func<Term, SetVariable[]> getChildSet =
//            new Func<Term, SetVariable[]>() {
//
//                @Override
//                public SetVariable[] apply(Term param) {
//                    return param.getChildSet();
//                }
//            };
//
//    private static abstract class VarExpr {
//    }
//
//    private static class BoolVarExpr extends VarExpr {
//
//        private final Constraint value;
//
//        BoolVarExpr(Constraint value) {
//            this.value = Check.notNull(value);
//        }
//
//        Constraint getValue() {
//            return value;
//        }
//    }
//
//    private static class IntVarExpr extends VarExpr {
//
//        private final AstClafer type;
//        private final IntegerVariable value;
//
//        IntVarExpr(AstClafer type, IntegerVariable value) {
//            this.type = Check.notNull(type);
//            this.value = Check.notNull(value);
//        }
//
//        AstClafer getType() {
//            return type;
//        }
//
//        IntegerVariable getValue() {
//            return value;
//        }
//    }
//
//    private static class SetVarExpr extends VarExpr {
//
//        private final AstClafer type;
//        private final SetVariable value;
//
//        SetVarExpr(AstClafer type, SetVariable value) {
//            this.type = Check.notNull(type);
//            this.value = Check.notNull(value);
//        }
//
//        AstClafer getType() {
//            return type;
//        }
//
//        SetVariable getValue() {
//            return value;
//        }
//    }
//}
//