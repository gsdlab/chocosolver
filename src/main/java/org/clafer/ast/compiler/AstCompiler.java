package org.clafer.ast.compiler;

import org.clafer.compiler.ClaferCompiler;
import org.clafer.ast.Ast;
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
import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstIntClafer;
import org.clafer.ast.AstModel;
import org.clafer.ast.AstRef;
import org.clafer.ast.Card;
import org.clafer.ir.IrBoolExpr;
import org.clafer.ir.IrIntExpr;
import org.clafer.ir.IrModule;
import org.clafer.ir.IrSetVar;
import solver.constraints.IntConstraintFactory;
import static org.clafer.ir.Irs.*;

/**
 * Compile from AST -> IR
 * 
 * @author jimmy
 */
public class AstCompiler {

    public static void main(String[] args) {
        AstModel model = Ast.newModel();
        AstAbstractClafer person = model.addAbstractClafer("person");
        AstConcreteClafer name = person.addChild("name").withCard(1, 2);

        AstConcreteClafer jim = model.addTopClafer("Jimmy").withCard(1, 2).extending(person);
        AstConcreteClafer jan = model.addTopClafer("Janet").withCard(1, 3).extending(person);

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.builder().defaultScope(5).intLow(-1).intHigh(1).toScope());
        System.out.println(solver);
        while (solver.nextSolution()) {
            System.out.println(solver.solution());
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

        IrBoolVar[] members = membership.get(clafer);
        module.addConstraint(boolChannel(members, claferSet));

        if (clafer.hasParent() && getScope(clafer.getParent()) > 1) {
            IrIntVar[] parents = parentPointers.get(clafer);
            module.addConstraint(sort(parents));
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
    }

    private void constrainAbstract(AstAbstractClafer clafer) {
        // Do nothing
    }
    private final ReadWriteHashMap<AstClafer, IrSetExpr> set = new ReadWriteHashMap<AstClafer, IrSetExpr>();
    private final ReadWriteHashMap<AstClafer, IrSetVar[]> childrenSet = new ReadWriteHashMap<AstClafer, IrSetVar[]>();
    private final ReadWriteHashMap<AstClafer, IrBoolVar[]> membership = new ReadWriteHashMap<AstClafer, IrBoolVar[]>();
    private final ReadWriteHashMap<AstConcreteClafer, IrIntVar[]> parentPointers = new ReadWriteHashMap<AstConcreteClafer, IrIntVar[]>();
    private final ReadWriteHashMap<AstRef, IrIntVar[]> refPointers = new ReadWriteHashMap<AstRef, IrIntVar[]>();

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
