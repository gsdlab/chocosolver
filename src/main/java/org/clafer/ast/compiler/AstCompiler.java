package org.clafer.ast.compiler;

import org.clafer.ir.IrSetExpr;
import org.clafer.Util;
import org.clafer.ir.IrBoolVar;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.clafer.Check;
import org.clafer.Scope;
import org.clafer.analysis.Analysis;
import org.clafer.analysis.AnalysisUtil;
import org.clafer.analysis.FormatAnalysis.Format;
import org.clafer.analysis.PartialSolutionAnalysis.PartialSolution;
import org.clafer.ast.Ast;
import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstException;
import org.clafer.ast.AstIntClafer;
import org.clafer.ast.AstModel;
import org.clafer.ast.AstRef;
import org.clafer.ast.Card;
import org.clafer.ir.IrBoolExpr;
import org.clafer.ir.IrIntExpr;
import org.clafer.ir.IrModule;
import org.clafer.ir.IrSetVar;
import org.clafer.ir.compiler.IrCompiler;
import solver.Solver;
import static org.clafer.ir.Irs.*;

/**
 *
 * @author jimmy
 */
public class AstCompiler {

    public static void main(String[] args) {
        AstModel model = Ast.newModel();
        model.addTopClafer("Jimmy").withCard(1, 2).addChild("Degree").withCard(1, 2);

        IrModule out = new IrModule();
        AstCompiler.compile(model, new Scope(100), out);

        Solver solver = new Solver();
        IrCompiler.compile(out, solver);

        if (solver.findSolution()) {
            System.out.println(solver);
        }
    }
    private final AstModel model;
    private final Analysis analysis;
    private final IrModule module;

    private AstCompiler(AstModel model, Scope scope, IrModule module) {
        this.model = Check.notNull(model);
        this.analysis = Analysis.analyze(model, scope);
        this.module = Check.notNull(module);
    }

    public static void compile(AstModel in, Scope scope, IrModule out) {
        AstCompiler compiler = new AstCompiler(in, scope, out);
        compiler.compile();
    }

    private void compile() {
        List<AstAbstractClafer> abstractClafers = model.getAbstractClafers();
        List<AstConcreteClafer> concreteClafers = AnalysisUtil.getConcreteClafers(model);
        List<AstClafer> clafers = new ArrayList<AstClafer>(abstractClafers.size() + concreteClafers.size());
        clafers.addAll(abstractClafers);
        clafers.addAll(concreteClafers);
        for (AstClafer clafer : clafers) {
//            if (Format.LowGroup.equals(getFormat(clafer))) {
            getCompiler(clafer).initLowGroup(clafer);
//            }
        }
//        for (AstClafer clafer : clafers) {
//            if (Format.ParentGroup.equals(getFormat(clafer))) {
//                getCompiler(clafer).initParentGroup(clafer);
//            }
//        }
        for (AstClafer clafer : clafers) {
//            if (Format.LowGroup.equals(getFormat(clafer))) {
            getCompiler(clafer).constrainLowGroup(clafer);
//            }
        }
//        for (AstClafer clafer : clafers) {
//            if (Format.ParentGroup.equals(getFormat(clafer))) {
//                getCompiler(clafer).constrainParentGroup(clafer);
//            }
//        }
    }

    private ClaferCompiler getCompiler(AstClafer clafer) {
        if (clafer instanceof AstConcreteClafer) {
            return concreteClaferCompiler;
        }
        throw new IllegalArgumentException();
    }
    private final ClaferCompiler<AstConcreteClafer> concreteClaferCompiler = new ClaferCompiler<AstConcreteClafer>() {

        @Override
        void initLowGroup(AstConcreteClafer clafer) {
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

        @Override
        void constrainLowGroup(AstConcreteClafer clafer) {
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
        }

        @Override
        void initParentGroup(AstConcreteClafer clafer) {
            throw new UnsupportedOperationException("Not supported yet.");
        }

        @Override
        void constrainParentGroup(AstConcreteClafer clafer) {
            throw new UnsupportedOperationException("Not supported yet.");
        }

        IrSetVar[] compile(AstConcreteClafer a) {
            PartialSolution partialParentSolution = getPartialParentSolution(a);
            switch (getFormat(a)) {
                case LowGroup:
                    IrSetVar[] svs = skipCards(a);
                    // Variables are added in the order that is most cooperative with symmetry breaking.
                    Card card = a.getCard();
                    for (int i = 0; i < partialParentSolution.size(); i++) {
                        if (partialParentSolution.hasClafer(i)) {
                            module.addConstraint(constrainCard(setCard(svs[i]), card));
                        } else {
                            if (card.isBounded()) {
                                module.addConstraint(implies(membership.get(a.getParent())[i],
                                        constrainCard(setCard(svs[i]), card)));
                            }
                            module.addConstraint(implies(not(membership.get(a.getParent())[i]),
                                    equal(svs[i], EmptySet)));
                        }
                    }

                    // TODO: optimize with partial solution?
//                    model.addConstraint(SetLexManager.setLex(svs, card));
                    return svs;
                case ParentGroup:
                    svs = new IrSetVar[partialParentSolution.size()];
                    assert a.getCard().getLow() == a.getCard().getHigh();
                    int lowCard = a.getCard().getLow();
                    for (int i = 0; i < svs.length; i++) {
                        if (partialParentSolution.hasClafer(i)) {
                            svs[i] = constant(Util.range(i * lowCard, i * lowCard + lowCard));
                        } else {
                            svs[i] = set(a.getName() + "#" + i, Util.range(i * lowCard, i * lowCard + lowCard));
                            module.addConstraint(implies(membership.get(a.getParent())[i],
                                    equal(svs[i], constant(Util.range(i * lowCard, i * lowCard + lowCard)))));
                            module.addConstraint(implies(not(membership.get(a.getParent())[i]),
                                    equal(svs[i], EmptySet)));
                        }
                    }
                    return svs;
                default:
                    throw new AstException();
            }
        }
    };
    private final Map<AstClafer, IrSetExpr> set = new HashMap<AstClafer, IrSetExpr>();
    private final Map<AstClafer, IrSetVar[]> childrenSet = new HashMap<AstClafer, IrSetVar[]>();
    private final Map<AstClafer, IrBoolVar[]> membership = new HashMap<AstClafer, IrBoolVar[]>();

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

    /**
     * Requires two steps to avoid circularity.
     */
    private static abstract class ClaferCompiler<T extends AstClafer> {

        /**
         * Create the variables.
         */
        abstract void initLowGroup(T clafer);

        /**
         * Create the variables.
         */
        abstract void initParentGroup(T clafer);

        /**
         * Constrain the variables.
         */
        abstract void constrainLowGroup(T clafer);

        /**
         * Constrain the variables.
         */
        abstract void constrainParentGroup(T clafer);
    }
}
