package org.clafer.ir.analysis;

import org.clafer.ir.IrRewriter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import org.clafer.collection.Pair;
import org.clafer.ir.IrAnd;
import org.clafer.ir.IrBoolExpr;
import org.clafer.ir.IrCard;
import org.clafer.ir.IrCompare;
import org.clafer.ir.IrDomain;
import org.clafer.ir.IrImplies;
import org.clafer.ir.IrIntExpr;
import org.clafer.ir.IrIntVar;
import org.clafer.ir.IrModule;
import org.clafer.ir.IrNop;
import org.clafer.ir.IrSetExpr;
import org.clafer.ir.IrSetLiteral;
import org.clafer.ir.IrSetVar;
import org.clafer.ir.IrUtil;
import static org.clafer.ir.Irs.*;

/**
 *
 * @author jimmy
 */
public class Canonicalizer {

    private Canonicalizer() {
    }

    /**
     * Rewrites the module in a form that is easier to pattern match and makes
     * some implicit constraints explicit.
     *
     * @param module the module to canonicalize
     * @return the canonicalized module
     */
    public static IrModule canonical(IrModule module) {
        CanonicalRewriter rewriter = new CanonicalRewriter();

        IrModule optModule = rewriter.rewriteAndNonNops(module, null);
        for (Entry<IrSetVar, IrIntVar> entry : rewriter.setVarCards.entrySet()) {
            optModule.addConstraint(equal($(entry.getValue()), card($(entry.getKey()))));
        }
        for (IrSetVar setVar : rewriter.setVars) {
            if (setVar.getCard().getLowBound() > setVar.getKer().size()
                    || setVar.getCard().getHighBound() < setVar.getEnv().size()) {
                // These variables need to have their cardinalities constrainted.
                optModule.addConstraint(equal(
                        $(domainInt("|" + setVar.getName() + "|", setVar.getCard())),
                        card($(setVar))));
            }
        }
        return optModule;
    }

    private static class CanonicalRewriter extends IrRewriter<Void> {

        private final List<IrSetVar> setVars = new ArrayList<IrSetVar>();
        private final Map<IrSetVar, IrIntVar> setVarCards = new HashMap<IrSetVar, IrIntVar>();

        @Override
        public IrBoolExpr visit(IrAnd ir, Void a) {
            Map<IrBoolExpr, Set<IrBoolExpr>> implications = new HashMap<IrBoolExpr, Set<IrBoolExpr>>();
            List<IrBoolExpr> opts = new ArrayList<IrBoolExpr>(ir.getOperands().length);
            for (IrBoolExpr operand : ir.getOperands()) {
                IrBoolExpr opt = rewrite(operand, a);
                if (opt instanceof IrImplies) {
                    IrImplies implies = (IrImplies) opt;
                    Set<IrBoolExpr> consequents = implications.get(implies.getAntecedent());
                    if (consequents == null) {
                        consequents = new HashSet<IrBoolExpr>();
                        implications.put(implies.getAntecedent(), consequents);
                    }
                    consequents.add(implies.getConsequent());
                } else {
                    opts.add(opt);
                }
            }
            for (Map.Entry<IrBoolExpr, Set<IrBoolExpr>> entry : implications.entrySet()) {
                IrBoolExpr antecedent = entry.getKey();
                Set<IrBoolExpr> consequents = entry.getValue();
                if (!consequents.isEmpty()) {
                    IrBoolExpr notAntecedent = not(antecedent);
                    Set<IrBoolExpr> alternatives = implications.get(notAntecedent);
                    if (alternatives != null && !alternatives.isEmpty()) {
                        Iterator<IrBoolExpr> iter = consequents.iterator();
                        while (iter.hasNext()) {
                            IrBoolExpr consequent = iter.next();
                            IrBoolExpr alternative = not(consequent);
                            if (alternatives.remove(alternative)) {
                                opts.add(
                                        // Either choice is fine, but prefer to pick
                                        // the antecedent that is not negative because
                                        // it looks nicer and some cases easier to
                                        // propagate.
                                        antecedent.isNegative()
                                        ? ifOnlyIf(notAntecedent, alternative)
                                        : ifOnlyIf(antecedent, consequent));
                                iter.remove();
                            }
                        }
                    }
                    for (IrBoolExpr consequent : consequents) {
                        opts.add(implies(antecedent, consequent));
                    }
                    consequents.clear();
                }
            }
            return and(opts);
        }

        @Override
        public IrBoolExpr visit(IrCompare ir, Void a) {
            IrIntExpr left = rewrite(ir.getLeft(), a);
            IrIntExpr right = rewrite(ir.getRight(), a);
            if (ir.getOp().isEquality()) {
                IrDomain leftDomain = left.getDomain();
                IrDomain rightDomain = right.getDomain();
                // This canoicalizes the form somewhat so that is easier for the other
                // analysis to find pattern matches.
                if (leftDomain.size() == 2) {
                    // Prefer the one that's closer to 0.
                    int prefer = leftDomain.getLowBound();
                    int unprefer = leftDomain.getHighBound();
                    if (Math.abs(leftDomain.getLowBound()) > Math.abs(leftDomain.getHighBound())) {
                        prefer = leftDomain.getHighBound();
                        unprefer = leftDomain.getLowBound();
                    }
                    Integer rightConstant = IrUtil.getConstant(right);
                    if (rightConstant != null && rightConstant.intValue() == unprefer) {
                        return compare(left, ir.getOp().negate(), prefer);
                    }
                }
                if (rightDomain.size() == 2) {
                    // Prefer the one that's closer to 0.
                    int prefer = rightDomain.getLowBound();
                    int unprefer = rightDomain.getHighBound();
                    if (Math.abs(rightDomain.getLowBound()) > Math.abs(rightDomain.getHighBound())) {
                        prefer = rightDomain.getHighBound();
                        unprefer = rightDomain.getLowBound();
                    }
                    Integer leftConstant = IrUtil.getConstant(left);
                    if (leftConstant != null && leftConstant.intValue() == unprefer) {
                        return compare(prefer, ir.getOp().negate(), right);
                    }
                }
            }
            return compare(left, ir.getOp(), right);
        }

        @Override
        public IrIntExpr visit(IrCard ir, Void a) {
            /*
             * |Literal| is a very common expression that is used very often.
             * Instead, rewrite as NewVar = |Literal| so that every reference
             * to |Literal| can use the same variable.
             */
            IrSetExpr set = rewrite(ir.getSet(), a);
            if (set instanceof IrSetLiteral) {
                IrSetLiteral setLiteral = (IrSetLiteral) set;
                IrSetVar setVar = setLiteral.getVar();
                IrIntVar card = setVarCards.get(setVar);
                if (card == null) {
                    card = domainInt("|" + setVar.getName() + "|", setVar.getCard());
                    setVarCards.put(setVar, card);
                }
                return $(card);
            }
            return card(set);
        }

        @Override
        public IrSetExpr visit(IrSetLiteral ir, Void a) {
            setVars.add(ir.getVar());
            return ir;
        }
    };
}
