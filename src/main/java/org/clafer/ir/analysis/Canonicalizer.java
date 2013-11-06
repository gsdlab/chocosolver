package org.clafer.ir.analysis;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import org.clafer.ir.IrCard;
import org.clafer.ir.IrIntExpr;
import org.clafer.ir.IrIntVar;
import org.clafer.ir.IrModule;
import org.clafer.ir.IrRewriter;
import org.clafer.ir.IrSetExpr;
import org.clafer.ir.IrSetVar;
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

        IrModule optModule = rewriter.rewrite(module, null);
        for (Entry<IrSetVar, IrIntVar> entry : rewriter.setVarCards.entrySet()) {
            optModule.addConstraint(equal(entry.getValue(), card(entry.getKey())));
        }
        rewriter.setVars.removeAll(rewriter.setVarCards.keySet());
        for (IrSetVar setVar : rewriter.setVars) {
            if (setVar.getCard().getLowBound() > setVar.getKer().size()
                    || setVar.getCard().getHighBound() < setVar.getEnv().size()) {
                // These variables need to have their cardinalities constrainted.
                optModule.addConstraint(equal(
                        domainInt("|" + setVar.getName() + "|", setVar.getCard()),
                        card(setVar)));
            }
        }
        return optModule;
    }

    private static class CanonicalRewriter extends IrRewriter<Void> {

        private final Set<IrSetVar> setVars = new HashSet<>();
        private final Map<IrSetVar, IrIntVar> setVarCards = new HashMap<>();

        @Override
        public IrIntExpr visit(IrCard ir, Void a) {
            /*
             * |Literal| is a very common expression that is used very often.
             * Instead, rewrite as NewVar = |Literal| so that every reference
             * to |Literal| can use the same variable.
             */
            IrSetExpr set = rewrite(ir.getSet(), a);
            if (set instanceof IrSetVar) {
                IrSetVar setVar = (IrSetVar) set;
                IrIntVar card = setVarCards.get(setVar);
                if (card == null) {
                    card = domainInt("|" + setVar.getName() + "|", setVar.getCard());
                    setVarCards.put(setVar, card);
                }
                return card;
            }
            return changed(ir.getSet(), set)
                    ? card(set)
                    : ir;
        }

        @Override
        public IrSetVar visit(IrSetVar ir, Void a) {
            setVars.add(ir);
            return ir;
        }
    };
}
