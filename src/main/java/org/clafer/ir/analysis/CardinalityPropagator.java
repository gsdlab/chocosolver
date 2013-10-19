package org.clafer.ir.analysis;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.clafer.collection.Pair;
import org.clafer.ir.IrBoolExpr;
import org.clafer.ir.IrDomain;
import org.clafer.ir.IrIntExpr;
import org.clafer.ir.IrIntVar;
import org.clafer.ir.IrModule;
import org.clafer.ir.IrRewriter;
import org.clafer.ir.IrSetVar;
import org.clafer.ir.IrUtil;
import org.clafer.ir.Irs;

/**
 *
 * @author jimmy
 */
public class CardinalityPropagator {

    private CardinalityPropagator() {
    }

    public static Pair<Map<IrSetVar, IrSetVar>, IrModule> propagate(IrModule module) {
        Map<IrSetVar, IrSetVar> propagated = new HashMap<IrSetVar, IrSetVar>();
        Map<IrSetVar, IrIntVar> duplicates = new HashMap<IrSetVar, IrIntVar>();
        List<IrBoolExpr> constraints = new ArrayList<IrBoolExpr>();
        for (IrBoolExpr constraint : module.getConstraints()) {
            Pair<IrIntExpr, IrSetVar> cardinality = AnalysisUtil.getAssignCardinality(constraint);
            if (cardinality != null) {
                IrIntExpr cardExpr = cardinality.getFst();
                IrSetVar setVar = cardinality.getSnd();

                if (cardExpr instanceof IrIntVar) {
                    IrIntVar cardVar = (IrIntVar) cardExpr;
                    IrIntVar duplicate = duplicates.put(setVar, cardVar);
                    if (duplicate != null) {
                        constraints.add(Irs.equal(cardVar, duplicate));
                        continue;
                    }
                }

                if (!cardExpr.getDomain().equals(setVar.getCard())) {
                    IrDomain card = IrUtil.intersection(
                            cardExpr.getDomain(),
                            setVar.getCard());
                    IrSetVar var = IrUtil.asConstant(setVar.withCard(card));
                    propagated.put(cardinality.getSnd(), var);
                }
            }
            constraints.add(constraint);
        }

        IrModule optModule = new IrModule();
        optModule.addVariables(module.getVariables());
        optModule.addConstraints(constraints);
        return new Pair<Map<IrSetVar, IrSetVar>, IrModule>(
                propagated,
                new CardinalityRewriter(propagated).rewrite(optModule, null));
    }

    private static class CardinalityRewriter extends IrRewriter<Void> {

        private final Map<IrSetVar, IrSetVar> propagated;

        CardinalityRewriter(Map<IrSetVar, IrSetVar> propagated) {
            this.propagated = propagated;
        }

        @Override
        public IrSetVar visit(IrSetVar ir, Void a) {
            IrSetVar var = propagated.get(ir);
            return var == null ? ir : var;
        }
    }
}
