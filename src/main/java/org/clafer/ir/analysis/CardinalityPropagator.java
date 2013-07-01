package org.clafer.ir.analysis;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.clafer.collection.Pair;
import org.clafer.ir.IrBoolExpr;
import org.clafer.ir.IrDomain;
import org.clafer.ir.IrIntExpr;
import org.clafer.ir.IrModule;
import org.clafer.ir.IrRewriter;
import org.clafer.ir.IrSetExpr;
import org.clafer.ir.IrSetVar;
import org.clafer.ir.IrUtil;
import static org.clafer.ir.Irs.*;

/**
 *
 * @author jimmy
 */
public class CardinalityPropagator {

    private CardinalityPropagator() {
    }

    public static Pair<Map<IrSetVar, IrSetVar>, IrModule> propagate(IrModule module) {
        List<IrBoolExpr> nops = new ArrayList<IrBoolExpr>();
        Map<IrSetVar, IrSetVar> propagated = new HashMap<IrSetVar, IrSetVar>();
        for (IrBoolExpr constraint : module.getConstraints()) {
            Pair<IrIntExpr, IrSetVar> cardinality = AnalysisUtil.getAssignCardinality(constraint);
            if (cardinality != null) {
                if (!cardinality.getFst().getDomain().equals(cardinality.getSnd().getCard())) {
                    IrDomain card = IrUtil.intersection(
                            cardinality.getFst().getDomain(),
                            cardinality.getSnd().getCard());
                    IrSetVar var = IrUtil.asConstant(cardinality.getSnd().withCard(card));
                    propagated.put(cardinality.getSnd(), var);
                    nops.add(nop(var));
                }
            }
        }
        return new Pair<Map<IrSetVar, IrSetVar>, IrModule>(
                propagated,
                new CardinalityRewriter(propagated).rewrite(module, null).addConstraints(nops));
    }

    private static class CardinalityRewriter extends IrRewriter<Void> {

        private final Map<IrSetVar, IrSetVar> propagated;

        CardinalityRewriter(Map<IrSetVar, IrSetVar> propagated) {
            this.propagated = propagated;
        }

        @Override
        public IrSetExpr visit(IrSetVar ir, Void a) {
            IrSetVar var = propagated.get(ir);
            return var == null ? ir : var;
        }
    }
}
