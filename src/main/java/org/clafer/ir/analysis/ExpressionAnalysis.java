package org.clafer.ir.analysis;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import org.clafer.ir.IrBoolExpr;
import org.clafer.ir.IrImplies;
import org.clafer.ir.IrModule;
import org.clafer.ir.Irs;

/**
 *
 * @author jimmy
 */
public class ExpressionAnalysis {

    private ExpressionAnalysis() {
    }

    public static IrModule analyze(IrModule module) {
        List<IrBoolExpr> constraints = new ArrayList<IrBoolExpr>();
        Map<IrBoolExpr, List<IrBoolExpr>> implications = new HashMap<IrBoolExpr, List<IrBoolExpr>>();

        for (IrBoolExpr constraint : module.getConstraints()) {
            if (constraint instanceof IrImplies) {
                IrImplies implies = (IrImplies) constraint;
                List<IrBoolExpr> sameConsequents = implications.get(implies.getAntecedent());
                if (sameConsequents == null) {
                    sameConsequents = new ArrayList<IrBoolExpr>();
                    implications.put(implies.getAntecedent(), sameConsequents);
                }
                sameConsequents.add(implies.getConsequent());
            } else {
                constraints.add(constraint);
            }
        }
        Iterator<Entry<IrBoolExpr, List<IrBoolExpr>>> iter = implications.entrySet().iterator();
        while (iter.hasNext()) {
            Entry<IrBoolExpr, List<IrBoolExpr>> entry = iter.next();
            iter.remove();

            IrBoolExpr antecedent = entry.getKey();
            List<IrBoolExpr> consequents = entry.getValue();
            IrBoolExpr negate = antecedent.negate();
            List<IrBoolExpr> alternatives = implications.get(negate);
            if (!consequents.isEmpty() && alternatives != null) {
                /**
                 * What is this optimization?
                 *
                 * Negative expressions are generally less efficient. Choose a
                 * the positive antecedent.
                 */
                if (!antecedent.isNegative()) {
                    constraints.add(Irs.ifThenElse(antecedent, Irs.and(consequents), Irs.and(alternatives)));
                } else {
                    constraints.add(Irs.ifThenElse(negate, Irs.and(alternatives), Irs.and(consequents)));
                }
                consequents.clear();
                alternatives.clear();
            } else {
                constraints.add(Irs.implies(antecedent, Irs.and(consequents)));
            }
        }
        return module.withConstraints(constraints);
    }
}
