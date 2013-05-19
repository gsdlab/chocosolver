package org.clafer.ir.analysis;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import org.clafer.ir.IrBoolConstraint;
import org.clafer.ir.IrBoolExpr;
import org.clafer.ir.IrConstraint;
import org.clafer.ir.IrImplies;
import org.clafer.ir.IrModule;
import org.clafer.ir.Irs;

/**
 *
 * @author jimmy
 */
public class ExpressionAnalysis {

    private static IrImplies getImplies(IrConstraint constraint) {
        if (constraint instanceof IrBoolConstraint) {
            IrBoolConstraint boolConstraint = (IrBoolConstraint) constraint;
            IrBoolExpr expr = boolConstraint.getExpr();
            if (expr instanceof IrImplies) {
                return (IrImplies) expr;
            }
        }
        return null;
    }

    public static IrModule analyze(IrModule module) {
        List<IrConstraint> constraints = new ArrayList<IrConstraint>();
        Map<IrBoolExpr, List<IrBoolExpr>> implications = new HashMap<IrBoolExpr, List<IrBoolExpr>>();

        for (IrConstraint constraint : module.getConstraints()) {
            IrImplies implies = getImplies(constraint);
            if (implies == null) {
                constraints.add(constraint);
            } else {
                List<IrBoolExpr> sameConsequents = implications.get(implies.getAntecedent());
                if (sameConsequents == null) {
                    sameConsequents = new ArrayList<IrBoolExpr>();
                    implications.put(implies.getAntecedent(), sameConsequents);
                }
                sameConsequents.add(implies.getConsequent());
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
                    constraints.add(Irs.boolConstraint(
                            Irs.ifThenElse(antecedent, Irs.and(consequents), Irs.and(alternatives))));
                } else {
                    constraints.add(Irs.boolConstraint(
                            Irs.ifThenElse(negate, Irs.and(alternatives), Irs.and(consequents))));
                }
                consequents.clear();
                alternatives.clear();
            } else {
                constraints.add(Irs.boolConstraint(
                        Irs.implies(antecedent, Irs.and(consequents))));
            }
        }
        return module.withConstraints(constraints);
    }
}
