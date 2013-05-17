package org.clafer.ir.analysis;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.clafer.ir.IrBoolConstraint;
import org.clafer.ir.IrBoolExpr;
import org.clafer.ir.IrConstraint;
import org.clafer.ir.IrExpr;
import org.clafer.ir.IrImplies;
import org.clafer.ir.IrModule;
import org.clafer.ir.Irs;

/**
 *
 * @author jimmy
 */
public class ExpressionAnalysis {
    
    public static IrModule analyze(IrModule module) {
        List<IrConstraint> constraints = new ArrayList<IrConstraint>();
        Map<IrExpr, IrImplies> implications = new HashMap<IrExpr, IrImplies>();

        for (IrConstraint constraint : module.getConstraints()) {
            if (constraint instanceof IrBoolConstraint) {
                IrBoolExpr expr = ((IrBoolConstraint) constraint).getExpr();
                if (expr instanceof IrImplies) {
                    IrImplies implies = (IrImplies) expr;
                    IrImplies negate = implications.remove(implies.getAntecedent().negate());
                    if (negate == null) {
                        IrImplies old = implications.put(implies.getAntecedent(), implies);
                        if (old != null) {
                            constraints.add(Irs.boolConstraint(old));
                        }
                    } else {
                        /**
                         * What is this optimization?
                         * 
                         * Negative expressions are generally less efficient. Choose a
                         * the positive antecedent.
                         */
                        if (!implies.getAntecedent().isNegative()) {
                            constraints.add(Irs.boolConstraint(
                                    Irs.ifThenElse(implies.getAntecedent(), implies.getConsequent(), negate.getConsequent())));
                        } else {
                            constraints.add(Irs.boolConstraint(
                                    Irs.ifThenElse(negate.getAntecedent(), negate.getConsequent(), implies.getConsequent())));
                        }
                    }
                } else {
                    constraints.add(constraint);
                }
            } else {
                constraints.add(constraint);
            }
        }
        for (IrImplies implication : implications.values()) {
            constraints.add(Irs.boolConstraint(implication));
        }
        return module.withConstraints(constraints);
    }
}
