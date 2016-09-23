package org.clafer.ir.analysis;

import java.util.HashSet;
import java.util.Set;
import org.clafer.collection.Pair;
import org.clafer.ir.IrBoolVar;
import org.clafer.ir.IrExpr;
import org.clafer.ir.IrModule;
import org.clafer.ir.IrRegister;
import org.clafer.ir.IrVar;
import org.clafer.ir.analysis.deduction.Coalesce;
import org.clafer.ir.analysis.deduction.FBBT;

/**
 *
 * @author jimmy
 */
public class Optimizer {

    private Optimizer() {
    }

    public static Pair<Coalesce, IrModule> optimize(IrModule module, boolean coalesceVariables) {
        IrModule optModule = Simplifier.optimize(module);
        Coalesce coalesce = new Coalesce();
        if (coalesceVariables) {
            Pair<Coalesce, IrModule> coalescePair = new FBBT().propagate(optModule);
            coalesce = coalescePair.getFst();
            optModule = coalescePair.getSnd();

            Set<IrVar> processed = new HashSet<>();
            for (IrExpr expr : optModule.getConstraints()) {
                if (expr instanceof IrRegister) {
                    IrVar var = ((IrRegister) expr).getVariable();
                    if (var instanceof IrBoolVar) {
                        IrBoolVar bool = (IrBoolVar) var;
                        bool = coalesce.get(bool);
                        if (!bool.isConstant() && processed.add(bool)) {
                            coalescePair = new FBBT().constructiveDisjunction(bool, bool.negate(), optModule);
                            coalesce = coalesce.compose(coalescePair.getFst());
                            optModule = coalescePair.getSnd();
                        }
                    }
                }
            }

            optModule = DuplicateConstraints.removeDuplicates(optModule);
        }
        optModule = LinearEquationOptimizer.optimize(optModule);
        return new Pair<>(coalesce, optModule);
    }
}
