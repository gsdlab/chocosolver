package org.clafer.ir.analysis;

import java.util.HashSet;
import java.util.Set;
import org.clafer.ir.IrBoolExpr;
import org.clafer.ir.IrModule;
import org.clafer.ir.IrVar;

/**
 * Due to coalescing, it is possible to end up with multiple cardinality
 * variables for the same set. Remove the duplicates.
 *
 * @author jimmy
 */
public class DuplicateConstraints {

    private DuplicateConstraints() {
    }

    public static IrModule removeDuplicates(IrModule module) {
        Set<IrVar> variables = new HashSet<IrVar>(module.getVariables());
        Set<IrBoolExpr> constraints = new HashSet<IrBoolExpr>(module.getConstraints());
        return new IrModule().addVariables(variables).addConstraints(constraints);
    }
}
