package org.clafer.ir.analysis;

import java.util.HashSet;
import java.util.Set;
import org.clafer.ir.IrBoolExpr;
import org.clafer.ir.IrModule;

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
        Set<IrBoolExpr> constraints = new HashSet<IrBoolExpr>(module.getConstraints());
        return new IrModule().addConstraints(constraints);
    }
}
