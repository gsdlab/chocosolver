package org.clafer.tree;

import choco.kernel.model.Model;
import static org.clafer.Exprs.*;
import choco.kernel.model.variables.integer.IntegerVariable;
import choco.kernel.solver.Solver;
import java.io.IOException;
import org.clafer.constraint.UniqRefManager;
import org.clafer.constraint.ZeroOutManager;

/**
 *
 * @author jimmy
 */
public class RefClafer extends Clafer {

    private final AtomicClafer type;
    private final IntegerVariable[] refs;

    public RefClafer(Model model, AtomicClafer type, ConcreteClafer parent, boolean unique) {
        super(parent.getName() + "@Ref", type.getScope());

        this.type = type;
        this.refs = intArray(getName(), parent.getScope(), type.getScopeLow(), type.getScopeHigh());

        // If cardinality is one, then it is already unique even without an explicit constraint.
        if (unique && parent.getCard().getHigh() > 1) {
            model.addConstraint(UniqRefManager.uniqRef(parent.getParentPointers(), refs));
        } else {
            // Zeroes out any refs that belong to a dead parents to remove bad isomorphisms.
            // The uniqRef constraint does this also as a side effect.
            model.addConstraint(ZeroOutManager.zeroOut(parent.getSet(), refs));
        }

        parent.setRef(this);
    }

    public AtomicClafer getType() {
        return type;
    }

    public IntegerVariable[] getRefs() {
        return refs;
    }

    @Override
    public int getScopeHigh() {
        return type.getScopeLow();
    }

    @Override
    public int getScopeLow() {
        return type.getScopeHigh();
    }

    @Override
    protected void print(Solver solver, String indent, int parent, Appendable output)
    throws IOException{
        output.append(indent + "ref = " + solver.getVar(refs[parent]).getVal())
                .append('\n');
    }
}
