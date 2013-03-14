package org.clafer.tree;

import static choco.Choco.*;
import choco.kernel.model.Model;
import choco.kernel.model.variables.integer.IntegerVariable;
import choco.kernel.solver.Solver;
import java.io.IOException;
import org.clafer.Check;
import org.clafer.constraint.UniqRefManager;
import org.clafer.constraint.ZeroOutManager;

/**
 *
 * @author jimmy
 */
public class RefClafer extends Clafer {

    private final boolean unique;
    private final AtomicClafer type;
    private final AtomicClafer parent;
    private final IntegerVariable[] refs;

    public RefClafer(AtomicClafer type, AtomicClafer parent, boolean unique) {
        super(Check.notNull("parent cannot be null", parent).getName() + "@Ref",
                Check.notNull("type cannot be null", type).getScope());
        this.type = type;
        this.parent = parent;
        this.unique = unique;
        this.refs = makeIntVarArray(getName(), parent.getScope(), type.getScopeLow(), type.getScopeHigh());

        parent.setRef(this);
    }

    public AtomicClafer getType() {
        return type;
    }

    public IntegerVariable[] getRefs() {
        return refs;
    }

    @Override
    public int getScopeLow() {
        return type.getScopeLow();
    }

    @Override
    public int getScopeHigh() {
        return type.getScopeHigh();
    }

    @Override
    public void build(Model model) {
        // If cardinality is one, then it is already unique even without an explicit constraint.
        if (unique
                && parent instanceof ConcreteClafer
                && ((ConcreteClafer) parent).getCard().getHigh() > 1) {
            model.addConstraint(UniqRefManager.uniqRef(((ConcreteClafer) parent).getParentPointers(), refs));
        } else {
            // Zeroes out any refs that belong to a dead parents to remove bad isomorphisms.
            // The uniqRef constraint does this also as a side effect.
            model.addConstraint(ZeroOutManager.zeroOut(parent.getSet(), refs));
        }
    }

    @Override
    protected void print(Solver solver, String indent, int parent, Appendable output)
            throws IOException {
        output.append(indent + "ref = " + solver.getVar(refs[parent]).getVal()).append('\n');
    }
}
