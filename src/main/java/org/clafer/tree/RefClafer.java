package org.clafer.tree;

import choco.Options;
import static choco.Choco.*;
import choco.kernel.model.Model;
import choco.kernel.model.variables.integer.IntegerVariable;
import choco.kernel.solver.Solver;
import java.io.IOException;
import org.clafer.Check;
import org.clafer.constraint.UniqRefManager;
import org.clafer.constraint.ZeroOutManager;
import org.clafer.tree.analysis.Analysis;

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
        super(Check.notNull("parent cannot be null", parent).getName() + "@Ref");
        this.type = type;
        this.parent = parent;
        this.unique = unique;
        String option = type instanceof IntClafer && type.getScope() > 100 ? Options.V_BOUND : Options.V_ENUM;
        this.refs = makeIntVarArray(getName(), parent.getScope(), type.getScopeLow(), type.getScopeHigh(), option);
    }

    public AtomicClafer getType() {
        return type;
    }

    public IntegerVariable[] getRefs() {
        return refs;
    }

    public int getScopeLow() {
        return type.getScopeLow();
    }

    public int getScopeHigh() {
        return type.getScopeHigh();
    }

    @Override
    protected void build(Model model, Analysis analysis) {
        // TODO: Abstract refs
        // If cardinality is one, then it is already unique even without an explicit constraint.
        if (unique
                && parent instanceof ConcreteClafer
                && ((ConcreteClafer) parent).getCard().getHigh() > 1) {
            model.addConstraint(UniqRefManager.uniqRef(((ConcreteClafer) parent).getParentPointers(), refs));
        } else {
            // Zeroes out any refs that belong to a dead parents to remove bad isomorphisms.
            // The uniqRef constraint does this also as a side effect.
            model.addConstraint(ZeroOutManager.zeroOut(parent.getMembership(), refs));
        }
    }

    @Override
    protected void print(Solver solver, String indent, int parent, Appendable output)
            throws IOException {
        int val = solver.getVar(refs[parent]).getVal();
        output.append(indent).append("ref = ");
        if (type instanceof IntClafer) {
            output.append(Integer.toString(val));
        } else {
            output.append(type.getName() + val);
        }
        output.append('\n');
    }
}
