package org.clafer.tree;

import choco.Choco;
import choco.kernel.model.Model;
import static org.clafer.Exprs.*;
import choco.kernel.model.variables.integer.IntegerVariable;
import choco.kernel.solver.Solver;
import org.clafer.UniqRefManager;

/**
 *
 * @author jimmy
 */
public class RefClafer extends Clafer {

    private final IntegerVariable[] refs;

    public RefClafer(Model model, int low, int high, ConcreteClafer parent) {
        super(parent.getName() + "@Ref");

        this.refs = intArray(getName(), parent.getScope(), low, high);

        for (int i = 0; i < refs.length; i++) {
            model.addConstraint(Choco.implies(Choco.notMember(i, parent.getSet()),
                    Choco.eq(refs[i], 0)));
        }

        if (parent.getCard().getHigh() > 1) {
            model.addConstraint(UniqRefManager.uniqRef(parent.getParentPointers(), refs));
        }

        parent.addChild(this);
    }

    public IntegerVariable[] getRefs() {
        return refs;
    }

    public int getLowB() {
        int low = Choco.MAX_UPPER_BOUND;
        for (IntegerVariable ref : refs) {
            low = Math.min(low, ref.getLowB());
        }
        return low;
    }

    public int getUppB() {
        int high = Choco.MIN_LOWER_BOUND;
        for (IntegerVariable ref : refs) {
            high = Math.max(high, ref.getUppB());
        }
        return high;
    }

    @Override
    protected void print(Solver solver, String indent, int parent) {
        System.out.println(indent + "ref = " + solver.getVar(refs[parent]).getVal());
    }
}
