package org.clafer.tree;

import static choco.Choco.*;
import choco.kernel.model.Model;
import choco.kernel.model.constraints.Constraint;
import choco.kernel.model.variables.set.SetVariable;
import org.clafer.Check;

/**
 *
 * @author jimmy
 */
public class ClaferConstraint {

    // The constraint is under this clafer
    private final AtomicClafer clafer;
    private final SetConstraint setConstraint;
    private final IntConstraint intConstraint;

    /**
     * No optimized translation.
     * 
     * @param clafer
     * @param setConstraint - Normal translation where "this" is a set that's either a singleton or empty.
     */
    public ClaferConstraint(AtomicClafer clafer, SetConstraint setConstraint) {
        this(clafer, setConstraint, null);
    }

    /**
     * Allow for optimized translation.
     * 
     * @param clafer
     * @param setConstraint - Normal translation where "this" is a set that's either a singleton or empty.
     * @param intConstraint - Optimized translation where "this" is known at compile time.
     */
    public ClaferConstraint(AtomicClafer clafer, SetConstraint setConstraint, IntConstraint intConstraint) {
        this.clafer = Check.notNull(clafer);
        this.setConstraint = Check.notNull(setConstraint);
        this.intConstraint = intConstraint;

        clafer.addConstraint(this);
    }

    public void build(Model model, ThisFactory thisFactory) {
        Card globalCard = clafer.globalCard;
        if (globalCard == null) {
            throw new IllegalStateException("Clafer has not discovered it's global card yet");
        }
        SetVariable s = clafer.getSet();
        int id = s.getLowB();
        if (intConstraint != null) {
            for (; id < globalCard.getLow(); id++) {
                // We know these ids exist so always optimize.
                System.out.println("opt!");
                addCondition(model, intConstraint.apply(thisFactory.newIntThis(id)));
            }
        }
        for (; id <= s.getUppB(); id++) {
            BoolExpr cond = null;
            if (intConstraint != null
                    && (cond = intConstraint.apply(thisFactory.newIntThis(id))) != null
                    // No global constraints means everything goes on the
                    // right hand side of the implies so optimize!
                    && cond.getConstraints().isEmpty()) {
                System.out.println("no globals!");
                addCondition(model, id, cond);
            } else {
                addCondition(model, id, setConstraint.apply(thisFactory.newSetThis(id)));
            }
            // TODO VERY IMPORTANT: We can also optimize cases like this:
            // this.ref = 3
            // 
            // For cases like this the constraints look like:
            // 1. to = new int var
            // 2. eq(to, 3)
            // 3. nth(this, refs, to)
            //
            // Can be optimized to (requires AST)
            // 1. nth(this, refs, 3)
            //
            // However the second form is bad because in the first form
            // we can optimize. nth is a global constraint, eq will only
            // happen on the right end of the implies, so nth being a global
            // constraint will not cause a contradiction (the to variable
            // will equal 0 if "this" doesn't exist).
            //
            // Not sure if worth optimizing
        }
    }

    private void addCondition(Model model, BoolExpr cond) {
        model.addConstraint(and(cond.getValue()));
        for (Constraint condConstraint : cond.getConstraints()) {
            model.addConstraint(condConstraint);
        }
    }

    private void addCondition(Model model, int id, BoolExpr cond) {
        model.addConstraint(implies(eq(clafer.getMembership()[id], 1), and(cond.getValue())));
        for (Constraint condConstraint : cond.getConstraints()) {
            model.addConstraint(condConstraint);
        }
    }
}
