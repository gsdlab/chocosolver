package solver.constraints;

import solver.variables.BoolVar;

/**
 *
 * @author jimmy
 */
public class Sneak {

    private Sneak() {
    }

    public static void setReify(Constraint c, BoolVar r) {
        c.boolReif = r;
    }

    public static ImplicationConstraint implies(BoolVar antecedent, Constraint consequent) {
        return new ImplicationConstraint(antecedent, consequent, antecedent.getSolver().TRUE);
    }

    public static ImplicationConstraint ifThenElse(BoolVar antecedent,
            Constraint consequent, Constraint alternative) {
        return new ImplicationConstraint(antecedent, consequent, alternative);
    }
}
