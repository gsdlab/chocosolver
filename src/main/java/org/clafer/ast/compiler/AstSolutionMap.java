package org.clafer.ast.compiler;

import java.util.Collections;
import java.util.Map;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstConstraint;
import org.clafer.ast.AstException;
import org.clafer.ast.AstModel;
import org.clafer.ast.AstRef;
import org.clafer.ast.analysis.Analysis;
import org.clafer.common.Check;
import org.clafer.ir.IrBoolVar;
import org.clafer.ir.IrIntVar;
import org.clafer.ir.IrSetVar;
import org.clafer.ir.IrStringVar;
import org.clafer.objective.Objective;

/**
 *
 * @author jimmy
 */
public class AstSolutionMap {

    private final AstModel model;
    private final Map<AstClafer, IrSetVar[]> siblingVars;
    private final Map<AstRef, IrIntVar[]> refVars;
    private final Map<AstRef, IrStringVar[]> refStrings;
    private final Map<AstConstraint, IrBoolVar> softVars;
    private final IrIntVar sumSoftVar;
    private final Map<Objective, IrIntVar> objectiveVars;
    private final Analysis analysis;

    AstSolutionMap(AstModel model,
            Map<AstClafer, IrSetVar[]> sibling,
            Map<AstRef, IrIntVar[]> refVars,
            Map<AstRef, IrStringVar[]> refStrings,
            Map<AstConstraint, IrBoolVar> softVars,
            IrIntVar sumSoftVar,
            Map<Objective, IrIntVar> objectiveVars,
            Analysis analysis) {
        this.model = Check.notNull(model);
        this.siblingVars = Check.notNull(sibling);
        this.refVars = Check.notNull(refVars);
        this.refStrings = Check.notNull(refStrings);
        this.softVars = Check.notNull(softVars);
        this.sumSoftVar = Check.notNull(sumSoftVar);
        this.objectiveVars = Check.notNull(objectiveVars);
        this.analysis = Check.notNull(analysis);
    }

    public AstModel getModel() {
        return model;
    }

    public Analysis getAnalysis() {
        return analysis;
    }

    /**
     * Returns the sibling variables associated with the Clafer.
     *
     * @param clafer the Clafer
     * @return the sibling variables associated with the Clafer
     */
    public IrSetVar[] getSiblingVars(AstClafer clafer) {
        return notNull(clafer + " not part of the AST solution", siblingVars.get(clafer));
    }

    /**
     * Returns the reference variables associated to the reference.
     *
     * @param ref the reference
     * @return the reference variables associated to the reference
     */
    public IrIntVar[] getRefVars(AstRef ref) {
        return notNull(ref + " not part of the AST solution", refVars.get(ref));
    }

    public IrStringVar[] getRefStrings(AstRef ref) {
        return notNull(ref + " not part of the AST solution", refStrings.get(ref));
    }

    /**
     * Returns the soft variable associated to the constraint.
     *
     * @param constraint the constraint
     * @return the soft variable associated to the constraint
     */
    public IrBoolVar getSoftVar(AstConstraint constraint) {
        return notNull(constraint + " not a compiled soft constraint", softVars.get(constraint));
    }

    public IrBoolVar[] getSoftVars() {
        return softVars.values().toArray(new IrBoolVar[softVars.size()]);
    }

    public Map<AstConstraint, IrBoolVar> getSoftVarsMap() {
        return Collections.unmodifiableMap(softVars);
    }

    /**
     * Returns the variable equal to the sum of the soft variables.
     *
     * @return the variable equal to the sum of the soft variables
     */
    public IrIntVar getSumSoftVar() {
        return sumSoftVar;
    }

    /**
     * Returns the variable associated to the objective.
     *
     * @param objective the objective
     * @return the variable associated to the objective
     */
    public IrIntVar getObjectiveVar(Objective objective) {
        return notNull(objective + " not a compiled objective", objectiveVars.get(objective));
    }

    private static <T> T notNull(String message, T t) {
        if (t == null) {
            throw new AstException(message);
        }
        return t;
    }
}
