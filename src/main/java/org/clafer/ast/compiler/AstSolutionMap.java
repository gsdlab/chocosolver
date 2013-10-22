package org.clafer.ast.compiler;

import java.util.Map;
import org.clafer.common.Check;
import org.clafer.ast.analysis.Analysis;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstConstraint;
import org.clafer.ast.AstException;
import org.clafer.ast.AstModel;
import org.clafer.ast.AstRef;
import org.clafer.collection.Pair;
import org.clafer.ir.IrBoolVar;
import org.clafer.ir.IrIntVar;
import org.clafer.ir.IrSetVar;

/**
 *
 * @author jimmy
 */
public class AstSolutionMap {

    private final AstModel model;
    private final Map<AstClafer, IrSetVar[]> siblingVars;
    private final Map<AstRef, IrIntVar[]> refVars;
    private final Pair<AstConstraint, IrBoolVar>[] softVars;
    private final IrIntVar sumSoftVars;
    private final Analysis analysis;

    AstSolutionMap(AstModel model,
            Map<AstClafer, IrSetVar[]> sibling,
            Map<AstRef, IrIntVar[]> refVars,
            Pair<AstConstraint, IrBoolVar>[] softVars,
            IrIntVar sumSoftVars,
            Analysis analysis) {
        this.model = Check.notNull(model);
        this.siblingVars = Check.notNull(sibling);
        this.refVars = Check.notNull(refVars);
        this.softVars = Check.noNulls(softVars);
        this.sumSoftVars = Check.notNull(sumSoftVars);
        this.analysis = Check.notNull(analysis);
    }

    public AstModel getModel() {
        return model;
    }

    public Analysis getAnalysis() {
        return analysis;
    }

    public IrSetVar[] getSiblingVars(AstClafer clafer) {
        return notNull(clafer + " not part of the AST solution", siblingVars.get(clafer));
    }

    public IrIntVar[] getRefVars(AstRef ref) {
        return notNull(ref + " not part of the AST solution", refVars.get(ref));
    }

    /**
     * Returns the soft variables and their corresponding constraints.
     *
     * @return the soft variables and their corresponding constraints
     */
    public Pair<AstConstraint, IrBoolVar>[] getSoftVars() {
        return softVars;
    }

    /**
     * Returns the variable equal to the sum of the soft variables.
     *
     * @return the variable equal to the sum of the soft variables
     */
    public IrIntVar getSumSoftVars() {
        return sumSoftVars;
    }

    private static <T> T notNull(String message, T t) {
        if (t == null) {
            throw new AstException(message);
        }
        return t;
    }
}
