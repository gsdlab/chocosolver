package org.clafer.ast.compiler;

import org.clafer.common.Check;
import org.clafer.ast.analysis.Analysis;
import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstConstraint;
import org.clafer.ast.AstException;
import org.clafer.ast.AstModel;
import org.clafer.ast.AstRef;
import org.clafer.collection.Pair;
import org.clafer.collection.ReadMap;
import org.clafer.ir.IrBoolVar;
import org.clafer.ir.IrIntVar;
import org.clafer.ir.IrSetVar;

/**
 *
 * @author jimmy
 */
public class AstSolutionMap {

    private final AstModel model;
    private final ReadMap<AstClafer, IrSetVar[]> childrenVars;
    private final ReadMap<AstRef, IrIntVar[]> refVars;
    private final Pair<AstConstraint, IrBoolVar>[] softVars;
    private final Analysis analysis;

    AstSolutionMap(AstModel model,
            ReadMap<AstClafer, IrSetVar[]> childrenVars,
            ReadMap<AstRef, IrIntVar[]> refVars,
            Pair<AstConstraint, IrBoolVar>[] softVars,
            Analysis analysis) {
        this.model = Check.notNull(model);
        this.childrenVars = childrenVars.readOnly();
        this.refVars = refVars.readOnly();
        this.softVars = Check.noNulls(softVars);
        this.analysis = analysis;
    }

    public AstModel getModel() {
        return model;
    }

    public IrSetVar[] getChildrenVars(AstClafer clafer) {
        return notNull(clafer + " not part of the AST solution", childrenVars.get(clafer));
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

    public int getScope(AstClafer clafer) {
        return analysis.getScope().getScope(clafer);
    }

    public int getOffset(AstAbstractClafer sup, AstClafer sub) {
        return analysis.getOffsets(sup).getOffset(sub);
    }

    public static <T> T notNull(String message, T t) {
        if (t == null) {
            throw new AstException(message);
        }
        return t;
    }
}
