package org.clafer.ast.compiler;

import org.clafer.Check;
import org.clafer.analysis.Analysis;
import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstModel;
import org.clafer.ast.AstRef;
import org.clafer.ast.AstUtil;
import org.clafer.collection.ReadMap;
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
    private final Analysis analysis;

    AstSolutionMap(AstModel model, ReadMap<AstClafer, IrSetVar[]> childrenVars, ReadMap<AstRef, IrIntVar[]> refVars, Analysis analysis) {
        this.model = Check.notNull(model);
        this.childrenVars = childrenVars.readOnly();
        this.refVars = refVars.readOnly();
        this.analysis = analysis;
    }

    public AstModel getModel() {
        return model;
    }

    public IrSetVar[] getChildrenVars(AstClafer clafer) {
        return AstUtil.notNull(clafer + " not part of the AST solution", childrenVars.get(clafer));
    }

    public IrIntVar[] getRefVars(AstRef ref) {
        return AstUtil.notNull(ref + " not part of the AST solution", refVars.get(ref));
    }

    public int getScope(AstClafer clafer) {
        return analysis.getScope().getScope(clafer);
    }

    public int getOffset(AstAbstractClafer sup, AstClafer sub) {
        return analysis.getOffset(sup, sub);
    }
}
