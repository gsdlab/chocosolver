package org.clafer.compiler;

import java.util.ArrayList;
import java.util.List;
import org.clafer.Check;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstRef;
import org.clafer.ast.compiler.AstSolutionMap;
import org.clafer.ir.IrIntVar;
import org.clafer.ir.IrSetVar;
import org.clafer.ir.compiler.IrSolutionMap;
import solver.variables.IntVar;
import solver.variables.SetVar;

/**
 *
 * @author jimmy
 */
public class SolutionMap {

    private final AstSolutionMap astSolution;
    private final IrSolutionMap irSolution;

    SolutionMap(AstSolutionMap astSolution, IrSolutionMap irSolution) {
        this.astSolution = Check.notNull(astSolution);
        this.irSolution = Check.notNull(irSolution);
    }

    public AstSolutionMap getAstSolution() {
        return astSolution;
    }

    public IrSolutionMap getIrSolution() {
        return irSolution;
    }

    public int getScope(AstClafer clafer) {
        return astSolution.getScope(clafer);
    }

    public Children[] getTopChildren() {
        List<Children> children = new ArrayList<Children>();
        getChildren(astSolution.getModel().getTopClafers(), 0, children);
        return children.toArray(new Children[children.size()]);
    }

    public Children[] getChildren(AstClafer clafer, int id) {
        List<Children> children = new ArrayList<Children>();
        getChildren(clafer, id, children);
        return children.toArray(new Children[children.size()]);
    }

    private void getChildren(AstClafer clafer, int id, List<Children> children) {
        getChildren(clafer.getChildren(), id, children);
        if (clafer.hasSuperClafer()) {
            getChildren(
                    clafer.getSuperClafer(),
                    id + astSolution.getOffset(clafer.getSuperClafer(), clafer),
                    children);
        }
    }

    private void getChildren(List<AstConcreteClafer> astChildren, int id, List<Children> children) {
        for (AstClafer child : astChildren) {
            IrSetVar childSetIrVar = astSolution.getChildrenVars(child)[id];
            if (childSetIrVar.isConstant()) {
                children.add(new Children(child, childSetIrVar.getValue()));
            } else {
                SetVar childSetVar = irSolution.getSetVar(childSetIrVar);
                children.add(new Children(child, childSetVar.getValue()));
            }
        }
    }

    public int getRefScope(AstRef ref) {
        return astSolution.getScope(ref.getSourceType());
    }

    public int[] getRefs(AstRef ref, int id) {
        IrIntVar[] refIrVars = astSolution.getRefVars(ref);
        int[] refs = new int[refIrVars.length];
        for (int i = 0; i < refs.length; i++) {
            IntVar refVar = irSolution.getIntVar(refIrVars[i]);
            refs[i] = refVar.getValue();
        }
        return refs;
    }

    public static class Children {

        private final AstClafer type;
        private final int[] ids;

        private Children(AstClafer type, int[] ids) {
            this.type = Check.notNull(type);
            this.ids = Check.notNull(ids);
        }

        public AstClafer getType() {
            return type;
        }

        public int[] getIds() {
            return ids;
        }
    }
}
