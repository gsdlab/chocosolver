package org.clafer.compiler;

import java.util.ArrayList;
import java.util.List;
import org.clafer.common.Check;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstRef;
import org.clafer.ast.compiler.AstSolutionMap;
import org.clafer.instance.InstanceClafer;
import org.clafer.instance.InstanceModel;
import org.clafer.ir.IrIntVar;
import org.clafer.ir.IrSetVar;
import org.clafer.ir.compiler.IrSolutionMap;
import solver.variables.IntVar;

/**
 *
 * @author jimmy
 */
public class ClaferSolutionMap {

    private final AstSolutionMap astSolution;
    private final IrSolutionMap irSolution;

    ClaferSolutionMap(AstSolutionMap astSolution, IrSolutionMap irSolution) {
        this.astSolution = Check.notNull(astSolution);
        this.irSolution = Check.notNull(irSolution);
    }

    public InstanceModel getInstance() {
        List<InstanceClafer> topInstances = new ArrayList<InstanceClafer>();
        for (AstConcreteClafer child : astSolution.getModel().getChildren()) {
            // [0] because top clafers only have exactly one children set
            IrSetVar topSetIrVar = astSolution.getChildrenVars(child)[0];
            int[] topIds = irSolution.getSetValue(topSetIrVar);
            for (int topId : topIds) {
                topInstances.add(getInstanceClafer(child, topId));
            }
        }
        return new InstanceModel(topInstances.toArray(new InstanceClafer[topInstances.size()]));
    }

    private InstanceClafer getInstanceClafer(AstConcreteClafer clafer, int id) {
        List<InstanceClafer> children = new ArrayList<InstanceClafer>();
        Integer ref = getInstanceClafer(clafer, id, children);
        return new InstanceClafer(clafer, id, ref, children.toArray(new InstanceClafer[children.size()]));
    }

    private Integer getInstanceClafer(AstClafer clafer, int id, List<InstanceClafer> children) {
        for (AstConcreteClafer child : clafer.getChildren()) {
            IrSetVar childSetIrVar = astSolution.getChildrenVars(child)[id];
            int[] childIds = irSolution.getSetValue(childSetIrVar);
            for (int childId : childIds) {
                children.add(getInstanceClafer(child, childId));
            }
        }
        Integer ref = null;
        if (clafer.hasSuperClafer()) {
            ref = getInstanceClafer(clafer.getSuperClafer(),
                    id + astSolution.getOffset(clafer.getSuperClafer(), clafer),
                    children);
        }
        if (clafer.hasRef()) {
            IrIntVar refIrVar = astSolution.getRefVars(clafer.getRef())[id];
            ref = irSolution.getIntValue(refIrVar);
        }
        return ref;
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

    public int getRefScope(AstRef ref) {
        return astSolution.getScope(ref.getSourceType());
    }

    public int getRef(AstRef ref, int id) {
        IrIntVar[] refIrVars = astSolution.getRefVars(ref);
        IntVar refVar = irSolution.getIntVar(refIrVars[id]);
        return refVar.getValue();
    }
}
