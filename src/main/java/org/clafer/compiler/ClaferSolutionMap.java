package org.clafer.compiler;

import java.util.ArrayList;
import java.util.List;
import org.clafer.Check;
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
        List<InstanceClafer> childrenInstances = new ArrayList<InstanceClafer>();
        getChildren(astSolution.getModel().getTopClafers(), 0, childrenInstances);
        return new InstanceModel(toArray(childrenInstances));
    }

    private void getChildren(List<AstConcreteClafer> children, int id, List<InstanceClafer> childrenInstances) {
        for (AstClafer child : children) {
            IrSetVar childSetIrVar = astSolution.getChildrenVars(child)[id];
            int[] childIds =
                    childSetIrVar.isConstant()
                    ? childSetIrVar.getValue()
                    : irSolution.getSetVar(childSetIrVar).getValue();
            for (int childId : childIds) {
                int ref = 0;
                if (child.hasRef()) {
                    IrIntVar refIrVar = astSolution.getRefVars(child.getRef())[id];
                    ref = refIrVar.isConstant() ? refIrVar.getValue() : irSolution.getIntVar(refIrVar).getValue();
                }
                List<InstanceClafer> grandChildInstances = new ArrayList<InstanceClafer>();
                getChildren(child.getChildren(), childId, grandChildInstances);
                if (child.hasSuperClafer()) {
                    getChildren(
                            child.getSuperClafer().getChildren(),
                            id + astSolution.getOffset(child.getSuperClafer(), child),
                            grandChildInstances);
                }
                childrenInstances.add(new InstanceClafer(child, childId, ref,
                        toArray(grandChildInstances)));
            }
        }
    }

    private InstanceClafer[] toArray(List<InstanceClafer> instances) {
        return instances.toArray(new InstanceClafer[instances.size()]);
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

//    public Children[] getTopChildren() {
//        List<Children> children = new ArrayList<Children>();
//        getChildren(astSolution.getModel().getTopClafers(), 0, children);
//        return children.toArray(new Children[children.size()]);
//    }
//
//    private void getChildren(AstClafer clafer, int id, List<Children> children) {
//        getChildren(clafer.getChildren(), id, children);
//        if (clafer.hasSuperClafer()) {
//            getChildren(
//                    clafer.getSuperClafer(),
//                    id + astSolution.getOffset(clafer.getSuperClafer(), clafer),
//                    children);
//        }
//    }
//
//    private void getChildren(List<AstConcreteClafer> astChildren, int id, List<Children> children) {
//        for (AstClafer child : astChildren) {
//            IrSetVar childSetIrVar = astSolution.getChildrenVars(child)[id];
//            if (childSetIrVar.isConstant()) {
//                children.add(new Children(child, childSetIrVar.getValue()));
//            } else {
//                SetVar childSetVar = irSolution.getSetVar(childSetIrVar);
//                children.add(new Children(child, childSetVar.getValue()));
//            }
//        }
//    }
    public int getRefScope(AstRef ref) {
        return astSolution.getScope(ref.getSourceType());
    }

    public int getRef(AstRef ref, int id) {
        IrIntVar[] refIrVars = astSolution.getRefVars(ref);
        IntVar refVar = irSolution.getIntVar(refIrVars[id]);
        return refVar.getValue();






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
