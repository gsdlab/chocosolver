package org.clafer.compiler;

import java.util.ArrayList;
import java.util.List;
import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstStringClafer;
import org.clafer.ast.compiler.AstSolutionMap;
import org.clafer.collection.Pair;
import org.clafer.common.Check;
import org.clafer.instance.InstanceClafer;
import org.clafer.instance.InstanceModel;
import org.clafer.instance.InstanceRef;
import org.clafer.ir.IrIntVar;
import org.clafer.ir.IrSetVar;
import org.clafer.ir.IrStringVar;
import org.clafer.ir.compiler.IrSolutionMap;

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
        List<InstanceClafer> topInstances = new ArrayList<>();
        for (AstConcreteClafer child : astSolution.getModel().getChildren()) {
            // [0] because top clafers only have exactly one children set
            IrSetVar topSetIrVar = astSolution.getSiblingVars(child)[0];
            int[] topIds = irSolution.getSetValue(topSetIrVar);
            for (int topId : topIds) {
                topInstances.add(getInstanceClafer(child, topId));
            }
        }
        return new InstanceModel(topInstances.toArray(new InstanceClafer[topInstances.size()]));
    }

    private InstanceClafer getInstanceClafer(AstConcreteClafer clafer, int id) {
        List<InstanceClafer> children = new ArrayList<>();
        InstanceRef ref = getInstanceClafer(clafer, id, children);
        return new InstanceClafer(clafer, id, ref, children.toArray(new InstanceClafer[children.size()]));
    }

    private InstanceRef getInstanceClafer(AstClafer clafer, int id, List<InstanceClafer> children) {
        for (AstConcreteClafer child : clafer.getChildren()) {
            IrSetVar childSetIrVar = astSolution.getSiblingVars(child)[id];
            int[] childIds = irSolution.getSetValue(childSetIrVar);
            for (int childId : childIds) {
                children.add(getInstanceClafer(child, childId));
            }
        }
        InstanceRef ref = null;
        if (clafer.hasSuperClafer()) {
            ref = getInstanceClafer(clafer.getSuperClafer(),
                    id + astSolution.getAnalysis().getOffsets(clafer.getSuperClafer()).getOffset(clafer),
                    children);
        }
        if (clafer.hasRef()) {
            AstClafer targetType = clafer.getRef().getTargetType();
            if (targetType instanceof AstStringClafer) {
                IrStringVar refIrVar = astSolution.getRefStrings(clafer.getRef())[id];
                int length = irSolution.getIntValue(refIrVar.getLength());
                int[] value = irSolution.getIntValues(refIrVar.getChars());
                ref = new InstanceRef(targetType, new String(value, 0, length));
            } else {
                IrIntVar refIrVar = astSolution.getRefVars(clafer.getRef())[id];
                int value = irSolution.getIntValue(refIrVar);
                if (targetType instanceof AstAbstractClafer) {
                    Pair<AstConcreteClafer, Integer> concreteRef = astSolution.getAnalysis().getConcreteId(
                            targetType, value);
                    targetType = concreteRef.getFst();
                    value = concreteRef.getSnd().intValue();
                }
                ref = new InstanceRef(targetType, value);
            }
        }
        return ref;
    }

    public AstSolutionMap getAstSolution() {
        return astSolution;
    }

    public IrSolutionMap getIrSolution() {
        return irSolution;
    }
}
