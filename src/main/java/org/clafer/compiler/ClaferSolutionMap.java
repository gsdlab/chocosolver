package org.clafer.compiler;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstStringClafer;
import org.clafer.ast.compiler.AstSolutionMap;
import org.clafer.collection.Pair;
import org.clafer.common.Check;
import org.clafer.instance.InstanceClafer;
import org.clafer.instance.InstanceModel;
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
        Map<Pair<AstClafer, Integer>, InstanceClafer> referenceMap = new HashMap<>();
        List<InstanceClafer> topInstances = new ArrayList<>();
        for (AstConcreteClafer child : astSolution.getModel().getChildren()) {
            // [0] because top clafers only have exactly one children set
            IrSetVar topSetIrVar = astSolution.getSiblingVars(child)[0];
            int[] topIds = irSolution.getValue(topSetIrVar);
            for (int topId : topIds) {
                topInstances.add(getInstanceClafer(child, topId, referenceMap));
            }
        }
        return new InstanceModel(topInstances.toArray(new InstanceClafer[topInstances.size()]));
    }

    private InstanceClafer getInstanceClafer(AstConcreteClafer clafer, int id,
            final Map<Pair<AstClafer, Integer>, InstanceClafer> referenceMap) {
        InstanceClafer instanceClafer = getInstanceClaferImpl(clafer, id, referenceMap);
        referenceMap.put(new Pair<>(clafer, id), instanceClafer);
        return instanceClafer;
    }

    private InstanceClafer getInstanceClaferImpl(AstConcreteClafer clafer, int id,
            final Map<Pair<AstClafer, Integer>, InstanceClafer> referenceMap) {
        List<InstanceClafer> children = new ArrayList<>();
        Pair<AstClafer, Object> ref = getInstanceClaferImpl(clafer, id, children, referenceMap);
        InstanceClafer[] childrenArray = children.toArray(new InstanceClafer[children.size()]);
        if (ref == null) {
            return new InstanceClafer(clafer, id, null, childrenArray);
        }
        if (ref.getFst().isPrimitive()) {
            return new InstanceClafer(clafer, id, ref.getSnd(), childrenArray);
        }
        final Pair<AstClafer, Integer> key = new Pair<>(ref.getFst(), (Integer) ref.getSnd());
        if (referenceMap.containsKey(key)) {
            return new InstanceClafer(clafer, id, referenceMap.get(key), childrenArray);
        }
        return new InstanceClafer(clafer, id, null, childrenArray) {

            @Override
            public boolean hasRef() {
                return true;
            }

            @Override
            public Object getRef() {
                return referenceMap.get(key);
            }
        };
    }

    private Pair<AstClafer, Object> getInstanceClaferImpl(AstClafer clafer, int id, List<InstanceClafer> children,
            final Map<Pair<AstClafer, Integer>, InstanceClafer> referenceMap) {
        for (AstConcreteClafer child : clafer.getChildren()) {
            IrSetVar childSetIrVar = astSolution.getSiblingVars(child)[id];
            int[] childIds = irSolution.getValue(childSetIrVar);
            for (int childId : childIds) {
                children.add(getInstanceClafer(child, childId, referenceMap));
            }
        }
        Pair<AstClafer, Object> ref = null;
        if (clafer.hasRef()) {
            AstClafer targetType = clafer.getRef().getTargetType();
            if (targetType instanceof AstStringClafer) {
                IrStringVar refIrVar = astSolution.getRefStrings(clafer.getRef())[id];
                String value = irSolution.getValue(refIrVar);
                ref = new Pair<>(targetType, value);
            } else {
                IrIntVar refIrVar = astSolution.getRefVars(clafer.getRef())[id];
                Integer value = irSolution.getValue(refIrVar);
                if (targetType instanceof AstAbstractClafer) {
                    Pair<AstConcreteClafer, Integer> concreteRef = astSolution.getAnalysis().getConcreteId(
                            targetType, value);
                    targetType = concreteRef.getFst();
                    value = concreteRef.getSnd();
                }
                ref = new Pair<>(targetType, value);
            }
        } else if (clafer.hasSuperClafer()) {
            ref = getInstanceClaferImpl(clafer.getSuperClafer(),
                    id + astSolution.getAnalysis().getOffsets(clafer.getSuperClafer()).getOffset(clafer),
                    children, referenceMap);
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
