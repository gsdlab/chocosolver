package org.clafer.compiler;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.chocosolver.solver.Solution;
import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstDecimalClafer;
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
        return getInstance(irSolution);
    }

    public InstanceModel getInstance(Solution solution) {
        return getInstance(irSolution.fromSolution(solution));
    }

    private InstanceModel getInstance(IrSolutionMap irSolution) {
        Map<Pair<AstClafer, Integer>, InstanceClafer> referenceMap = new HashMap<>();
        List<InstanceClafer> topInstances = new ArrayList<>();
        for (AstConcreteClafer child : astSolution.getModel().getChildren()) {
            // [0] because top clafers only have exactly one children set
            IrSetVar topSetIrVar = astSolution.getSiblingVars(child)[0];
            int[] topIds = irSolution.getValue(topSetIrVar);
            for (int topId : topIds) {
                topInstances.add(getInstanceClafer(child, topId, referenceMap, irSolution));
            }
        }
        return new InstanceModel(topInstances.toArray(new InstanceClafer[topInstances.size()]));
    }

    private InstanceClafer getInstanceClafer(AstConcreteClafer clafer, int id,
            final Map<Pair<AstClafer, Integer>, InstanceClafer> referenceMap, IrSolutionMap irSolution) {
        InstanceClafer instanceClafer = getInstanceClaferImpl(clafer, id, referenceMap, irSolution);
        referenceMap.put(new Pair<>(clafer, id), instanceClafer);
        return instanceClafer;
    }

    private InstanceClafer getInstanceClaferImpl(AstConcreteClafer clafer, int id,
            final Map<Pair<AstClafer, Integer>, InstanceClafer> referenceMap, IrSolutionMap irSolution) {
        List<InstanceClafer> children = new ArrayList<>();
        Pair<AstClafer, Object> ref = getInstanceClaferImpl(clafer, id, true, children, referenceMap, irSolution);
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

    /**
     * Adds the children of the clafer and of its super clafers to the list.
     * Returns the reference of the clafer, or the reference it inherits, if
     * withRef holds; a reference redefined by a sub clafer overrides the
     * inherited one, but the inherited children are still added.
     */
    private Pair<AstClafer, Object> getInstanceClaferImpl(AstClafer clafer, int id, boolean withRef,
            List<InstanceClafer> children, final Map<Pair<AstClafer, Integer>, InstanceClafer> referenceMap, IrSolutionMap irSolution) {
        for (AstConcreteClafer child : clafer.getChildren()) {
            IrSetVar childSetIrVar = astSolution.getSiblingVars(child)[id];
            int[] childIds = irSolution.getValue(childSetIrVar);
            for (int childId : childIds) {
                children.add(getInstanceClafer(child, childId, referenceMap, irSolution));
            }
        }
        Pair<AstClafer, Object> ref = null;
        if (withRef && clafer.hasRef()) {
            AstClafer targetType = clafer.getRef().getTargetType();
            if (targetType instanceof AstStringClafer) {
                IrStringVar refIrVar = astSolution.getRefStrings(clafer)[id];
                String value = irSolution.getValue(refIrVar);
                ref = new Pair<>(targetType, value);
            } else if (targetType instanceof AstDecimalClafer) {
                IrIntVar refIrVar = astSolution.getRefVars(clafer)[id];
                int value = irSolution.getValue(refIrVar);
                ref = new Pair<>(targetType, toDecimal(astSolution.getAnalysis().getScope().unscaleDecimal(value)));
            } else {
                IrIntVar refIrVar = astSolution.getRefVars(clafer)[id];
                Integer value = irSolution.getValue(refIrVar);
                if (targetType instanceof AstAbstractClafer) {
                    Pair<AstConcreteClafer, Integer> concreteRef = astSolution.getAnalysis().getConcreteId(
                            targetType, value);
                    targetType = concreteRef.getFst();
                    value = concreteRef.getSnd();
                }
                ref = new Pair<>(targetType, value);
            }
        }
        if (clafer.hasSuperClafer()) {
            Pair<AstClafer, Object> superRef = getInstanceClaferImpl(clafer.getSuperClafer(),
                    id + astSolution.getAnalysis().getOffsets(clafer.getSuperClafer()).getOffset(clafer),
                    withRef && ref == null, children, referenceMap, irSolution);
            if (ref == null) {
                ref = superRef;
            }
        }
        return ref;
    }

    /**
     * The decimal in its shortest form with at least one fractional digit,
     * e.g. 199.0 and 12.5.
     */
    private static BigDecimal toDecimal(BigDecimal value) {
        BigDecimal shortest = value.stripTrailingZeros();
        return shortest.scale() < 1 ? shortest.setScale(1) : shortest;
    }

    public AstSolutionMap getAstSolution() {
        return astSolution;
    }

    public IrSolutionMap getIrSolution() {
        return irSolution;
    }
}
