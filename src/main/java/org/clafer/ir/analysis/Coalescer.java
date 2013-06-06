package org.clafer.ir.analysis;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import org.clafer.collection.Pair;
import org.clafer.graph.GraphUtil;
import org.clafer.graph.KeyGraph;
import org.clafer.ir.IrBoolCast;
import org.clafer.ir.IrBoolExpr;
import org.clafer.ir.IrCompare;
import org.clafer.ir.IrDomain;
import org.clafer.ir.IrIfOnlyIf;
import org.clafer.ir.IrIntExpr;
import org.clafer.ir.IrIntLiteral;
import org.clafer.ir.IrIntVar;
import org.clafer.ir.IrModule;
import org.clafer.ir.IrRewriter;
import org.clafer.ir.IrUtil;
import static org.clafer.ir.Irs.*;

/**
 * @author jimmy
 */
public class Coalescer {

    private Coalescer() {
    }

    public static Pair<Map<IrIntVar, IrIntVar>, IrModule> coalesce(IrModule module) {
        KeyGraph<IrIntVar> intGraph = new KeyGraph<IrIntVar>();
        for (IrBoolExpr constraint : module.getConstraints()) {
            if (constraint instanceof IrCompare) {
                IrCompare compare = (IrCompare) constraint;
                if (IrCompare.Op.Equal.equals(compare.getOp())
                        && compare.getLeft() instanceof IrIntLiteral
                        && compare.getRight() instanceof IrIntLiteral) {
                    IrIntLiteral left = (IrIntLiteral) compare.getLeft();
                    IrIntLiteral right = (IrIntLiteral) compare.getRight();
                    intGraph.addEdge(left.getVar(), right.getVar());
                    intGraph.addEdge(right.getVar(), left.getVar());
                }
            }
            if (constraint instanceof IrIfOnlyIf) {
                IrIfOnlyIf ifOnlyIf = (IrIfOnlyIf) constraint;
                if (ifOnlyIf.getLeft() instanceof IrBoolCast
                        && ifOnlyIf.getRight() instanceof IrBoolCast) {
                    IrBoolCast left = (IrBoolCast) ifOnlyIf.getLeft();
                    IrBoolCast right = (IrBoolCast) ifOnlyIf.getRight();
                    if (left.getExpr() instanceof IrIntLiteral && right.getExpr() instanceof IrIntLiteral) {
                        IrIntLiteral leftExpr = (IrIntLiteral) left.getExpr();
                        IrIntLiteral rightExpr = (IrIntLiteral) right.getExpr();
                        intGraph.addEdge(leftExpr.getVar(), rightExpr.getVar());
                        intGraph.addEdge(rightExpr.getVar(), leftExpr.getVar());
                    }
                }
            }
        }
        Map<IrIntVar, IrIntVar> coalescedInts = new HashMap<IrIntVar, IrIntVar>();
        for (Set<IrIntVar> component : GraphUtil.computeStronglyConnectedComponents(intGraph)) {
            if (component.size() > 1) {
                Iterator<IrIntVar> iter = component.iterator();
                IrIntVar var = iter.next();
                StringBuilder name = new StringBuilder().append(var.getName());
                IrDomain domain = var.getDomain();
                while (iter.hasNext()) {
                    var = iter.next();
                    name.append(';').append(var.getName());
                    domain = IrUtil.intersection(domain, var.getDomain());
                }

                if (domain.isEmpty()) {
                    // Model is unsatisfiable. Compile anyways?
                } else {
                    IrIntVar coalesced = domainInt(name.toString(), domain);
                    for (IrIntVar coalesce : component) {
                        coalescedInts.put(coalesce, coalesced);
                    }
                }
            }
        }

        IrIntVar[] coalescedIntVars = new IrIntVar[module.getIntVars().length];
        for (int i = 0; i < coalescedIntVars.length; i++) {
            IrIntVar intVar = module.getIntVars()[i];
            IrIntVar coalescedIntVar = coalescedInts.get(intVar);
            coalescedIntVars[i] = coalescedIntVar == null ? intVar : coalescedIntVar;
        }
        module.setIntVars(coalescedIntVars);
        return new Pair<Map<IrIntVar, IrIntVar>, IrModule>(
                coalescedInts,
                new CoalesceRewriter(coalescedInts).rewrite(module, null));
    }

    private static class CoalesceRewriter extends IrRewriter<Void> {

        private final Map<IrIntVar, IrIntVar> coalesced;

        CoalesceRewriter(Map<IrIntVar, IrIntVar> coalesced) {
            this.coalesced = coalesced;
        }

        @Override
        public IrIntExpr visit(IrIntLiteral ir, Void a) {
            IrIntVar var = coalesced.get(ir.getVar());
            return var == null ? super.visit(ir, a) : $(var);
        }
    }
}
