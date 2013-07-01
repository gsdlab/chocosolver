package org.clafer.ir.analysis;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import org.clafer.collection.Triple;
import org.clafer.graph.GraphUtil;
import org.clafer.graph.KeyGraph;
import org.clafer.ir.IrBoolCast;
import org.clafer.ir.IrBoolDomain;
import org.clafer.ir.IrBoolExpr;
import org.clafer.ir.IrBoolNop;
import org.clafer.ir.IrBoolVar;
import org.clafer.ir.IrCompare;
import org.clafer.ir.IrDomain;
import org.clafer.ir.IrIfOnlyIf;
import org.clafer.ir.IrIntExpr;
import org.clafer.ir.IrIntLiteral;
import org.clafer.ir.IrIntNop;
import org.clafer.ir.IrIntVar;
import org.clafer.ir.IrModule;
import org.clafer.ir.IrNot;
import org.clafer.ir.IrRewriter;
import org.clafer.ir.IrUtil;
import static org.clafer.ir.Irs.*;

/**
 * @author jimmy
 */
public class Coalescer {

    private Coalescer() {
    }

    public static Triple<Map<IrBoolVar, IrBoolVar>, Map<IrIntVar, IrIntVar>, IrModule> coalesce(IrModule module) {
        KeyGraph<IrIntVar> intGraph = new KeyGraph<IrIntVar>();
        Map<IrBoolVar, IrBoolVar> coalescedBools = new HashMap<IrBoolVar, IrBoolVar>();
        Map<IrIntVar, IrIntVar> coalescedInts = new HashMap<IrIntVar, IrIntVar>();
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
            } else if (constraint instanceof IrIfOnlyIf) {
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
            } else if (constraint instanceof IrBoolVar) {
                IrBoolVar bool = (IrBoolVar) constraint;
                if (IrBoolDomain.BoolDomain.equals(bool.getDomain())) {
                    coalescedBools.put(bool, True);
                }
            } else if (constraint instanceof IrNot) {
                IrNot not = (IrNot) constraint;
                if (not.getExpr() instanceof IrBoolVar) {
                    IrBoolVar bool = (IrBoolVar) not.getExpr();
                    if (IrBoolDomain.BoolDomain.equals(bool.getDomain())) {
                        coalescedBools.put(bool, False);
                    }
                }
            } else if (constraint instanceof IrBoolCast) {
                IrBoolCast cast = (IrBoolCast) constraint;
                if (cast.getExpr() instanceof IrIntLiteral) {
                    IrIntLiteral intt = (IrIntLiteral) cast.getExpr();
                    IrIntVar constant = cast.isFlipped() ? Zero : One;
                    intGraph.addEdge(intt.getVar(), constant);
                    intGraph.addEdge(constant, intt.getVar());
                }
            }
        }
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

        return new Triple<Map<IrBoolVar, IrBoolVar>, Map<IrIntVar, IrIntVar>, IrModule>(
                coalescedBools,
                coalescedInts,
                new CoalesceRewriter(coalescedBools, coalescedInts).rewrite(module, null));

    }

    private static class CoalesceRewriter extends IrRewriter<Void> {

        private final Map<IrBoolVar, IrBoolVar> coalescedBools;
        private final Map<IrIntVar, IrIntVar> coalescedInts;

        CoalesceRewriter(Map<IrBoolVar, IrBoolVar> coalescedBools, Map<IrIntVar, IrIntVar> coalescedInts) {
            this.coalescedBools = coalescedBools;
            this.coalescedInts = coalescedInts;
        }

        @Override
        public IrBoolExpr visit(IrBoolNop ir, Void a) {
            IrBoolVar var = coalescedBools.get(ir.getVar());
            return var == null ? super.visit(ir, a) : nop(var);
        }

        @Override
        public IrBoolExpr visit(IrIntNop ir, Void a) {
            IrIntVar var = coalescedInts.get(ir.getVar());
            return var == null ? super.visit(ir, a) : nop(var);
        }

        @Override
        public IrBoolExpr visit(IrBoolVar ir, Void a) {
            IrBoolVar var = coalescedBools.get(ir);
            return var == null ? super.visit(ir, a) : var;
        }

        @Override
        public IrIntExpr visit(IrIntLiteral ir, Void a) {
            IrIntVar var = coalescedInts.get(ir.getVar());
            return var == null ? super.visit(ir, a) : $(var);
        }
    }
}
