package org.clafer.ir.analysis;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import org.clafer.collection.Triple;
import org.clafer.graph.GraphUtil;
import org.clafer.graph.KeyGraph;
import org.clafer.ir.IrBoolDomain;
import org.clafer.ir.IrBoolExpr;
import org.clafer.ir.IrBoolVar;
import org.clafer.ir.IrCompare;
import org.clafer.ir.IrDomain;
import org.clafer.ir.IrIfOnlyIf;
import org.clafer.ir.IrIntConstant;
import org.clafer.ir.IrIntExpr;
import org.clafer.ir.IrIntVar;
import org.clafer.ir.IrModule;
import org.clafer.ir.IrNot;
import org.clafer.ir.IrRewriter;
import org.clafer.ir.IrSetTest;
import org.clafer.ir.IrSetVar;
import org.clafer.ir.IrUtil;
import static org.clafer.ir.Irs.*;

/**
 * @author jimmy
 */
public class Coalescer {

    private Coalescer() {
    }

    public static Triple<Map<IrIntVar, IrIntVar>, Map<IrSetVar, IrSetVar>, IrModule> coalesce(IrModule module) {
        KeyGraph<IrIntVar> intGraph = new KeyGraph<IrIntVar>();
        KeyGraph<IrSetVar> setGraph = new KeyGraph<IrSetVar>();
        Map<IrIntVar, IrIntVar> coalescedInts = new HashMap<IrIntVar, IrIntVar>();
        Map<IrSetVar, IrSetVar> coalescedSets = new HashMap<IrSetVar, IrSetVar>();
        for (IrBoolExpr constraint : module.getConstraints()) {
            if (constraint instanceof IrCompare) {
                IrCompare compare = (IrCompare) constraint;
                IrIntExpr left = compare.getLeft();
                IrIntExpr right = compare.getRight();
                switch (compare.getOp()) {
                    case Equal:
                        if (left instanceof IrIntVar && right instanceof IrIntVar) {
                            intGraph.addUndirectedEdge((IrIntVar) left, (IrIntVar) right);
                        } else if (left instanceof IrIntVar
                                && !IrUtil.isSubsetOf(left.getDomain(), right.getDomain())) {
                            IrIntVar leftVar = (IrIntVar) left;
                            intGraph.addUndirectedEdge(leftVar,
                                    domainInt("Equal " + right.getDomain() + " from " + leftVar.getName(), right.getDomain()));
                        } else if (right instanceof IrIntVar
                                && !IrUtil.isSubsetOf(right.getDomain(), left.getDomain())) {
                            IrIntVar rightVar = (IrIntVar) right;
                            intGraph.addUndirectedEdge(rightVar,
                                    domainInt("Equal " + left.getDomain() + " from " + rightVar.getName(), left.getDomain()));
                        }
                        break;
                    case NotEqual:
                        if (left instanceof IrIntVar && right instanceof IrIntConstant) {
                            IrIntVar leftVar = (IrIntVar) left;
                            IrDomain notRight = IrUtil.difference(leftVar.getDomain(), right.getDomain());
                            intGraph.addUndirectedEdge(leftVar,
                                    domainInt("Remove " + right + " from " + leftVar, notRight));
                        } else if (left instanceof IrIntConstant && right instanceof IrIntVar) {
                            IrIntVar rightVar = (IrIntVar) right;
                            IrDomain notLeft = IrUtil.difference(rightVar.getDomain(), left.getDomain());
                            intGraph.addUndirectedEdge(rightVar,
                                    domainInt("Remove " + left + " from " + rightVar, notLeft));
                        }
                        break;
                    case LessThan:
                        if (left instanceof IrIntVar
                                && left.getDomain().getHighBound() >= right.getDomain().getHighBound()) {
                            IrIntVar leftVar = (IrIntVar) left;
                            IrDomain lessThanLeft = IrUtil.intersection(left.getDomain(), boundDomain(left.getDomain().getLowBound(), right.getDomain().getHighBound() - 1));
                            intGraph.addUndirectedEdge(leftVar,
                                    domainInt("Less than " + right.getDomain() + " from " + leftVar.getName(), lessThanLeft));
                        }
                        if (right instanceof IrIntVar
                                && right.getDomain().getLowBound() <= left.getDomain().getLowBound()) {
                            IrIntVar rightVar = (IrIntVar) right;
                            IrDomain greaterThanRight = IrUtil.intersection(right.getDomain(), boundDomain(left.getDomain().getLowBound() + 1, right.getDomain().getHighBound()));
                            intGraph.addUndirectedEdge(rightVar,
                                    domainInt("Greater than " + left.getDomain() + " from " + rightVar.getName(), greaterThanRight));
                        }
                        break;
                    case LessThanEqual:
                        if (left instanceof IrIntVar
                                && left.getDomain().getHighBound() > right.getDomain().getHighBound()) {
                            IrIntVar leftVar = (IrIntVar) left;
                            IrDomain lessThanEqualLeft = IrUtil.intersection(left.getDomain(), boundDomain(left.getDomain().getLowBound(), right.getDomain().getHighBound()));
                            intGraph.addUndirectedEdge(leftVar,
                                    domainInt("Less than equal " + right.getDomain() + " from " + leftVar.getName(), lessThanEqualLeft));
                        }
                        if (right instanceof IrIntVar
                                && right.getDomain().getLowBound() < left.getDomain().getLowBound()) {
                            IrIntVar rightVar = (IrIntVar) right;
                            IrDomain greaterThanEqualRight = IrUtil.intersection(right.getDomain(), boundDomain(left.getDomain().getLowBound(), right.getDomain().getHighBound()));
                            intGraph.addUndirectedEdge(rightVar,
                                    domainInt("Greater than equal " + left.getDomain() + " from " + rightVar.getName(), greaterThanEqualRight));
                        }
                        break;
                    case GreaterThan:
                        if (left instanceof IrIntVar
                                && left.getDomain().getLowBound() <= right.getDomain().getLowBound()) {
                            IrIntVar leftVar = (IrIntVar) left;
                            IrDomain greaterThanLeft = IrUtil.intersection(left.getDomain(), boundDomain(right.getDomain().getLowBound() + 1, left.getDomain().getHighBound()));
                            intGraph.addUndirectedEdge(leftVar,
                                    domainInt("Greater than " + right.getDomain() + " from " + leftVar.getName(), greaterThanLeft));
                        }
                        if (right instanceof IrIntVar
                                && right.getDomain().getHighBound() >= left.getDomain().getHighBound()) {
                            IrIntVar rightVar = (IrIntVar) right;
                            IrDomain lessThanRight = IrUtil.intersection(right.getDomain(), boundDomain(right.getDomain().getLowBound(), left.getDomain().getHighBound() - 1));
                            intGraph.addUndirectedEdge(rightVar,
                                    domainInt("Less than " + left.getDomain() + " from " + rightVar.getName(), lessThanRight));
                        }
                        break;
                    case GreaterThanEqual:
                        if (left instanceof IrIntVar
                                && left.getDomain().getLowBound() < right.getDomain().getLowBound()) {
                            IrIntVar leftVar = (IrIntVar) left;
                            IrDomain greaterThanLeft = IrUtil.intersection(left.getDomain(), boundDomain(right.getDomain().getLowBound(), left.getDomain().getHighBound()));
                            intGraph.addUndirectedEdge(leftVar,
                                    domainInt("Greater than equal " + right.getDomain() + " from " + leftVar.getName(), greaterThanLeft));
                        }
                        if (right instanceof IrIntVar
                                && right.getDomain().getHighBound() > left.getDomain().getHighBound()) {
                            IrIntVar rightVar = (IrIntVar) right;
                            IrDomain lessThanRight = IrUtil.intersection(right.getDomain(), boundDomain(right.getDomain().getLowBound(), left.getDomain().getHighBound()));
                            intGraph.addUndirectedEdge(rightVar,
                                    domainInt("Less than equal " + left.getDomain() + " from " + rightVar.getName(), lessThanRight));
                        }
                        break;
                }
            } else if (constraint instanceof IrIfOnlyIf) {
                IrIfOnlyIf ifOnlyIf = (IrIfOnlyIf) constraint;
                if (ifOnlyIf.getLeft() instanceof IrBoolVar
                        && ifOnlyIf.getRight() instanceof IrBoolVar) {
                    IrBoolVar left = (IrBoolVar) ifOnlyIf.getLeft();
                    IrBoolVar right = (IrBoolVar) ifOnlyIf.getRight();
                    intGraph.addUndirectedEdge(left, right);
                }
            } else if (constraint instanceof IrBoolVar) {
                IrBoolVar bool = (IrBoolVar) constraint;
                if (IrBoolDomain.BoolDomain.equals(bool.getDomain())) {
                    intGraph.addUndirectedEdge(bool, True);
                }
            } else if (constraint instanceof IrNot) {
                IrNot not = (IrNot) constraint;
                if (not.getExpr() instanceof IrBoolVar) {
                    IrBoolVar bool = (IrBoolVar) not.getExpr();
                    if (IrBoolDomain.BoolDomain.equals(bool.getDomain())) {
                        intGraph.addUndirectedEdge(bool, False);
                    }
                }
            } else if (constraint instanceof IrSetTest) {
                IrSetTest test = (IrSetTest) constraint;
                if (IrSetTest.Op.Equal.equals(test.getOp())
                        && test.getLeft() instanceof IrSetVar
                        && test.getRight() instanceof IrSetVar) {
                    IrSetVar left = (IrSetVar) test.getLeft();
                    IrSetVar right = (IrSetVar) test.getRight();
                    setGraph.addUndirectedEdge(left, right);
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
                        if (!coalesced.equals(coalesce)) {
                            coalescedInts.put(coalesce, coalesced);
                        }
                    }
                }
            }
        }
        for (Set<IrSetVar> component : GraphUtil.computeStronglyConnectedComponents(setGraph)) {
            if (component.size() > 1) {
                Iterator<IrSetVar> iter = component.iterator();
                IrSetVar var = iter.next();
                StringBuilder name = new StringBuilder().append(var.getName());
                IrDomain env = var.getEnv();
                IrDomain ker = var.getKer();
                IrDomain card = var.getCard();
                while (iter.hasNext()) {
                    var = iter.next();
                    name.append(';').append(var.getName());
                    env = IrUtil.intersection(env, var.getEnv());
                    ker = IrUtil.union(ker, var.getKer());
                    card = IrUtil.intersection(card, var.getCard());
                }
                card = IrUtil.intersection(boundDomain(ker.size(), env.size()), card);
                if (!IrUtil.isSubsetOf(ker, env) || card.isEmpty()) {
                    // Model is unsatisfiable. Compile anyways?
                } else {
                    IrSetVar coalesced = set(name.toString(), env, ker, card);
                    for (IrSetVar coalesce : component) {
                        if (!coalesced.equals(coalesce)) {
                            coalescedSets.put(coalesce, coalesced);
                        }
                    }
                }
            }
        }
        return new Triple<Map<IrIntVar, IrIntVar>, Map<IrSetVar, IrSetVar>, IrModule>(
                coalescedInts,
                coalescedSets,
                new CoalesceRewriter(coalescedInts, coalescedSets).rewrite(module, null));
    }

    private static class CoalesceRewriter extends IrRewriter<Void> {

        private final Map<IrIntVar, IrIntVar> coalescedInts;
        private final Map<IrSetVar, IrSetVar> coalescedSets;

        CoalesceRewriter(Map<IrIntVar, IrIntVar> coalescedInts, Map<IrSetVar, IrSetVar> coalescedSets) {
            this.coalescedInts = coalescedInts;
            this.coalescedSets = coalescedSets;
        }

        @Override
        public IrBoolVar visit(IrBoolVar ir, Void a) {
            IrBoolVar var = (IrBoolVar) coalescedInts.get(ir);
            return var == null ? ir : var;
        }

        @Override
        public IrIntVar visit(IrIntVar ir, Void a) {
            IrIntVar var = coalescedInts.get(ir);
            return var == null ? ir : var;
        }

        @Override
        public IrSetVar visit(IrSetVar ir, Void a) {
            IrSetVar var = coalescedSets.get(ir);
            return var == null ? ir : var;
        }
    }
}
