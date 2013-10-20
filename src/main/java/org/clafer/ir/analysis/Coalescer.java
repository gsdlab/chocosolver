package org.clafer.ir.analysis;

import gnu.trove.iterator.TIntIterator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import org.clafer.collection.Pair;
import org.clafer.collection.Triple;
import org.clafer.graph.GraphUtil;
import org.clafer.graph.KeyGraph;
import org.clafer.ir.IrBoolChannel;
import org.clafer.ir.IrBoolDomain;
import org.clafer.ir.IrBoolExpr;
import org.clafer.ir.IrBoolVar;
import org.clafer.ir.IrCompare;
import static org.clafer.ir.IrCompare.Op.Equal;
import static org.clafer.ir.IrCompare.Op.GreaterThan;
import static org.clafer.ir.IrCompare.Op.GreaterThanEqual;
import static org.clafer.ir.IrCompare.Op.LessThan;
import static org.clafer.ir.IrCompare.Op.LessThanEqual;
import static org.clafer.ir.IrCompare.Op.NotEqual;
import org.clafer.ir.IrDomain;
import org.clafer.ir.IrIfOnlyIf;
import org.clafer.ir.IrIntConstant;
import org.clafer.ir.IrIntExpr;
import org.clafer.ir.IrBoolExprVisitorAdapter;
import org.clafer.ir.IrIntChannel;
import org.clafer.ir.IrIntVar;
import org.clafer.ir.IrMember;
import org.clafer.ir.IrModule;
import org.clafer.ir.IrNot;
import org.clafer.ir.IrRewriter;
import org.clafer.ir.IrSelectN;
import org.clafer.ir.IrSetExpr;
import org.clafer.ir.IrSetTest;
import org.clafer.ir.IrSetVar;
import org.clafer.ir.IrUtil;
import org.clafer.ir.IrWithin;
import static org.clafer.ir.Irs.*;

/**
 * @author jimmy
 */
public class Coalescer {

    private Coalescer() {
    }

    public static Triple<Map<IrIntVar, IrIntVar>, Map<IrSetVar, IrSetVar>, IrModule> coalesce(IrModule module) {
        Pair<KeyGraph<IrIntVar>, KeyGraph<IrSetVar>> graphs = findEquivalences(module.getConstraints());
        KeyGraph<IrIntVar> intGraph = graphs.getFst();
        KeyGraph<IrSetVar> setGraph = graphs.getSnd();
        Map<IrIntVar, IrIntVar> coalescedInts = new HashMap<IrIntVar, IrIntVar>();
        Map<IrSetVar, IrSetVar> coalescedSets = new HashMap<IrSetVar, IrSetVar>();

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

    private static Pair<KeyGraph<IrIntVar>, KeyGraph<IrSetVar>> findEquivalences(Iterable<IrBoolExpr> constraints) {
        KeyGraph<IrIntVar> intGraph = new KeyGraph<IrIntVar>();
        KeyGraph<IrSetVar> setGraph = new KeyGraph<IrSetVar>();
        Pair<KeyGraph<IrIntVar>, KeyGraph<IrSetVar>> pair =
                new Pair<KeyGraph<IrIntVar>, KeyGraph<IrSetVar>>(intGraph, setGraph);
        EquivalenceFinder finder = new EquivalenceFinder();
        for (IrBoolExpr constraint : constraints) {
            constraint.accept(finder, pair);
        }
        return pair;
    }

    private static class EquivalenceFinder extends IrBoolExprVisitorAdapter<Pair<KeyGraph<IrIntVar>, KeyGraph<IrSetVar>>, Void> {

        @Override
        public Void visit(IrBoolVar ir, Pair<KeyGraph<IrIntVar>, KeyGraph<IrSetVar>> a) {
            KeyGraph<IrIntVar> intGraph = a.getFst();
            if (IrBoolDomain.BoolDomain.equals(ir.getDomain())) {
                intGraph.addUndirectedEdge(ir, True);
            }
            return null;
        }

        @Override
        public Void visit(IrNot ir, Pair<KeyGraph<IrIntVar>, KeyGraph<IrSetVar>> a) {
            KeyGraph<IrIntVar> intGraph = a.getFst();
            if (ir.getExpr() instanceof IrBoolVar) {
                IrBoolVar bool = (IrBoolVar) ir.getExpr();
                if (IrBoolDomain.BoolDomain.equals(bool.getDomain())) {
                    intGraph.addUndirectedEdge(bool, False);
                }
            }
            return null;
        }

        @Override
        public Void visit(IrIfOnlyIf ir, Pair<KeyGraph<IrIntVar>, KeyGraph<IrSetVar>> a) {
            KeyGraph<IrIntVar> intGraph = a.getFst();
            if (ir.getLeft() instanceof IrBoolVar
                    && ir.getRight() instanceof IrBoolVar) {
                IrBoolVar left = (IrBoolVar) ir.getLeft();
                IrBoolVar right = (IrBoolVar) ir.getRight();
                intGraph.addUndirectedEdge(left, right);
            }
            return null;
        }

        @Override
        public Void visit(IrWithin ir, Pair<KeyGraph<IrIntVar>, KeyGraph<IrSetVar>> a) {
            KeyGraph<IrIntVar> intGraph = a.getFst();
            if (ir.getValue() instanceof IrIntVar) {
                intGraph.addUndirectedEdge((IrIntVar) ir.getValue(),
                        domainInt("within(" + ir.getRange() + ")", ir.getRange()));
            }
            return null;
        }

        @Override
        public Void visit(IrCompare ir, Pair<KeyGraph<IrIntVar>, KeyGraph<IrSetVar>> a) {
            KeyGraph<IrIntVar> intGraph = a.getFst();
            IrIntExpr left = ir.getLeft();
            IrIntExpr right = ir.getRight();
            switch (ir.getOp()) {
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
            return null;
        }

        @Override
        public Void visit(IrSetTest ir, Pair<KeyGraph<IrIntVar>, KeyGraph<IrSetVar>> a) {
            KeyGraph<IrSetVar> setGraph = a.getSnd();
            if (IrSetTest.Op.Equal.equals(ir.getOp())
                    && ir.getLeft() instanceof IrSetVar
                    && ir.getRight() instanceof IrSetVar) {
                IrSetVar left = (IrSetVar) ir.getLeft();
                IrSetVar right = (IrSetVar) ir.getRight();
                setGraph.addUndirectedEdge(left, right);
            }
            return null;
        }

        @Override
        public Void visit(IrMember ir, Pair<KeyGraph<IrIntVar>, KeyGraph<IrSetVar>> a) {
            KeyGraph<IrSetVar> setGraph = a.getSnd();
            IrSetExpr set = ir.getSet();
            IrDomain env = set.getEnv();
            IrDomain ker = set.getKer();
            IrDomain card = set.getCard();
            Integer constant = IrUtil.getConstant(ir.getElement());
            if (constant != null && set instanceof IrSetVar && !ker.contains(constant)) {
                ker = IrUtil.union(ker, constantDomain(constant));
                card = IrUtil.intersection(boundDomain(ker.size(), env.size()), card);
                if (card.isEmpty()) {
                    // Model is unsatisfiable. Compile anyways?
                } else {
                    setGraph.addEdge((IrSetVar) set, set("member(" + constant + ")", env, ker, card));
                }
            }
            return null;
        }

        @Override
        public Void visit(IrBoolChannel ir, Pair<KeyGraph<IrIntVar>, KeyGraph<IrSetVar>> a) {
            KeyGraph<IrIntVar> intGraph = a.getFst();
            IrBoolExpr[] bools = ir.getBools();
            IrSetExpr set = ir.getSet();
            IrDomain env = set.getEnv();
            IrDomain ker = set.getKer();
            for (int i = 0; i < bools.length; i++) {
                if (bools[i] instanceof IrBoolVar && !IrUtil.isConstant(bools[i])) {
                    if (!env.contains(i)) {
                        intGraph.addEdge((IrBoolVar) bools[i], False);
                    } else if (ker.contains(i)) {
                        intGraph.addEdge((IrBoolVar) bools[i], True);
                    }
                }
            }
            return null;
        }

        @Override
        public Void visit(IrIntChannel ir, Pair<KeyGraph<IrIntVar>, KeyGraph<IrSetVar>> a) {
            KeyGraph<IrIntVar> intGraph = a.getFst();
            IrIntExpr[] ints = ir.getInts();
            IrSetExpr[] sets = ir.getSets();
            for (int i = 0; i < sets.length; i++) {
                TIntIterator iter = sets[i].getKer().iterator();
                while (iter.hasNext()) {
                    int j = iter.next();
                    if (ints[j] instanceof IrIntVar && !IrUtil.isConstant(ints[j])) {
                        intGraph.addEdge((IrIntVar) ints[j], constant(i));
                    }
                }
            }
            return null;
        }

        @Override
        public Void visit(IrSelectN ir, Pair<KeyGraph<IrIntVar>, KeyGraph<IrSetVar>> a) {
            KeyGraph<IrIntVar> intGraph = a.getFst();
            IrBoolExpr[] bools = ir.getBools();
            IrIntExpr n = ir.getN();
            for (int i = 0; i < bools.length; i++) {
                if (n instanceof IrIntVar) {
                    if (IrUtil.isTrue(bools[i]) && i >= n.getDomain().getLowBound()) {
                        intGraph.addEdge((IrIntVar) n, boundInt("selectN(" + i + ")", i + 1, bools.length));
                    } else if (IrUtil.isFalse(bools[i]) && i < n.getDomain().getHighBound()) {
                        intGraph.addEdge((IrIntVar) n, boundInt("selectN(" + i + ")", 0, i));
                    }
                }
            }
            for (int i = 0; i < n.getDomain().getLowBound(); i++) {
                if (bools[i] instanceof IrBoolVar && !IrUtil.isConstant(bools[i])) {
                    intGraph.addEdge((IrBoolVar) bools[i], True);
                }
            }
            for (int i = n.getDomain().getHighBound(); i < bools.length; i++) {
                if (bools[i] instanceof IrBoolVar && !IrUtil.isConstant(bools[i])) {
                    intGraph.addEdge((IrBoolVar) bools[i], False);
                }
            }
            return null;
        }
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
