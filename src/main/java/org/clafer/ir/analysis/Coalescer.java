package org.clafer.ir.analysis;

import gnu.trove.iterator.TIntIterator;
import gnu.trove.set.TIntSet;
import gnu.trove.set.hash.TIntHashSet;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import org.clafer.collection.Pair;
import org.clafer.collection.Triple;
import org.clafer.graph.GraphUtil;
import org.clafer.graph.KeyGraph;
import org.clafer.ir.IrAdd;
import org.clafer.ir.IrArrayToSet;
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
import org.clafer.ir.IrFilterString;
import org.clafer.ir.IrIntChannel;
import org.clafer.ir.IrIntVar;
import org.clafer.ir.IrJoinFunction;
import org.clafer.ir.IrJoinRelation;
import org.clafer.ir.IrMember;
import org.clafer.ir.IrModule;
import org.clafer.ir.IrNot;
import org.clafer.ir.IrNotImplies;
import org.clafer.ir.IrRewriter;
import org.clafer.ir.IrSelectN;
import org.clafer.ir.IrSetExpr;
import org.clafer.ir.IrSetTest;
import org.clafer.ir.IrSetUnion;
import org.clafer.ir.IrSetVar;
import org.clafer.ir.IrSingleton;
import org.clafer.ir.IrSortSets;
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
                if (!IrUtil.isSubsetOf(ker, env) || ker.size() > env.size()) {
                    // Model is unsatisfiable. Compile anyways?
                } else {
                    card = IrUtil.intersection(boundDomain(ker.size(), env.size()), card);
                    if (card.isEmpty()) {
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
        }
        return new Triple<Map<IrIntVar, IrIntVar>, Map<IrSetVar, IrSetVar>, IrModule>(
                coalescedInts,
                coalescedSets,
                new CoalesceRewriter(coalescedInts, coalescedSets).rewrite(module, null));
    }

    private static void propagateDomain(IrDomain left, IrIntExpr right, KeyGraph<IrIntVar> intGraph) {
        if (left.equals(right.getDomain())) {
            return;
        }
        if (right instanceof IrIntVar) {
            IrDomain domain = IrUtil.intersection(left, right.getDomain());
            if (!domain.equals(right.getDomain())) {
                intGraph.addUndirectedEdge((IrIntVar) right,
                        domainInt("domain" + domain, domain));
            }
        } else if (right instanceof IrAdd) {
            IrAdd add = (IrAdd) right;
            IrIntExpr[] addends = add.getAddends();
            if (addends.length == 1) {
                propagateDomain(IrUtil.offset(left, -add.getOffset()), addends[0], intGraph);
            }
        }
    }

    private static void propagateEnv(IrDomain left, IrSetExpr right, KeyGraph<IrIntVar> intGraph, KeyGraph<IrSetVar> setGraph) {
        if (left.equals(right.getEnv())) {
            return;
        }
        if (right instanceof IrSetVar) {
            IrDomain env = IrUtil.intersection(left, right.getEnv());
            if (!env.equals(right.getEnv())) {
                setGraph.addUndirectedEdge((IrSetVar) right, set("env" + env, env));
            }
        } else if (right instanceof IrSingleton) {
            propagateDomain(left, ((IrSingleton) right).getValue(), intGraph);
        } else if (right instanceof IrArrayToSet) {
            propagateEnvArrayToSet(left, (IrArrayToSet) right, intGraph);
        } else if (right instanceof IrJoinFunction) {
            propagateEnvJoinFunction(left, (IrJoinFunction) right, intGraph);
        } else if (right instanceof IrSetUnion) {
            propagateEnvSetUnion(left, (IrSetUnion) right, intGraph, setGraph);
        }
    }

    private static void propagateEnvArrayToSet(IrDomain left, IrArrayToSet right, KeyGraph<IrIntVar> intGraph) {
        for (IrIntExpr child : right.getArray()) {
            propagateDomain(left, child, intGraph);
        }
    }

    private static void propagateEnvJoinFunction(IrDomain left, IrJoinFunction right, KeyGraph<IrIntVar> intGraph) {
        TIntIterator iter = right.getTake().getKer().iterator();
        while (iter.hasNext()) {
            propagateDomain(left, right.getRefs()[iter.next()], intGraph);
        }
    }

    private static void propagateEnvSetUnion(IrDomain left, IrSetUnion right, KeyGraph<IrIntVar> intGraph, KeyGraph<IrSetVar> setGraph) {
        for (IrSetExpr operand : right.getOperands()) {
            propagateEnv(left, operand, intGraph, setGraph);
        }
    }

    private static void propagateCard(IrDomain left, IrSetExpr right, KeyGraph<IrIntVar> intGraph, KeyGraph<IrSetVar> setGraph) {
        if (left.equals(right.getCard())) {
            return;
        }
        if (right instanceof IrSetVar) {
            IrDomain card = IrUtil.intersection(left, right.getCard());
            if (!card.equals(right.getCard())) {
                setGraph.addUndirectedEdge(
                        (IrSetVar) right,
                        set("card" + card, right.getEnv(), EmptyDomain, card));
            }
        } else if (right instanceof IrJoinRelation) {
            propagateCardJoinRelation(left, (IrJoinRelation) right, intGraph, setGraph);
        } else if (right instanceof IrJoinFunction) {
            propagateCardJoinFunction(left, (IrJoinFunction) right, intGraph, setGraph);
        } else if (right instanceof IrSetUnion) {
            propagateCardSetUnion(left, (IrSetUnion) right, intGraph, setGraph);
        }
    }

    private static void propagateCardJoinRelation(IrDomain left, IrJoinRelation right, KeyGraph<IrIntVar> intGraph, KeyGraph<IrSetVar> setGraph) {
        if (right.isInjective()) {
            IrSetExpr take = right.getTake();
            IrSetExpr[] children = right.getChildren();
            int lb = left.getLowBound();
            int ub = left.getHighBound();
            int[] envLbs = new int[take.getEnv().size() - take.getKer().size()];
            int[] envUbs = new int[envLbs.length];
            int kerMinCard = 0;
            int kerMaxCard = 0;
            int env = 0;
            TIntIterator iter = take.getEnv().iterator();
            while (iter.hasNext()) {
                int i = iter.next();
                if (take.getKer().contains(i)) {
                    kerMinCard += children[i].getCard().getLowBound();
                    kerMaxCard += children[i].getCard().getHighBound();
                } else {
                    envLbs[env] = children[i].getCard().getLowBound();
                    envUbs[env] = children[i].getCard().getHighBound();
                    env++;
                }
            }
            Arrays.sort(envLbs);
            Arrays.sort(envUbs);
            int i;
            for (i = 0; i < envLbs.length && (kerMinCard < ub || envLbs[i] == 0); i++) {
                kerMinCard += envLbs[i];
            }
            int high = i + take.getKer().size();
            for (i = envUbs.length - 1; i >= 0 && kerMaxCard < lb; i--) {
                kerMaxCard += envUbs[i];
            }
            int low = envUbs.length - 1 - i + take.getKer().size();
            if (low > take.getCard().getLowBound() || high < take.getCard().getHighBound()) {
                propagateCard(boundDomain(low, high), take, intGraph, setGraph);
            }
        }
    }

    private static void propagateCardJoinFunction(IrDomain left, IrJoinFunction right, KeyGraph<IrIntVar> intGraph, KeyGraph<IrSetVar> setGraph) {
        IrSetExpr take = right.getTake();
        int low = Math.max(take.getKer().size(), left.getLowBound());
        int high = Math.min(take.getEnv().size(),
                right.hasGlobalCardinality()
                ? left.getHighBound() * right.getGlobalCardinality()
                : take.getCard().getHighBound());
        if (low > take.getCard().getLowBound() || high < take.getCard().getHighBound()) {
            propagateCard(boundDomain(low, high), take, intGraph, setGraph);
        }
    }

    private static void propagateCardSetUnion(IrDomain left, IrSetUnion right, KeyGraph<IrIntVar> intGraph, KeyGraph<IrSetVar> setGraph) {
        for (IrSetExpr operand : right.getOperands()) {
            propagateCard(boundDomain(0, left.getHighBound()), operand, intGraph, setGraph);
        }
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

    private static class EquivalenceFinder extends IrBoolExprVisitorAdapter<Pair<KeyGraph<IrIntVar>, KeyGraph<IrSetVar>>, Boolean> {

        private final Map<IrSetVar, IrIntVar> duplicates = new HashMap<IrSetVar, IrIntVar>();

        @Override
        public Boolean visit(IrBoolVar ir, Pair<KeyGraph<IrIntVar>, KeyGraph<IrSetVar>> a) {
            KeyGraph<IrIntVar> intGraph = a.getFst();
            if (IrBoolDomain.BoolDomain.equals(ir.getDomain())) {
                intGraph.addUndirectedEdge(ir, True);
            }
            return null;
        }

        @Override
        public Boolean visit(IrNot ir, Pair<KeyGraph<IrIntVar>, KeyGraph<IrSetVar>> a) {
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
        public Boolean visit(IrNotImplies ir, Pair<KeyGraph<IrIntVar>, KeyGraph<IrSetVar>> a) {
            KeyGraph<IrIntVar> intGraph = a.getFst();
            if (ir.getAntecedent() instanceof IrBoolVar) {
                intGraph.addUndirectedEdge((IrBoolVar) ir.getAntecedent(), True);
            }
            if (ir.getConsequent() instanceof IrBoolVar) {
                intGraph.addUndirectedEdge((IrBoolVar) ir.getConsequent(), False);
            }
            return null;
        }

        @Override
        public Boolean visit(IrIfOnlyIf ir, Pair<KeyGraph<IrIntVar>, KeyGraph<IrSetVar>> a) {
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
        public Boolean visit(IrWithin ir, Pair<KeyGraph<IrIntVar>, KeyGraph<IrSetVar>> a) {
            KeyGraph<IrIntVar> intGraph = a.getFst();
            if (ir.getValue() instanceof IrIntVar) {
                intGraph.addUndirectedEdge((IrIntVar) ir.getValue(),
                        domainInt("within" + ir.getRange(), ir.getRange()));
            }
            return null;
        }

        @Override
        public Boolean visit(IrCompare ir, Pair<KeyGraph<IrIntVar>, KeyGraph<IrSetVar>> a) {
            KeyGraph<IrIntVar> intGraph = a.getFst();
            KeyGraph<IrSetVar> setGraph = a.getSnd();
            IrIntExpr left = ir.getLeft();
            IrIntExpr right = ir.getRight();
            switch (ir.getOp()) {
                case Equal:
                    Pair<IrIntExpr, IrSetVar> cardinality = AnalysisUtil.getAssignCardinality(ir);
                    if (cardinality != null) {
                        IrIntExpr cardExpr = cardinality.getFst();
                        IrSetVar setVar = cardinality.getSnd();

                        if (cardExpr instanceof IrIntVar) {
                            IrIntVar cardVar = (IrIntVar) cardExpr;
                            IrIntVar duplicate = duplicates.put(setVar, cardVar);
                            if (duplicate != null) {
                                intGraph.addUndirectedEdge(cardVar, duplicate);
                                return null;
                            }
                        }
                        if (!cardExpr.getDomain().equals(setVar.getCard())) {
                            propagateCard(cardExpr.getDomain(), setVar, intGraph, setGraph);
                        }
                    } else if (left instanceof IrIntVar && right instanceof IrIntVar) {
                        intGraph.addUndirectedEdge((IrIntVar) left, (IrIntVar) right);
                    } else if (left instanceof IrIntVar
                            && !IrUtil.isSubsetOf(left.getDomain(), right.getDomain())) {
                        IrIntVar leftVar = (IrIntVar) left;
                        intGraph.addUndirectedEdge(leftVar,
                                domainInt("equal" + right.getDomain(), right.getDomain()));
                    } else if (right instanceof IrIntVar
                            && !IrUtil.isSubsetOf(right.getDomain(), left.getDomain())) {
                        IrIntVar rightVar = (IrIntVar) right;
                        intGraph.addUndirectedEdge(rightVar,
                                domainInt("equal" + left.getDomain(), left.getDomain()));
                    }
                    break;
                case NotEqual:
                    if (left instanceof IrIntVar && right instanceof IrIntConstant) {
                        IrIntVar leftVar = (IrIntVar) left;
                        IrDomain notRight = IrUtil.difference(leftVar.getDomain(), right.getDomain());
                        intGraph.addUndirectedEdge(leftVar,
                                domainInt("notEqual" + right.getDomain(), notRight));
                    } else if (left instanceof IrIntConstant && right instanceof IrIntVar) {
                        IrIntVar rightVar = (IrIntVar) right;
                        IrDomain notLeft = IrUtil.difference(rightVar.getDomain(), left.getDomain());
                        intGraph.addUndirectedEdge(rightVar,
                                domainInt("notEqual" + left.getDomain(), notLeft));
                    }
                    break;
                case LessThan:
                    if (left instanceof IrIntVar
                            && left.getDomain().getHighBound() >= right.getDomain().getHighBound()) {
                        IrIntVar leftVar = (IrIntVar) left;
                        IrDomain lessThanLeft = IrUtil.intersection(left.getDomain(), boundDomain(left.getDomain().getLowBound(), right.getDomain().getHighBound() - 1));
                        intGraph.addUndirectedEdge(leftVar,
                                domainInt("lessThan" + right.getDomain(), lessThanLeft));
                    }
                    if (right instanceof IrIntVar
                            && right.getDomain().getLowBound() <= left.getDomain().getLowBound()) {
                        IrIntVar rightVar = (IrIntVar) right;
                        IrDomain greaterThanRight = IrUtil.intersection(right.getDomain(), boundDomain(left.getDomain().getLowBound() + 1, right.getDomain().getHighBound()));
                        intGraph.addUndirectedEdge(rightVar,
                                domainInt("greaterThan" + left.getDomain(), greaterThanRight));
                    }
                    break;
                case LessThanEqual:
                    if (left instanceof IrIntVar
                            && left.getDomain().getHighBound() > right.getDomain().getHighBound()) {
                        IrIntVar leftVar = (IrIntVar) left;
                        IrDomain lessThanEqualLeft = IrUtil.intersection(left.getDomain(), boundDomain(left.getDomain().getLowBound(), right.getDomain().getHighBound()));
                        intGraph.addUndirectedEdge(leftVar,
                                domainInt("lessThanEqual" + right.getDomain(), lessThanEqualLeft));
                    }
                    if (right instanceof IrIntVar
                            && right.getDomain().getLowBound() < left.getDomain().getLowBound()) {
                        IrIntVar rightVar = (IrIntVar) right;
                        IrDomain greaterThanEqualRight = IrUtil.intersection(right.getDomain(), boundDomain(left.getDomain().getLowBound(), right.getDomain().getHighBound()));
                        intGraph.addUndirectedEdge(rightVar,
                                domainInt("greaterThanEqual" + left.getDomain(), greaterThanEqualRight));
                    }
                    break;
                case GreaterThan:
                    if (left instanceof IrIntVar
                            && left.getDomain().getLowBound() <= right.getDomain().getLowBound()) {
                        IrIntVar leftVar = (IrIntVar) left;
                        IrDomain greaterThanLeft = IrUtil.intersection(left.getDomain(), boundDomain(right.getDomain().getLowBound() + 1, left.getDomain().getHighBound()));
                        intGraph.addUndirectedEdge(leftVar,
                                domainInt("greaterThan" + right.getDomain(), greaterThanLeft));
                    }
                    if (right instanceof IrIntVar
                            && right.getDomain().getHighBound() >= left.getDomain().getHighBound()) {
                        IrIntVar rightVar = (IrIntVar) right;
                        IrDomain lessThanRight = IrUtil.intersection(right.getDomain(), boundDomain(right.getDomain().getLowBound(), left.getDomain().getHighBound() - 1));
                        intGraph.addUndirectedEdge(rightVar,
                                domainInt("lessThan" + left.getDomain(), lessThanRight));
                    }
                    break;
                case GreaterThanEqual:
                    if (left instanceof IrIntVar
                            && left.getDomain().getLowBound() < right.getDomain().getLowBound()) {
                        IrIntVar leftVar = (IrIntVar) left;
                        IrDomain greaterThanLeft = IrUtil.intersection(left.getDomain(), boundDomain(right.getDomain().getLowBound(), left.getDomain().getHighBound()));
                        intGraph.addUndirectedEdge(leftVar,
                                domainInt("greaterThanEqual" + right.getDomain(), greaterThanLeft));
                    }
                    if (right instanceof IrIntVar
                            && right.getDomain().getHighBound() > left.getDomain().getHighBound()) {
                        IrIntVar rightVar = (IrIntVar) right;
                        IrDomain lessThanRight = IrUtil.intersection(right.getDomain(), boundDomain(right.getDomain().getLowBound(), left.getDomain().getHighBound()));
                        intGraph.addUndirectedEdge(rightVar,
                                domainInt("lessThanEqual" + left.getDomain(), lessThanRight));
                    }
                    break;
            }
            return null;
        }

        @Override
        public Boolean visit(IrSetTest ir, Pair<KeyGraph<IrIntVar>, KeyGraph<IrSetVar>> a) {
            KeyGraph<IrIntVar> intGraph = a.getFst();
            KeyGraph<IrSetVar> setGraph = a.getSnd();
            IrSetExpr left = ir.getLeft();
            IrSetExpr right = ir.getRight();
            if (IrSetTest.Op.Equal.equals(ir.getOp())) {
                if (ir.getLeft() instanceof IrSetVar
                        && ir.getRight() instanceof IrSetVar) {
                    setGraph.addUndirectedEdge((IrSetVar) left, (IrSetVar) right);
                } else {
                    if (!left.getEnv().equals(right.getEnv())) {
                        propagateEnv(left.getEnv(), right, intGraph, setGraph);
                        propagateEnv(right.getEnv(), left, intGraph, setGraph);
                    }
                    if (!left.getCard().equals(right.getCard())) {
                        propagateCard(left.getCard(), right, intGraph, setGraph);
                        propagateCard(right.getCard(), left, intGraph, setGraph);
                    }
                }
            }
            return null;
        }

        @Override
        public Boolean visit(IrMember ir, Pair<KeyGraph<IrIntVar>, KeyGraph<IrSetVar>> a) {
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
                    setGraph.addUndirectedEdge((IrSetVar) set, set("member" + constant, env, ker, card));
                }
            }
            return null;
        }

        @Override
        public Boolean visit(IrBoolChannel ir, Pair<KeyGraph<IrIntVar>, KeyGraph<IrSetVar>> a) {
            KeyGraph<IrIntVar> intGraph = a.getFst();
            KeyGraph<IrSetVar> setGraph = a.getSnd();
            IrBoolExpr[] bools = ir.getBools();
            IrSetExpr set = ir.getSet();
            IrDomain env = set.getEnv();
            IrDomain ker = set.getKer();
            TIntSet notFalses = new TIntHashSet();
            env.transferTo(notFalses);
            boolean changed = false;
            for (int i = 0; i < bools.length; i++) {
                if (bools[i] instanceof IrBoolVar && !IrUtil.isConstant(bools[i])) {
                    if (!env.contains(i)) {
                        intGraph.addUndirectedEdge((IrBoolVar) bools[i], False);
                    } else if (ker.contains(i)) {
                        intGraph.addUndirectedEdge((IrBoolVar) bools[i], True);
                    }
                }
                if (IrUtil.isFalse(bools[i])) {
                    changed |= notFalses.remove(i);
                }
            }
            if (changed) {
                propagateEnv(enumDomain(notFalses), set, intGraph, setGraph);
            }
            return null;
        }

        @Override
        public Boolean visit(IrIntChannel ir, Pair<KeyGraph<IrIntVar>, KeyGraph<IrSetVar>> a) {
            KeyGraph<IrIntVar> intGraph = a.getFst();
            KeyGraph<IrSetVar> setGraph = a.getSnd();
            IrIntExpr[] ints = ir.getInts();
            IrSetExpr[] sets = ir.getSets();

            TIntSet kers = new TIntHashSet();

            for (int i = 0; i < ints.length; i++) {
                TIntSet domain = new TIntHashSet();
                for (int j = 0; j < sets.length; j++) {
                    if (sets[j].getEnv().contains(i)) {
                        domain.add(j);
                    }
                }
                propagateDomain(enumDomain(domain), ints[i], intGraph);
            }
            for (IrSetExpr set : sets) {
                set.getKer().transferTo(kers);
            }
            for (int i = 0; i < sets.length; i++) {
                TIntSet env = new TIntHashSet();
                for (int j = 0; j < ints.length; j++) {
                    if (ints[j].getDomain().contains(i)) {
                        env.add(j);
                    }
                }
                env.removeAll(kers);
                sets[i].getKer().transferTo(env);
                propagateEnv(enumDomain(env), sets[i], intGraph, setGraph);
            }
            return null;
        }

        @Override
        public Boolean visit(IrSortSets ir, Pair<KeyGraph<IrIntVar>, KeyGraph<IrSetVar>> a) {
            KeyGraph<IrIntVar> intGraph = a.getFst();
            KeyGraph<IrSetVar> setGraph = a.getSnd();
            int low = 0;
            int high = 0;
            for (IrSetExpr set : ir.getSets()) {
                IrDomain card = set.getCard();
                propagateEnv(boundDomain(low, high + card.getHighBound() - 1), set, intGraph, setGraph);
                low += card.getLowBound();
                high += card.getHighBound();
            }
            return null;
        }

        @Override
        public Boolean visit(IrSelectN ir, Pair<KeyGraph<IrIntVar>, KeyGraph<IrSetVar>> a) {
            KeyGraph<IrIntVar> intGraph = a.getFst();
            IrBoolExpr[] bools = ir.getBools();
            IrIntExpr n = ir.getN();
            for (int i = 0; i < bools.length; i++) {
                if (n instanceof IrIntVar) {
                    if (IrUtil.isTrue(bools[i]) && i >= n.getDomain().getLowBound()) {
                        intGraph.addUndirectedEdge((IrIntVar) n, boundInt("selectN" + i, i + 1, bools.length));
                    } else if (IrUtil.isFalse(bools[i]) && i < n.getDomain().getHighBound()) {
                        intGraph.addUndirectedEdge((IrIntVar) n, boundInt("selectN" + i, 0, i));
                    }
                }
            }
            for (int i = 0; i < n.getDomain().getLowBound(); i++) {
                if (bools[i] instanceof IrBoolVar && !IrUtil.isConstant(bools[i])) {
                    intGraph.addUndirectedEdge((IrBoolVar) bools[i], True);
                }
            }
            for (int i = n.getDomain().getHighBound(); i < bools.length; i++) {
                if (bools[i] instanceof IrBoolVar && !IrUtil.isConstant(bools[i])) {
                    intGraph.addUndirectedEdge((IrBoolVar) bools[i], False);
                }
            }
            return null;
        }

        @Override
        public Boolean visit(IrFilterString ir, Pair<KeyGraph<IrIntVar>, KeyGraph<IrSetVar>> a) {
            KeyGraph<IrIntVar> intGraph = a.getFst();
            TIntIterator iter = ir.getSet().getEnv().iterator();
            int i = 0;
            while (iter.hasNext()) {
                int env = iter.next();
                if (!ir.getSet().getKer().contains(env)) {
                    break;
                }
                IrIntExpr string = ir.getString()[env - ir.getOffset()];
                IrIntExpr result = ir.getResult()[i];
                // TODO reuse code above in IrCompare
                if (string instanceof IrIntVar && result instanceof IrIntVar) {
                    intGraph.addUndirectedEdge((IrIntVar) string, (IrIntVar) result);
                }
                i++;
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
