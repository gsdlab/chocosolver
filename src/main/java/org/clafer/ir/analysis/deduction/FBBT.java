package org.clafer.ir.analysis.deduction;

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import org.clafer.collection.Pair;
import org.clafer.common.UnsatisfiableException;
import org.clafer.domain.Domain;
import org.clafer.ir.IllegalIntException;
import org.clafer.ir.IllegalSetException;
import org.clafer.ir.IllegalStringException;
import org.clafer.ir.IrAcyclic;
import org.clafer.ir.IrAdd;
import org.clafer.ir.IrAllDifferent;
import org.clafer.ir.IrAnd;
import org.clafer.ir.IrArrayEquality;
import org.clafer.ir.IrArrayToSet;
import org.clafer.ir.IrBoolChannel;
import org.clafer.ir.IrBoolExpr;
import org.clafer.ir.IrBoolVar;
import org.clafer.ir.IrCard;
import org.clafer.ir.IrCompare;
import org.clafer.ir.IrCount;
import org.clafer.ir.IrElement;
import org.clafer.ir.IrIfOnlyIf;
import org.clafer.ir.IrIntChannel;
import org.clafer.ir.IrIntVar;
import org.clafer.ir.IrJoinFunction;
import org.clafer.ir.IrJoinRelation;
import org.clafer.ir.IrMember;
import org.clafer.ir.IrMinus;
import org.clafer.ir.IrModule;
import org.clafer.ir.IrMul;
import org.clafer.ir.IrNot;
import org.clafer.ir.IrNotMember;
import org.clafer.ir.IrOffset;
import org.clafer.ir.IrSelectN;
import org.clafer.ir.IrSetDifference;
import org.clafer.ir.IrSetElement;
import org.clafer.ir.IrSetEquality;
import org.clafer.ir.IrSetMin;
import org.clafer.ir.IrSetTernary;
import org.clafer.ir.IrSetUnion;
import org.clafer.ir.IrSetVar;
import org.clafer.ir.IrSingleton;
import org.clafer.ir.IrSortSets;
import org.clafer.ir.IrSortStrings;
import org.clafer.ir.IrSortStringsChannel;
import org.clafer.ir.IrStringCompare;
import org.clafer.ir.IrStringVar;
import org.clafer.ir.IrSubsetEq;
import org.clafer.ir.IrTernary;
import org.clafer.ir.IrVar;

/**
 * Feasibility-based bounds tightening.
 *
 * In addition, removes some non-bounds as well and coalesces variables when
 * possible.
 *
 * @author jimmy
 */
public class FBBT {

    private final Map<Class<?>, BoolDeducer<?>> boolDeducers;
    private final Map<Class<?>, IntDeducer<?>> intDeducers;
    private final Map<Class<?>, SetDeducer<?>> setDeducers;

    public FBBT() {
        boolDeducers = new HashMap<>();
        boolDeducers.put(IrAcyclic.class, new AcyclicDeducer());
        boolDeducers.put(IrAllDifferent.class, new AllDifferentDeducer());
        boolDeducers.put(IrAnd.class, new AndDeducer());
        boolDeducers.put(IrArrayEquality.class, new ArrayEqualityDeducer());
        boolDeducers.put(IrBoolChannel.class, new BoolChannelDeducer());
        boolDeducers.put(IrBoolVar.class, new BoolVarDeducer());
        boolDeducers.put(IrCompare.class, new CompareDeducer());
        boolDeducers.put(IrIfOnlyIf.class, new IfOnlyIfDeducer());
        boolDeducers.put(IrIntChannel.class, new IntChannelDeducer());
        boolDeducers.put(IrMember.class, new MemberDeducer());
        boolDeducers.put(IrNot.class, new NotDeducer());
        boolDeducers.put(IrNotMember.class, new NotMemberDeducer());
        boolDeducers.put(IrSelectN.class, new SelectNDeducer());
        boolDeducers.put(IrSetEquality.class, new SetEqualityDeducer());
        boolDeducers.put(IrStringCompare.class, new StringCompareDeducer());
        boolDeducers.put(IrSortSets.class, new SortSetsDeducer());
        boolDeducers.put(IrSortStrings.class, new SortStringsDeducer());
        boolDeducers.put(IrSortStringsChannel.class, new SortStringsChannelDeducer());
        boolDeducers.put(IrSubsetEq.class, new SubsetEqDeducer());
        intDeducers = new HashMap<>();
        intDeducers.put(IrAdd.class, new AddDeducer());
        intDeducers.put(IrCard.class, new CardDeducer());
        intDeducers.put(IrCount.class, new CountDeducer());
        intDeducers.put(IrElement.class, new ElementDeducer());
        intDeducers.put(IrMinus.class, new MinusDeducer());
        intDeducers.put(IrMul.class, new MulDeducer());
        intDeducers.put(IrSetMin.class, new SetMinDeducer());
        intDeducers.put(IrTernary.class, new TernaryDeducer());
        setDeducers = new HashMap<>();
        setDeducers.put(IrArrayToSet.class, new ArrayToSetDeducer());
        setDeducers.put(IrJoinFunction.class, new JoinFunctionDeducer());
        setDeducers.put(IrJoinRelation.class, new JoinRelationDeducer());
        setDeducers.put(IrOffset.class, new OffsetDeducer());
        setDeducers.put(IrSetDifference.class, new SetDifferenceDeducer());
        setDeducers.put(IrSetElement.class, new SetElementDeducer());
        setDeducers.put(IrSetTernary.class, new SetTernaryDeducer());
        setDeducers.put(IrSetUnion.class, new SetUnionDeducer());
        setDeducers.put(IrSingleton.class, new SingletonDeducer());
    }

    public Pair<Coalesce, IrModule> propagate(IrModule module) {
        try {
            Set<IrBoolExpr> changed = new HashSet<>();
            changed.addAll(module.getConstraints());
            State state = new State(module);

            Coalesce coalesce = propagateImpl(state, changed);
            Coalesce cur = coalesce;
            while (!cur.isEmpty()) {
                cur = propagateImpl(state, changed);
                coalesce = coalesce.compose(cur);
            }
            return new Pair<>(coalesce, state.toModule());
        } catch (IllegalIntException | IllegalSetException | IllegalStringException e) {
            throw new UnsatisfiableException(e);
        }
    }

    private Coalesce propagate(Deduction deduction, State state) {
        try {
            Coalesce coalesce = deduction.apply(state.setVars, state.stringVars);

            if (coalesce.isEmpty()) {
                return coalesce;
            }

            Set<IrBoolExpr> changed = new HashSet<>();
            state.apply(coalesce, changed);

            Coalesce cur = coalesce;
            while (!cur.isEmpty()) {
                cur = propagateImpl(state, changed);
                coalesce = coalesce.compose(cur);
            }
            return coalesce;
        } catch (IllegalIntException | IllegalSetException | IllegalStringException e) {
            throw new UnsatisfiableException(e);
        }
    }

    private Coalesce propagateImpl(State state, Set<IrBoolExpr> changed) {
        Deduction deduction = new Deduction(boolDeducers, intDeducers, setDeducers);

        changed.forEach(deduction::tautology);

        assert deduction.checkInvariants();

        Coalesce coalesce = deduction.apply(state.setVars, state.stringVars);

        if (coalesce.isEmpty()) {
            return coalesce;
        }

        changed.clear();
        state.apply(coalesce, changed);

        return coalesce;
    }

    public Pair<Coalesce, IrModule> constructiveDisjunction(IrBoolExpr case1, IrBoolExpr case2, IrModule module) {
        Deduction deduction = new Deduction(boolDeducers, intDeducers, setDeducers);

        module.getConstraints().forEach(deduction::tautology);

        assert deduction.checkInvariants();

        try {
            Deduction case1Deduction = new Deduction(deduction);
            case1Deduction.tautology(case1);
            Coalesce coalesce1 = propagate(case1Deduction, new State(module, case1));
            try {
                Deduction case2Deduction = new Deduction(deduction);
                case2Deduction.tautology(case2);
                Coalesce coalesce2 = propagate(case2Deduction, new State(module, case2));

                coalesce1.forEachIntVar((key, value1) -> {
                    IrIntVar value2 = coalesce2.get(key);
                    if (value2 != key) {
                        Domain combine = value1.getDomain().union(value2.getDomain());
                        deduction.within(key, combine);
                    }
                });

                coalesce1.forEachSetVar((key, value1) -> {
                    IrSetVar value2 = coalesce2.get(key);
                    if (value2 != key) {
                        Domain combineKer = value1.getKer().intersection(value2.getKer());
                        Domain combineEnv = value1.getEnv().union(value2.getEnv());
                        Domain combineCard = value1.getCard().union(value2.getCard());
                        deduction.kerContains(key, combineKer);
                        deduction.envSubsetOf(key, combineEnv);
                        deduction.cardWithin(key, combineCard);
                    }
                });
            } catch (UnsatisfiableException e) {
                deduction.contradiction(case2);
            }
        } catch (UnsatisfiableException e) {
            deduction.contradiction(case1);
        }
        State state = new State(module);
        Coalesce coalesce = propagate(deduction, state);
        return new Pair<>(coalesce, state.toModule());
    }

    private static class State {

        final IrBoolExpr[] constraints;
        int size;
        Set<IrSetVar> setVars = new HashSet<>();
        Set<IrStringVar> stringVars = new HashSet<>();

        State(IrModule module, IrBoolExpr tautology) {
            Collection<IrBoolExpr> c = module.getConstraints();
            if (tautology == null) {
                this.constraints = c.toArray(new IrBoolExpr[c.size()]);
            } else {
                this.constraints = c.toArray(new IrBoolExpr[c.size() + 1]);
                this.constraints[c.size()] = tautology;
            }
            this.size = constraints.length;
            for (IrVar var : module.getVariables()) {
                if (!var.isConstant()) {
                    if (var instanceof IrSetVar) {
                        setVars.add((IrSetVar) var);
                    } else if (var instanceof IrStringVar) {
                        stringVars.add((IrStringVar) var);
                    }
                }
            }
        }

        State(IrModule module) {
            this(module, null);
        }

        void apply(Coalesce coalesce, Set<IrBoolExpr> changed) {
            for (int i = 0; i < size;) {
                IrBoolExpr newConstraint = coalesce.rewrite(constraints[i], null);
                if (newConstraint.getDomain().isFalse()) {
                    throw new UnsatisfiableException();
                } else if (newConstraint.getDomain().isTrue()) {
                    size--;
                    constraints[i] = constraints[size];
                    constraints[size] = null;
                } else {
                    if (constraints[i] != newConstraint) {
                        changed.add(newConstraint);
                        constraints[i] = newConstraint;
                    }
                    i++;
                }
            }

            setVars = setVars.stream().map(coalesce::get).collect(Collectors.toSet());
            stringVars = stringVars.stream().map(coalesce::get).collect(Collectors.toSet());
        }

        IrModule toModule() {
            IrModule module = new IrModule();
            for (int i = 0; i < size; i++) {
                module.addConstraints(constraints[i]);
            }
            return module;
        }
    }
}
