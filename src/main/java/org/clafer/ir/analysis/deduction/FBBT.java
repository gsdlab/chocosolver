package org.clafer.ir.analysis.deduction;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import org.clafer.collection.Triple;
import org.clafer.common.UnsatisfiableException;
import org.clafer.domain.Domain;
import org.clafer.ir.IllegalIntException;
import org.clafer.ir.IllegalSetException;
import org.clafer.ir.IllegalStringException;
import org.clafer.ir.IrAdd;
import org.clafer.ir.IrAllDifferent;
import org.clafer.ir.IrArrayEquality;
import org.clafer.ir.IrArrayToSet;
import org.clafer.ir.IrBoolChannel;
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
import org.clafer.ir.IrUtil;
import org.clafer.ir.IrVar;
import org.clafer.ir.IrWithin;
import static org.clafer.ir.Irs.domainInt;
import static org.clafer.ir.Irs.set;

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
        boolDeducers.put(IrAllDifferent.class, new AllDifferentDeducer());
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
        boolDeducers.put(IrWithin.class, new WithinDeducer());
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

    public Triple<Map<IrIntVar, IrIntVar>, Map<IrSetVar, IrSetVar>, IrModule>
            propagate(IrModule module) {
        try {
            return propagateImpl(module);
        } catch (IllegalIntException | IllegalSetException | IllegalStringException e) {
            throw new UnsatisfiableException(e);
        }
    }

    private Triple<Map<IrIntVar, IrIntVar>, Map<IrSetVar, IrSetVar>, IrModule>
            propagateImpl(IrModule module) {
        Map<IrIntVar, IrIntVar> coalescedInts = new HashMap<>();
        Map<IrSetVar, IrSetVar> coalescedSets = new HashMap<>();

        Deduction deduction = new Deduction(boolDeducers, intDeducers, setDeducers);

        module.getConstraints().forEach(deduction::tautology);

        deduction.checkInvariants();

        Map<IrIntVar, Domain> intRetains = deduction.getIntRetains();
        Map<IrSetVar, Domain> setContains = deduction.getSetContains();
        Map<IrSetVar, Domain> setSubsetOf = deduction.getSetSubsetOf();

        Collection<IrSetVar> pendingSetVars = new HashSet<>();
        Collection<IrStringVar> pendingStringVars = new HashSet<>();
        for (IrVar var : module.getVariables()) {
            if (var instanceof IrSetVar) {
                pendingSetVars.add((IrSetVar) var);
            } else if (var instanceof IrStringVar) {
                pendingStringVars.add((IrStringVar) var);
            }
        }

        for (Set<IrIntVar> component : deduction.getIntEquals()) {
            if (component.size() > 1) {
                Iterator<IrIntVar> iter = component.iterator();
                IrIntVar var = iter.next();
                Domain domain = removeOrDefault(intRetains, var, var.getDomain());
                List<String> names = new ArrayList<>(component.size());
                names.add(var.getName());
                while (iter.hasNext()) {
                    var = iter.next();
                    domain = domain.intersection(removeOrDefault(intRetains, var, var.getDomain()));
                    names.add(var.getName());
                }
                IrIntVar coalesced = domainInt(joinNames(names), domain);
                for (IrIntVar coalesce : component) {
                    coalescedInts.put(coalesce, coalesced);
                }
            }
        }
        intRetains.forEach(
                (var, domain) -> coalescedInts.put(var, domainInt(var.getName(), domain))
        );

        for (Set<IrSetVar> component : deduction.getSetEquals()) {
            if (component.size() > 1) {
                Iterator<IrSetVar> iter = component.iterator();
                IrSetVar var = iter.next();
                Domain ker = setContains.getOrDefault(var, var.getKer());
                Domain env = setSubsetOf.getOrDefault(var, var.getEnv());
                IrIntVar card = coalescedInts.getOrDefault(var.getCardVar(), var.getCardVar());
                List<String> names = new ArrayList<>(component.size());
                names.add(var.getName());
                while (iter.hasNext()) {
                    var = iter.next();
                    ker = ker.union(setContains.getOrDefault(var, var.getKer()));
                    env = env.intersection(setSubsetOf.getOrDefault(var, var.getEnv()));
                    assert card.equals(coalescedInts.getOrDefault(var.getCardVar(), var.getCardVar()));
                    names.add(var.getName());
                }
                IrSetVar coalesced = set(joinNames(names), env, ker, card);
                for (IrSetVar coalesce : component) {
                    coalescedSets.put(coalesce, coalesced);
                }
                pendingSetVars.removeAll(component);
            }
        }
        for (IrSetVar var : pendingSetVars) {
            Domain ker = setContains.get(var);
            Domain env = setSubsetOf.get(var);
            IrIntVar card = coalescedInts.get(var.getCardVar());
            if (ker != null || env != null || card != null) {
                ker = ker == null ? var.getKer() : ker;
                env = env == null ? var.getEnv() : env;
                card = card == null ? var.getCardVar() : card;
                coalescedSets.put(var, set(var.getName(), env, ker, card));
            }
        }

        Map<IrStringVar, IrStringVar> coalescedStrings = IrUtil.stringRenamer(
                pendingStringVars, coalescedInts);

        return new Triple<>(
                coalescedInts,
                coalescedSets,
                IrUtil.renameVariables(module, coalescedInts, coalescedSets, coalescedStrings));
    }

    private static <K, V> V removeOrDefault(Map<K, V> map, K key, V defaultValue) {
        V value = map.remove(key);
        return value == null ? defaultValue : value;
    }

    private static String stripParens(String name) {
        if (name.startsWith("(") && name.endsWith(")")) {
            return name.substring(1, name.length() - 1);
        }
        return name;
    }

    private static String joinNames(List<String> names) {
        if (names.size() == 1) {
            return names.get(0);
        }
        return names.stream().map(FBBT::stripParens).collect(Collectors.joining(";", "(", ")"));
    }
}
