package org.clafer.ir.analysis.deduction;

import java.util.HashMap;
import java.util.Map;
import org.clafer.collection.Pair;
import org.clafer.common.UnsatisfiableException;
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
import org.clafer.ir.IrBoolVar;
import org.clafer.ir.IrCard;
import org.clafer.ir.IrCompare;
import org.clafer.ir.IrCount;
import org.clafer.ir.IrElement;
import org.clafer.ir.IrIfOnlyIf;
import org.clafer.ir.IrIntChannel;
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
import org.clafer.ir.IrSingleton;
import org.clafer.ir.IrSortSets;
import org.clafer.ir.IrSortStrings;
import org.clafer.ir.IrSortStringsChannel;
import org.clafer.ir.IrStringCompare;
import org.clafer.ir.IrSubsetEq;
import org.clafer.ir.IrTernary;
import org.clafer.ir.IrWithin;

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

    public Pair<Coalesce, IrModule> propagate(IrModule module) {
        try {
            Pair<Coalesce, IrModule> coalescePair = propagateImpl(module);
            Coalesce coalesce = coalescePair.getFst();
            module = coalescePair.getSnd();
            while (!coalescePair.getFst().isEmpty()) {
                coalescePair = propagateImpl(module);
                coalesce = coalesce.compose(coalescePair.getFst());
                module = coalescePair.getSnd();
            }
            return new Pair<>(coalesce, module);
        } catch (IllegalIntException | IllegalSetException | IllegalStringException e) {
            throw new UnsatisfiableException(e);
        }
    }

    private Pair<Coalesce, IrModule> propagateImpl(IrModule module) {
        Deduction deduction = new Deduction(boolDeducers, intDeducers, setDeducers);

        module.getConstraints().forEach(deduction::tautology);

        deduction.checkInvariants();

        Coalesce coalesce = deduction.apply(module);

        return new Pair<>(coalesce, coalesce.rewrite(module, null));
    }
}
