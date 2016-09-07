package org.clafer.ir.analysis.deduction;

import gnu.trove.list.TIntList;
import gnu.trove.list.array.TIntArrayList;
import gnu.trove.set.hash.TIntHashSet;
import java.util.Collection;
import java.util.Set;
import org.clafer.collection.DisjointSets;
import org.clafer.ir.IrIntExpr;
import org.clafer.ir.IrSortStringsChannel;
import org.clafer.ir.IrUtil;

/**
 *
 * @author jimmy
 */
class SortStringsChannelDeducer implements BoolDeducer<IrSortStringsChannel> {

    @Override
    public void deduce(IrSortStringsChannel ir, Deduction deduction) {
        IrIntExpr[][] strings = ir.getStrings();
        IrIntExpr[] ints = ir.getInts();
        DisjointSets<Integer> equivalenceClasses = new DisjointSets<>();
        TIntList[] largerThans = new TIntList[strings.length];
        for (int i = 0; i < strings.length; i++) {
            largerThans[i] = new TIntArrayList(4);
            equivalenceClasses.union(i, i);
        }
        for (int i = 0; i < strings.length; i++) {
            for (int j = i + 1; j < strings.length; j++) {
                switch (IrUtil.compareString(strings[i], strings[j])) {
                    case EQ:
                        deduction.equal(ints[i], ints[j]);
                        equivalenceClasses.union(i, j);
                        break;
                    case LT:
                        deduction.lessThan(ints[i], ints[j]);
                        largerThans[j].add(i);
                        break;
                    case LE:
                        deduction.lessThanEqual(ints[i], ints[j]);
                        largerThans[j].add(i);
                        break;
                    case GT:
                        deduction.greaterThan(ints[i], ints[j]);
                        largerThans[i].add(j);
                        break;
                    case GE:
                        deduction.greaterThanEqual(ints[i], ints[j]);
                        largerThans[i].add(j);
                        break;
                    case UNKNOWN:
                        largerThans[i].add(j);
                        largerThans[j].add(i);
                        break;
                }
            }
        }
        Collection<Set<Integer>> equivalenceComponents = equivalenceClasses.connectedComponents();
        for (Set<Integer> equivalenceComponent : equivalenceComponents) {
            int representative = equivalenceClasses.representative(equivalenceComponent.iterator().next());
            TIntHashSet largerThanClasses = new TIntHashSet(largerThans[representative].size());
            largerThans[representative].forEach(x -> largerThanClasses.add(equivalenceClasses.representative(x)) || true);
            for (int i : equivalenceComponent) {
                deduction.lessThanEqual(ints[i], largerThanClasses.size());
            }
        }
        for (int i = 0; i < ints.length; i++) {
            for (int j = i + 1; j < ints.length; j++) {
                switch (IrUtil.compare(ints[i], ints[j])) {
                    case EQ:
                        deduction.lexEqual(strings[i], strings[j]);
                        break;
                    case LT:
                        deduction.lexThan(strings[i], strings[j]);
                        break;
                    case LE:
                        deduction.lexThanEqual(strings[i], strings[j]);
                        break;
                    case GT:
                        deduction.lexThan(strings[j], strings[i]);
                        break;
                    case GE:
                        deduction.lexThanEqual(strings[j], strings[i]);
                        break;
                }
            }
        }
    }
}
