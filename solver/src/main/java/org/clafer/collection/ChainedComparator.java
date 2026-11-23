package org.clafer.collection;

import java.util.Comparator;
import org.clafer.common.Check;

/**
 *
 * @param <T> the type of objects that may be compared by this comparator
 * @author jimmy
 */
public class ChainedComparator<T> implements Comparator<T> {

    private final Comparator<T>[] comparators;

    @SafeVarargs
    public ChainedComparator(Comparator<T>... comparators) {
        this.comparators = Check.noNulls(comparators);
    }

    @Override
    public int compare(T o1, T o2) {
        for (Comparator<T> comparator : comparators) {
            int compare = comparator.compare(o1, o2);
            if (compare != 0) {
                return compare;
            }
        }
        return 0;
    }
}
