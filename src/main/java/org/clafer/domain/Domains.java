package org.clafer.domain;

import gnu.trove.TIntCollection;
import gnu.trove.set.TIntSet;
import gnu.trove.set.hash.TIntHashSet;
import java.util.Arrays;
import java.util.Collection;

/**
 *
 * @author jimmy
 */
public class Domains {

    public static final BoolDomain TrueDomain = BoolDomain.TrueDomain;
    public static final BoolDomain FalseDomain = BoolDomain.FalseDomain;
    public static final BoolDomain TrueFalseDomain = BoolDomain.TrueFalseDomain;
    public static final Domain EmptyDomain = new EmptyDomain();
    public static final Domain ZeroDomain = FalseDomain;
    public static final Domain OneDomain = TrueDomain;
    public static final Domain ZeroOneDomain = TrueFalseDomain;
    public static final Domain Unbounded = boundDomain(Integer.MIN_VALUE, Integer.MAX_VALUE);

    public static BoolDomain domain(boolean value) {
        return value ? TrueDomain : FalseDomain;
    }

    public static Domain constantDomain(int value) {
        return boundDomain(value, value);
    }

    public static Domain fromToDomain(int from, int to) {
        if (from == to) {
            return EmptyDomain;
        }
        return boundDomain(from, to - 1);
    }

    public static Domain boundDomain(int low, int high) {
        if (low == 0 && high == 0) {
            return ZeroDomain;
        }
        if (low == 1 && high == 1) {
            return OneDomain;
        }
        if (low == 0 && high == 1) {
            return ZeroOneDomain;
        }
        return new BoundDomain(low, high);
    }

    public static Domain enumDomain(int... values) {
        return enumDomain(new TIntHashSet(values));
    }

    public static Domain enumDomain(TIntCollection values) {
        return enumDomain(new TIntHashSet(values));
    }

    public static Domain enumDomain(TIntSet values) {
        switch (values.size()) {
            case 0:
                return EmptyDomain;
            case 1:
                int value = values.iterator().next();
                return boundDomain(value, value);
            default:
                int[] array = values.toArray();
                Arrays.sort(array);
                // If the values are over a contiguous interval, then return a bound domain.
                int low = array[0];
                int high = array[array.length - 1];
                if (high - low + 1 == array.length) {
                    // A contigious interval.
                    return boundDomain(low, high);
                }
                return new EnumDomain(array);
        }
    }

    public static Domain[] enumDomains(TIntSet... values) {
        Domain[] domains = new Domain[values.length];
        for (int i = 0; i < domains.length; i++) {
            domains[i] = enumDomain(values[i]);
        }
        return domains;
    }

    public static Domain union(Collection<Domain> domains) {
        Domain union = EmptyDomain;
        for (Domain domain : domains) {
            union = union.union(domain);
        }
        return union;
    }
}
