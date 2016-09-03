package org.clafer.math;

import gnu.trove.iterator.TIntObjectIterator;
import gnu.trove.map.TIntObjectMap;
import gnu.trove.map.hash.TIntObjectHashMap;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;
import java.util.function.IntFunction;
import org.clafer.domain.Domain;

/**
 *
 * @author jimmy
 */
public class SetTheory {

    private final TIntObjectMap<Domain> envs = new TIntObjectHashMap<>();
    private final Set<Union> unions = new HashSet<>();
    private final Set<Subset> subsets = new HashSet<>();

    public void union(int union, int[] operands) {
        switch (operands.length) {
            case 0:
                throw new IllegalArgumentException();
            case 1:
                equal(union, operands[0]);
            default:
                unions.add(new Union(union, operands));
        }
    }

    public void equal(int v1, int v2) {
        subset(v1, v2);
        subset(v2, v1);
    }

    public void subset(int sub, int sup) {
        if (sub != sup) {
            subsets.add(new Subset(sub, sup));
        }
    }

    public void subset(int sub, Domain domain) {
        Domain prev = envs.get(sub);
        envs.put(sub, prev == null ? domain : prev.intersection(domain));
    }

    public Domain getEnv(int v) {
        return envs.get(v);
    }

    public boolean propagate() {
        boolean propagated = false;
        boolean changed;
        do {
            changed = false;
            for (Union union : unions) {
                int[] operands = union.operands;
                Domain unionDomain = envs.get(operands[0]);
                for (int i = 1; i < operands.length && unionDomain != null; i++) {
                    Domain next = envs.get(operands[i]);
                    unionDomain = next == null ? null : unionDomain.union(next);
                }
                if (unionDomain != null) {
                    changed |= retainEnv(union.union, unionDomain);
                }
                unionDomain = envs.get(union.union);
                if (unionDomain != null) {
                    for (int i = 0; i < operands.length; i++) {
                        changed |= retainEnv(operands[i], unionDomain);
                    }
                }
            }
            for (Subset subset : subsets) {
                Domain supEnv = envs.get(subset.sup);
                if (supEnv != null) {
                    changed |= retainEnv(subset.sub, supEnv);
                }
            }
            propagated |= changed;
        } while (changed);
        return propagated;
    }

    private boolean retainEnv(int v, Domain retain) {
        Domain env = envs.get(v);
        if (env == null) {
            envs.put(v, retain);
            return true;
        }
        Domain intersection = env.intersection(retain);
        if (env.equals(intersection)) {
            return false;
        }
        envs.put(v, intersection);
        return true;
    }

    private static class Union {

        private final int union;
        private final int[] operands;

        public Union(int union, int[] operands) {
            this.union = union;
            this.operands = operands.clone();
            Arrays.sort(this.operands);
        }

        @Override
        public boolean equals(Object obj) {
            if (obj instanceof Union) {
                Union other = (Union) obj;
                return union == other.union && Arrays.equals(operands, other.operands);
            }
            return false;
        }

        @Override
        public int hashCode() {
            return union ^ Arrays.hashCode(operands);
        }
    }

    private static class Subset {

        private final int sub;
        private final int sup;

        public Subset(int sub, int sup) {
            this.sub = sub;
            this.sup = sup;
        }

        @Override
        public boolean equals(Object obj) {
            if (obj instanceof Subset) {
                Subset other = (Subset) obj;
                return sub == other.sub && sup == other.sup;
            }
            return false;
        }

        @Override
        public int hashCode() {
            return sub ^ sup;
        }
    }

    public String toString(IntFunction<Object> mapper) {
        StringBuilder result = new StringBuilder();
        result.append("===ENV===\n");
        for (TIntObjectIterator iter = envs.iterator(); iter.hasNext();) {
            iter.advance();
            result.append("  ").append(mapper.apply(iter.key())).append(" subset of ").append(iter.value()).append('\n');
        }
        result.append("===UNION===\n");
        for (Union union : unions) {
            result.append("  ").append(mapper.apply(union.union)).append(" = union(");
            for (int i = 0; i < union.operands.length; i++) {
                if (i > 0) {
                    result.append(", ");
                }
                result.append(mapper.apply(union.operands[i]));
            }
            result.append(")\n");
        }
        result.append("===SUBSET===\n");
        for (Subset subset : subsets) {
            result.append("  ").append(mapper.apply(subset.sub)).append(" subset of ").append(mapper.apply(subset.sup)).append('\n');
        }
        return result.toString();
    }
}
