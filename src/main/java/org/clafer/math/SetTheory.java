package org.clafer.math;

import gnu.trove.map.TIntObjectMap;
import gnu.trove.map.hash.TIntObjectHashMap;
import java.util.Arrays;
import java.util.LinkedHashSet;
import java.util.Set;
import org.clafer.domain.Domain;

/**
 *
 * @author jimmy
 */
public class SetTheory {

    private final TIntObjectMap<Domain> envs = new TIntObjectHashMap<>();
    private final Set<Union> unions = new LinkedHashSet<>();
    private final Set<Subset> subsets = new LinkedHashSet<>();

    public Equalable union(int... operands) {
        if (operands.length == 0) {
            return x -> {
            };
        }
        if (operands.length == 1) {
            return x -> equal(x, operands[0]);
        }
        return x -> unions.add(new Union(operands, x));
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

    public void propagate() {
        boolean changed;
        do {
            changed = false;
            for (Union union : unions) {
                int output = union.output;
                int[] operands = union.operands;
                Domain unionDomain = envs.get(operands[0]);
                for (int i = 1; i < operands.length && unionDomain != null; i++) {
                    Domain next = envs.get(operands[i]);
                    unionDomain = next == null ? null : unionDomain.union(next);
                }
                if (unionDomain != null) {
                    changed |= retainEnv(output, unionDomain);
                }
                unionDomain = envs.get(output);
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
        } while (changed);
    }

    private boolean retainEnv(int var, Domain retain) {
        Domain env = envs.get(var);
        if (env == null) {
            envs.put(var, retain);
            return true;
        }
        Domain intersection = env.intersection(retain);
        if (env.equals(intersection)) {
            return false;
        }
        envs.put(var, intersection);
        return true;
    }

    public interface Equalable {

        public void equalsTo(int var);
    }

    private static class Union {

        private final int[] operands;
        private final int output;

        public Union(int[] operands, int output) {
            this.operands = operands.clone();
            Arrays.sort(this.operands);
            this.output = output;
        }

        @Override
        public boolean equals(Object obj) {
            if (obj instanceof Union) {
                Union other = (Union) obj;
                return output == other.output && Arrays.equals(operands, other.operands);
            }
            return false;
        }

        @Override
        public int hashCode() {
            return output ^ Arrays.hashCode(operands);
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
}
