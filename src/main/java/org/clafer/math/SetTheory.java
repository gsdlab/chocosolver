package org.clafer.math;

import gnu.trove.iterator.TIntObjectIterator;
import gnu.trove.map.TIntObjectMap;
import gnu.trove.map.hash.TIntObjectHashMap;
import java.util.ArrayList;
import java.util.List;
import java.util.function.IntFunction;
import org.clafer.domain.Domain;

/**
 *
 * @author jimmy
 */
public class SetTheory extends SetEnvironment {

    private final BaseDomainMap envs = new BaseDomainMap();
    private final List<SetEnvironment[]> constructiveDisjunctions = new ArrayList<>();

    public Domain getEnv(int v) {
        return envs.get(v);
    }

    public void constructiveDisjunction(SetEnvironment... disjunction) {
        if (disjunction.length <= 1) {
            throw new IllegalArgumentException();
        }
        constructiveDisjunctions.add(disjunction);
    }

    public boolean propagate() {
        boolean propagated = false;
        boolean changed;
        do {
            changed = propagate(envs, this);
            // Handle the constructive conjunctions.
            for (SetEnvironment[] constructiveDisjunction : constructiveDisjunctions) {
                assert constructiveDisjunction.length > 0;
                // The alternate disjunctions.
                SubDomainMap[] disjunction = new SubDomainMap[constructiveDisjunction.length];
                int i = 0;
                for (SetEnvironment conjunction : constructiveDisjunction) {
                    SubDomainMap conjunctionEnv = new SubDomainMap(envs);
                    propagate(conjunctionEnv, conjunction, this);
                    disjunction[i++] = conjunctionEnv;
                }
                assert i == disjunction.length;

                // Merge the alternate disjunctions by union.
                TIntObjectIterator<Domain> iterator = disjunction[0].map.iterator();
                while (iterator.hasNext()) {
                    iterator.advance();

                    int key = iterator.key();
                    Domain union = iterator.value();
                    for (int j = 1; j < disjunction.length && union != null; j++) {
                        Domain domain = disjunction[j].get(key);
                        union = domain == null ? null : union.union(domain);
                    }
                    // Merge back into the main environment by intersection.
                    if (union != null) {
                        changed |= envs.put(key, union);
                    }
                }
            }
            propagated |= changed;
        } while (changed);
        return propagated;
    }

    private static boolean propagate(DomainMap map, SetEnvironment... environments) {
        boolean propagated = false;
        for (SetEnvironment environment : environments) {
            for (SubsetDomain subset : environment.subsetDomains) {
                propagated |= map.put(subset.sub, subset.sup);
            }
        }
        boolean changed;
        do {
            changed = false;
            for (SetEnvironment environment : environments) {
                for (Subset subset : environment.subsets) {
                    Domain supEnv = map.get(subset.sup);
                    if (supEnv != null) {
                        changed |= map.put(subset.sub, supEnv);
                    }
                }
                for (Union union : environment.unions) {
                    int[] operands = union.operands;
                    Domain unionDomain = map.get(operands[0]);
                    for (int i = 1; i < operands.length && unionDomain != null; i++) {
                        Domain next = map.get(operands[i]);
                        unionDomain = next == null ? null : unionDomain.union(next);
                    }
                    if (unionDomain != null) {
                        changed |= map.put(union.union, unionDomain);
                    }
                    unionDomain = map.get(union.union);
                    if (unionDomain != null) {
                        for (int i = 0; i < operands.length; i++) {
                            changed |= map.put(operands[i], unionDomain);
                        }
                    }
                }
            }
            propagated |= changed;
        } while (changed);
        return propagated;
    }

    public String toString(IntFunction<Object> mapper) {
        StringBuilder result = new StringBuilder();
        result.append("===ENV===\n");
        for (TIntObjectIterator iter = envs.map.iterator(); iter.hasNext();) {
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

    public static SetEnvironment or() {
        return new SetEnvironment();
    }

    private static interface DomainMap {

        Domain get(int v);

        boolean put(int v, Domain domain);
    }

    private static class BaseDomainMap implements DomainMap {

        final TIntObjectMap<Domain> map = new TIntObjectHashMap<>();

        @Override
        public Domain get(int v) {
            return map.get(v);
        }

        @Override
        public boolean put(int v, Domain domain) {
            Domain prev = get(v);
            if (prev == null) {
                map.put(v, domain);
                return true;
            }
            Domain intersection = prev.intersection(domain);
            if (prev.equals(intersection)) {
                return false;
            }
            map.put(v, intersection);
            return true;
        }
    }

    private static class SubDomainMap implements DomainMap {

        final BaseDomainMap baseMap;
        final TIntObjectMap<Domain> map = new TIntObjectHashMap<>();

        SubDomainMap(BaseDomainMap baseMap) {
            this.baseMap = baseMap;
        }

        @Override
        public Domain get(int v) {
            Domain domain = map.get(v);
            return domain == null ? baseMap.get(v) : domain;
        }

        @Override
        public boolean put(int v, Domain domain) {
            Domain prev = get(v);
            if (prev == null) {
                map.put(v, domain);
                return true;
            }
            Domain intersection = prev.intersection(domain);
            if (prev.equals(intersection)) {
                return false;
            }
            map.put(v, intersection);
            return true;
        }
    }
}
