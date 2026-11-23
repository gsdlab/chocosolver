package org.clafer.math;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;
import org.clafer.domain.Domain;

/**
 *
 * @author jimmy
 */
public class SetEnvironment {

    protected final Set<SubsetDomain> subsetDomains = new HashSet<>();
    protected final Set<Subset> subsets = new HashSet<>();
    protected final Set<Union> unions = new HashSet<>();

    public SetEnvironment subset(int sub, int sup) {
        if (sub != sup) {
            subsets.add(new Subset(sub, sup));
        }
        return this;
    }

    public SetEnvironment subset(int sub, Domain domain) {
        subsetDomains.add(new SubsetDomain(sub, domain));
        return this;
    }

    public SetEnvironment equal(int v1, int v2) {
        subset(v1, v2);
        subset(v2, v1);
        return this;
    }

    public SetEnvironment union(int union, int[] operands) {
        switch (operands.length) {
            case 0:
                throw new IllegalArgumentException();
            case 1:
                equal(union, operands[0]);
            default:
                unions.add(new Union(union, operands));
        }
        return this;
    }

    protected static class Union {

        protected final int union;
        protected final int[] operands;

        protected Union(int union, int[] operands) {
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

    protected static class Subset {

        protected final int sub;
        protected final int sup;

        protected Subset(int sub, int sup) {
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

    protected static class SubsetDomain {

        protected final int sub;
        protected final Domain sup;

        protected SubsetDomain(int sub, Domain sup) {
            this.sub = sub;
            this.sup = sup;
        }

        @Override
        public boolean equals(Object obj) {
            if (obj instanceof SubsetDomain) {
                SubsetDomain other = (SubsetDomain) obj;
                return sub == other.sub && sup.equals(other.sup);
            }
            return false;
        }

        @Override
        public int hashCode() {
            return sub ^ sup.hashCode();
        }
    }
}
