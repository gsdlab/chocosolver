package org.clafer.ir;

import java.util.Arrays;

/**
 *
 * @author jimmy
 */
public interface IrDomain {

    public int size();

    public int[] getValues();

    public Integer getConstant();

    public static class IrEnumDomain implements IrDomain {

        private final int[] values;

        public IrEnumDomain(int[] values) {
            this.values = Arrays.copyOf(values, values.length);
            Arrays.sort(this.values);
        }

        @Override
        public int size() {
            return values.length;
        }

        @Override
        public int[] getValues() {
            return values;
        }

        @Override
        public Integer getConstant() {
            if (values.length == 1) {
                return values[0];
            }
            return null;
        }

        @Override
        public boolean equals(Object obj) {
            if (obj instanceof IrEnumDomain) {
                IrEnumDomain other = (IrEnumDomain) obj;
                return Arrays.equals(values, other.values);
            } else if (obj instanceof IrBoundDomain) {
                IrBoundDomain other = (IrBoundDomain) obj;
                if (size() != size()) {
                    return false;
                }
                for (int i = 0; i < values.length; i++) {
                    if (values[i] != other.low + i) {
                        return false;
                    }
                }
                return true;
            }
            return false;
        }

        @Override
        public int hashCode() {
            return Arrays.hashCode(values);
        }

        @Override
        public String toString() {
            StringBuilder result = new StringBuilder();
            result.append('{');
            for (int i = 0; i < values.length; i++) {
                if (i > 0) {
                    result.append(", ");
                }
                result.append(values[i]);
            }
            result.append('}');
            return result.toString();
        }
    }

    public static class IrBoundDomain implements IrDomain {

        private final int low;
        private final int high;

        public IrBoundDomain(int low, int high) {
            if (low > high) {
                throw new IllegalArgumentException();
            }
            this.low = low;
            this.high = high;
        }

        public int getLow() {
            return low;
        }

        public int getHigh() {
            return high;
        }

        @Override
        public int size() {
            return high + 1 - low;
        }

        @Override
        public int[] getValues() {
            int[] values = new int[size()];
            for (int i = 0; i < values.length; i++) {
                values[i] = low + i;
            }
            return values;
        }

        @Override
        public Integer getConstant() {
            if (low == high) {
                return low;
            }
            return null;
        }

        @Override
        public boolean equals(Object obj) {
            if (obj instanceof IrBoundDomain) {
                IrBoundDomain other = (IrBoundDomain) obj;
                return low == other.low && high == other.high;
            } else if (obj instanceof IrEnumDomain) {
                IrEnumDomain other = (IrEnumDomain) obj;
                if (size() != other.size()) {
                    return false;
                }
                for (int i = 0; i < other.values.length; i++) {
                    if (other.values[i] != low + i) {
                        return false;
                    }
                }
                return true;
            }
            return false;
        }

        @Override
        public int hashCode() {
            return low ^ high;
        }

        @Override
        public String toString() {
            return "{" + low + ", ..., " + high + "}";
        }
    }
}
