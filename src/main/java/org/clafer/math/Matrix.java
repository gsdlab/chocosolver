package org.clafer.math;

import gnu.trove.TCollections;
import gnu.trove.function.TObjectFunction;
import gnu.trove.iterator.TIntObjectIterator;
import gnu.trove.map.TIntObjectMap;
import gnu.trove.map.hash.TIntObjectHashMap;
import java.util.Arrays;

/**
 * A spare matrix of rational numbers.
 *
 * @author jimmy
 */
public class Matrix {

    private final TIntObjectHashMap<Rational>[] rows;
    private final int numberOfColumns;

    public Matrix(long... column) {
        this(convert(column));
    }

    public Matrix(long[][] rows) {
        this(convert(rows));
    }

    public Matrix(Rational[] column) {
        this(convert(column));
    }

    public Matrix(Rational[][] rows) {
        this.numberOfColumns = rows[0].length;

        this.rows = rows(rows.length, this.numberOfColumns);
        for (int i = 0; i < rows.length; i++) {
            for (int j = 0; j < rows[i].length; j++) {
                Rational r = rows[i][j];
                if (!r.isZero()) {
                    this.rows[i].put(j, r);
                }
            }
        }
    }

    private Matrix(TIntObjectHashMap<Rational>[] data, int columns) {
        for (TIntObjectHashMap<Rational> d : data) {
            for (Rational o : d.valueCollection()) {
                if (o.isZero()) {
                    throw new IllegalArgumentException(Arrays.toString(data));
                }
            }
        }
        this.rows = data;
        this.numberOfColumns = columns;
    }

    private static TIntObjectHashMap<Rational>[] rows(int rows, int columns) {
        @SuppressWarnings("unchecked")
        TIntObjectHashMap<Rational>[] data
                = (TIntObjectHashMap<Rational>[]) new TIntObjectHashMap<?>[rows];
        for (int i = 0; i < data.length; i++) {
            data[i] = new TIntObjectHashMap<>(Math.min(columns, 8));
        }
        return data;
    }

    private static Rational[][] convert(long[] column) {
        Rational[][] convert = new Rational[column.length][1];
        for (int i = 0; i < convert.length; i++) {
            convert[i][0] = new Rational(column[i]);
        }
        return convert;
    }

    private static Rational[][] convert(long[][] rows) {
        Rational[][] convert = new Rational[rows.length][rows[0].length];
        for (int i = 0; i < convert.length; i++) {
            for (int j = 0; j < convert[i].length; j++) {
                convert[i][j] = new Rational(rows[i][j]);
            }
        }
        return convert;
    }

    private static Rational[][] convert(Rational[] column) {
        Rational[][] convert = new Rational[column.length][1];
        for (int i = 0; i < convert.length; i++) {
            convert[i][0] = column[i];
        }
        return convert;
    }

    public static Matrix identity(int size) {
        TIntObjectHashMap<Rational>[] matrix = rows(size, 1);
        for (int i = 0; i < size; i++) {
            matrix[i].put(i, Rational.One);
        }
        return new Matrix(matrix, size);
    }

    public int numberOfRows() {
        return rows.length;
    }

    public int numberOfColumns() {
        return numberOfColumns;
    }

    public boolean isSquare() {
        return numberOfRows() == numberOfColumns();
    }

    public TIntObjectMap<Rational>[] getRows() {
        @SuppressWarnings("unchecked")
        TIntObjectMap<Rational>[] copy
                = (TIntObjectMap< Rational>[]) new TIntObjectMap<?>[rows.length];
        for (int i = 0; i < copy.length; i++) {
            copy[i] = TCollections.unmodifiableMap(rows[i]);
        }
        return copy;
    }

    public Matrix transpose() {
        TIntObjectHashMap<Rational>[] matrix = rows(numberOfColumns(), numberOfRows());
        for (int i = 0; i < rows.length; i++) {
            TIntObjectIterator<Rational> iter = rows[i].iterator();
            for (int j = rows[i].size(); j-- > 0;) {
                iter.advance();
                matrix[iter.key()].put(i, iter.value());
            }
        }
        return new Matrix(matrix, numberOfRows());
    }

    public Matrix addColumns(Matrix right) {
        if (numberOfRows() != right.numberOfRows()) {
            throw new IllegalArgumentException();
        }
        TIntObjectHashMap<Rational>[] matrix = rows(numberOfRows(), numberOfColumns() + right.numberOfColumns());

        for (int i = 0; i < rows.length; i++) {
            TIntObjectHashMap<Rational> row1 = rows[i];
            int size1 = row1 == null ? 0 : row1.size();
            TIntObjectHashMap<Rational> row2 = right.rows[i];
            int size2 = row2 == null ? 0 : row2.size();

            int size = size1 + size2;
            if (size > 0) {
                if (row1 != null) {
                    matrix[i].putAll(row1);
                }
                if (row2 != null) {
                    TIntObjectIterator<Rational> iter = row2.iterator();
                    for (int j = row2.size(); j-- > 0;) {
                        iter.advance();
                        matrix[i].put(iter.key() + numberOfColumns(), iter.value());
                    }
                }
            }
        }
        return new Matrix(matrix, numberOfColumns() + right.numberOfColumns());
    }

    public Matrix subColumns(int start) {
        return subColumns(start, numberOfColumns());
    }

    public Matrix subColumns(int start, int end) {
        return subMatrix(0, numberOfRows(), start, end);
    }

    public Matrix subMatrix(int rowStart, int rowEnd, int columnStart, int columnEnd) {
        if (rowStart < 0 || rowStart > numberOfRows()) {
            throw new IllegalArgumentException();
        }
        if (rowEnd < 0 || rowEnd > numberOfRows()) {
            throw new IllegalArgumentException();
        }
        if (rowStart > rowEnd) {
            throw new IllegalArgumentException();
        }
        if (columnStart < 0 || columnStart > numberOfColumns()) {
            throw new IllegalArgumentException();
        }
        if (columnEnd < 0 || columnEnd > numberOfColumns()) {
            throw new IllegalArgumentException();
        }
        if (columnStart > columnEnd) {
            throw new IllegalArgumentException();
        }
        TIntObjectHashMap<Rational>[] matrix = rows(rowEnd - rowStart, columnEnd - columnStart);
        for (int i = rowStart; i < rowEnd; i++) {
            TIntObjectHashMap<Rational> row = rows[i];
            if (row != null) {
                TIntObjectIterator<Rational> iter = row.iterator();
                for (int j = row.size(); j-- > 0;) {
                    iter.advance();
                    int column = iter.key();
                    if (column >= columnStart && column < columnEnd) {
                        matrix[i].put(column - columnStart, iter.value());
                    }
                }
            }
        }
        return new Matrix(matrix, columnEnd - columnStart);
    }

    public Matrix multiply(Matrix multiplier) {
        if (numberOfColumns() != multiplier.numberOfRows()) {
            throw new IllegalArgumentException("Cannot multiply a "
                    + numberOfRows() + "x" + numberOfColumns() + " matrix with a "
                    + multiplier.numberOfRows() + "x" + multiplier.numberOfColumns() + " matrix."
            );
        }

        Matrix multiplierT = multiplier.transpose();
        TIntObjectHashMap<Rational>[] matrix = rows(numberOfRows(), multiplier.numberOfColumns());
        for (int i = 0; i < rows.length; i++) {
            for (int j = 0; j < multiplierT.rows.length; j++) {
                TIntObjectIterator<Rational> iter = rows[i].iterator();
                Rational sum = Rational.Zero;
                for (int k = rows[i].size(); k-- > 0;) {
                    iter.advance();
                    Rational r2 = multiplierT.rows[j].get(iter.key());
                    if (r2 != null) {
                        Rational r1 = iter.value();
                        sum = sum.add(r1.mul(r2));
                    }
                }
                if (!sum.isZero()) {
                    matrix[i].put(j, sum);
                }
            }
        }
        return new Matrix(matrix, multiplier.numberOfColumns());
    }

    public Matrix gaussJordanElimination() {
        int lead = 0;
        int rowCount = numberOfRows();
        int columnCount = numberOfColumns();

        @SuppressWarnings("unchecked")
        TIntObjectHashMap<Rational>[] matrix
                = (TIntObjectHashMap<Rational>[]) new TIntObjectHashMap<?>[rows.length];
        for (int i = 0; i < matrix.length; i++) {
            matrix[i] = new TIntObjectHashMap<>(rows[i]);
        }

        for (int r = 0; r < rowCount; r++) {
            if (columnCount <= lead) {
                return new Matrix(matrix, numberOfColumns());
            }
            int i = r;
            while (!matrix[i].containsKey(lead)) {
                i++;
                if (i == rowCount) {
                    i = r;
                    lead++;
                    if (lead == columnCount) {
                        return new Matrix(matrix, numberOfColumns());
                    }
                }
            }
            if (i != r) {
                // Swap rows i and r
                TIntObjectHashMap< Rational> rowI = matrix[i];
                matrix[i] = matrix[r];
                matrix[r] = rowI;
            }
            if (matrix[r].containsKey(lead)) {
                // Divide row r by matrix[r][lead]
                TIntObjectHashMap<Rational> rowR = matrix[r];
                final Rational divisor = rowR.get(lead);
                assert divisor != null;
                rowR.transformValues(new TObjectFunction<Rational, Rational>() {

                    @Override
                    public Rational execute(Rational value) {
                        return value.div(divisor);
                    }
                });
            }
            for (i = 0; i < rowCount; i++) {
                if (i != r) {
                    // Subtract M[i, lead] multiplied by row r from row I
                    TIntObjectHashMap<Rational> rowI = matrix[i];
                    TIntObjectHashMap<Rational> rowR = matrix[r];
                    Rational multiplier = rowI.get(lead);
                    if (multiplier != null) {
                        TIntObjectIterator<Rational> iter = rowR.iterator();
                        for (int j = rowR.size(); j-- > 0;) {
                            iter.advance();
                            Rational sub = iter.value().mul(multiplier);
                            assert !sub.isZero();
                            Rational value = rowI.putIfAbsent(iter.key(), sub.minus());
                            if (value != null) {
                                if (value.equals(sub)) {
                                    rowI.remove(iter.key());
                                } else {
                                    rowI.put(iter.key(), value.sub(sub));
                                }
                            }
                        }
                    }
                }
            }
            lead++;
        }
        return new Matrix(matrix, numberOfColumns());
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof Matrix) {
            Matrix other = (Matrix) obj;
            return Arrays.equals(rows, other.rows) && numberOfColumns() == other.numberOfColumns();
        }
        return false;
    }

    @Override
    public int hashCode() {
        return Arrays.deepHashCode(rows);
    }

    @Override
    public String toString() {
        String[][] strings = new String[numberOfRows()][numberOfColumns()];
        int[] maxLength = new int[numberOfColumns()];
        for (int i = 0; i < strings.length; i++) {
            for (int j = 0; j < strings[i].length; j++) {
                Rational r = rows[i].get(j);
                strings[i][j] = r == null ? "0" : r.toString();
                maxLength[j] = Math.max(maxLength[j], strings[i][j].length());
            }
        }
        StringBuilder result = new StringBuilder();
        for (String[] string : strings) {
            result.append('[');
            for (int j = 0; j < string.length; j++) {
                if (j > 0) {
                    result.append(',');
                }
                for (int k = string[j].length(); k < maxLength[j] + 1; k++) {
                    result.append(' ');
                }
                result.append(string[j]);
            }
            result.append("]\n");
        }
        return result.toString();
    }
}
