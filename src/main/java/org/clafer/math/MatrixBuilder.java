package org.clafer.math;

import gnu.trove.map.hash.TIntObjectHashMap;

/**
 *
 * @author jimmy
 */
public class MatrixBuilder {

    private final TIntObjectHashMap<Rational>[] rows;
    private final int numberOfColumns;

    public MatrixBuilder(int rows, int columns) {
        this.rows = (TIntObjectHashMap<Rational>[]) new TIntObjectHashMap<?>[rows];
        this.numberOfColumns = columns;
    }

    public Rational get(int row, int column) {
        if (rows[row] == null) {
            return Rational.Zero;
        }
        Rational value = rows[row].get(column);
        return value == null ? Rational.Zero : value;
    }

    public void set(int row, int column, Rational value) {
        if (rows[row] == null) {
            rows[row] = new TIntObjectHashMap<>(4);
        }
        rows[row].put(column, value);
    }

    public int numberOfRows() {
        return rows.length;
    }

    public int numberOfColumns() {
        return numberOfColumns;
    }

    public Matrix toMatrix() {
        return new Matrix(rows, numberOfColumns);
    }
}
