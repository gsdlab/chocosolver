package org.clafer.math;

import static org.clafer.math.Matrix.identity;
import static org.junit.Assert.assertEquals;
import static org.junit.Assume.assumeTrue;
import org.junit.experimental.theories.DataPoints;
import org.junit.experimental.theories.Theories;
import org.junit.experimental.theories.Theory;
import org.junit.runner.RunWith;

/**
 *
 * @author jimmy
 */
@RunWith(Theories.class)
public class MatrixTheory {

    @DataPoints
    public static final Matrix[] matrices = new Matrix[]{
        new Matrix(new long[][]{
            {1, 2, 3},
            {3, 4, 5},
            {6, 7, 9}
        })
    };

    @Theory
    public void transposeInverseTranspose(Matrix matrix) {
        assertEquals(matrix, matrix.transpose().transpose());
    }

    @Theory
    public void addColumnInverseSubColumn(Matrix matrix, Matrix columns) {
        assertEquals(matrix, matrix.addColumns(columns).subColumns(matrix.numberOfColumns()));
    }

    @Theory
    public void gaussJordanInverse(Matrix matrix) {
        assumeTrue(matrix.isSquare());
        Matrix augmented = matrix.addColumns(identity(matrix.numberOfRows()));
        Matrix inverse = augmented.gaussJordanElimination().subColumns(matrix.numberOfRows());

        assertEquals(identity(matrix.numberOfRows()), inverse.multiply(matrix));
        assertEquals(identity(matrix.numberOfRows()), matrix.multiply(inverse));
    }
}
