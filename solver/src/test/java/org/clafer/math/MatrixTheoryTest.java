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
public class MatrixTheoryTest {

    @DataPoints
    public static final Matrix[] matrices = new Matrix[]{
        new Matrix(new long[][]{
            {1, 2, 3},
            {3, 4, 5},
            {6, 7, 9}
        }),
        Matrix.identity(3),
        new Matrix(2, 5, 3)
    };

    @Theory
    public void transposeInverseTranspose(Matrix matrix) {
        assertEquals(matrix, matrix.transpose().transpose());
    }

    @Theory
    public void addColumnInverseSubColumn(Matrix matrix, Matrix columns) {
        assertEquals(matrix, matrix.addColumns(columns).subColumns(0, matrix.numberOfColumns()));
    }

    @Theory
    public void gaussJordanInverse(Matrix matrix) {
        assumeTrue(matrix.isSquare());
        int n = matrix.numberOfRows();
        Matrix augmented = matrix.addColumns(identity(n)).gaussJordanElimination();

        if (augmented.subColumns(0, matrix.numberOfColumns()).equals(identity(n))) {
            Matrix inverse = augmented.subColumns(n);
            assertEquals(identity(n), inverse.multiply(matrix));
            assertEquals(identity(n), matrix.multiply(inverse));
        }
    }
}
