package org.clafer.collection;

import java.util.Arrays;
import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertTrue;
import org.junit.Test;

/**
 *
 * @author jimmy
 */
public class EitherTest {

    private static Either<Integer, Integer> left(Integer left) {
        return Either.left(left);
    }

    private static Either<Integer, Integer> right(Integer right) {
        return Either.right(right);
    }

    @Test
    public void testLeft() {
        assertTrue(left(1).isLeft());
        assertFalse(left(1).isRight());
        assertEquals(1, left(1).getLeft().intValue());
    }

    @Test
    public void testRight() {
        assertFalse(right(2).isLeft());
        assertTrue(right(2).isRight());
        assertEquals(2, right(2).getRight().intValue());
    }

    @Test
    public void testFilterLefts() {
        @SuppressWarnings("unchecked")
        Either<Integer, Integer>[] eithers = new Either[]{
            left(1), left(2), right(3), left(4), right(5)};
        Integer[] lefts = new Integer[]{1, 2, 4};
        assertArrayEquals(lefts, Either.filterLeft(eithers));
        assertEquals(Arrays.asList(lefts), Either.filterLeft(Arrays.asList(eithers)));
    }

    @Test
    public void testFilterRights() {
        @SuppressWarnings("unchecked")
        Either<Integer, Integer>[] eithers = new Either[]{
            left(1), left(2), right(3), left(4), right(5)};
        Integer[] rights = new Integer[]{3, 5};
        assertArrayEquals(rights, Either.filterRight(eithers));
        assertEquals(Arrays.asList(rights), Either.filterRight(Arrays.asList(eithers)));
    }

    @Test
    public void testEquals() {
        assertEquals(left(1), left(1));
        assertNotEquals(left(1), left(2));
        assertEquals(right(1), right(1));
        assertNotEquals(right(1), right(2));
        assertNotEquals(left(1), right(1));
    }

    @Test
    public void testHashcode() {
        assertEquals(left(1).hashCode(), left(1).hashCode());
        assertEquals(right(1).hashCode(), right(1).hashCode());
    }
}
