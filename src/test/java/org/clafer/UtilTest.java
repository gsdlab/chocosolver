package org.clafer;

import java.util.Random;
import static org.junit.Assert.*;
import org.junit.Test;

/**
 *
 * @author jimmy
 */
public class UtilTest {

    private final Random rand = new Random();

    @Test
    public void testGcd() {
        for (int repeat = 0; repeat < 100; repeat++) {
            int a = Math.abs(rand.nextInt());
            int b = Math.abs(rand.nextInt());
            if (a == 0) {
                a = 1;
            }
            if (b == 0) {
                b = 1;
            }
            int gcd = Util.gcd(a, b);
            assertEquals(0, a % gcd);
            assertEquals(0, b % gcd);
            assertEquals(1, Util.gcd(a / gcd, b / gcd));
        }
    }

    @Test
    public void testShiftLeft() {
        for (int repeat = 0; repeat < 100000; repeat++) {
            int[] array = Util.range(0, rand.nextInt(50) );
            int shift = rand.nextInt(array.length);
            Util.shiftLeft(array, shift);

            for (int i = 0; i < array.length; i++) {
                assertEquals((i + shift) % array.length, array[i]);
            }
        }
    }
}
