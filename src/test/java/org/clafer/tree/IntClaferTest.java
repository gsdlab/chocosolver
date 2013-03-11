package org.clafer.tree;

import org.clafer.collection.IntPair;
import org.junit.Test;
import static org.junit.Assert.*;

/**
 *
 * @author jimmy
 */
public class IntClaferTest {

    @Test
    public void testBitwidth() {
        IntPair[] answers = new IntPair[]{
            null,
            new IntPair(-1, 0),
            new IntPair(-2, 1),
            new IntPair(-4, 3),
            new IntPair(-8, 7),
            new IntPair(-16, 15),
            new IntPair(-32, 31),
            new IntPair(-64, 63),
            new IntPair(-128, 127),
            new IntPair(-256, 255),
            new IntPair(-512, 511),
            new IntPair(-1024, 1023),
            new IntPair(-2048, 2047),
            new IntPair(-4096, 4095),
            new IntPair(-8192, 8191),
            new IntPair(-16384, 16383),
            new IntPair(-32768, 32767),
            new IntPair(-65536, 65535),
            new IntPair(-131072, 131071),
            new IntPair(-262144, 262143),
            new IntPair(-524288, 524287),
            new IntPair(-1048576, 1048575),
            new IntPair(-2097152, 2097151),
            new IntPair(-4194304, 4194303),
            new IntPair(-8388608, 8388607),
            new IntPair(-16777216, 16777215),
            new IntPair(-33554432, 33554431),
            new IntPair(-67108864, 67108863),
            new IntPair(-134217728, 134217727),
            new IntPair(-268435456, 268435455),
            new IntPair(-536870912, 536870911),
        };
        for(int i = 1; i < answers.length; i++) {
            IntClafer c = new IntClafer(i);
            assertEquals(answers[i].getFst(), c.getScopeLow());
            assertEquals(answers[i].getSnd(), c.getScopeHigh());
        }
    }

}
