package org.clafer.lsp;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;

import org.clafer.frontend.SourceSpan;
import org.eclipse.lsp4j.Position;
import org.eclipse.lsp4j.Range;
import org.junit.Test;

public class PositionsTest {

    @Test
    public void columnsWithoutTabsAreCharactersPlusOne() {
        Positions p = new Positions("abc\ndef\n");
        assertEquals(new Position(1, 2), p.toLsp(2, 3));
        assertArrayEquals(new int[]{2, 3}, p.fromLsp(new Position(1, 2)));
    }

    @Test
    public void aTabMovesToTheColumnAfterTheNextMultipleOfEight() {
        Positions p = new Positions("\tx\nab\tc\n");
        assertEquals(new Position(0, 1), p.toLsp(1, 9));
        assertArrayEquals(new int[]{1, 9}, p.fromLsp(new Position(0, 1)));
        // "ab" is columns 1 and 2, the tab moves from 3 to 9
        assertEquals(new Position(1, 3), p.toLsp(2, 9));
        assertArrayEquals(new int[]{2, 9}, p.fromLsp(new Position(1, 3)));
        // a column inside the tab is the tab
        assertEquals(new Position(1, 2), p.toLsp(2, 5));
    }

    @Test
    public void aCharacterOutsideTheBmpIsTwoCodeUnits() {
        // the lexer rejects it, but a diagnostic after it on its line must still land right
        Positions p = new Positions("😀x\n");
        assertEquals(new Position(0, 2), p.toLsp(1, 3));
    }

    @Test
    public void pastTheEnd() {
        Positions p = new Positions("ab\n");
        assertEquals(new Position(0, 2), p.toLsp(1, 7));
        assertEquals(new Position(1, 0), p.toLsp(5, 1));
    }

    @Test
    public void emptySpansAreWidenedToTheirCharacter() {
        Positions p = new Positions("ab\n");
        assertEquals(new Range(new Position(0, 1), new Position(0, 2)), p.toVisibleLsp(SourceSpan.at(1, 2)));
        assertEquals(new Range(new Position(0, 2), new Position(0, 2)), p.toVisibleLsp(SourceSpan.at(1, 3)));
    }
}
