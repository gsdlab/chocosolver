package org.clafer.scope;

import java.math.BigDecimal;
import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstModel;
import static org.clafer.ast.Asts.newModel;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import org.junit.Test;

public class ScopeFileTest {

    private final AstModel model = newModel();
    private final AstAbstractClafer t = model.addAbstract("T");
    private final AstConcreteClafer a = model.addChild("A").withCard(1, 1);
    private final AstConcreteClafer b = a.addChild("b").extending(t);

    private Scope apply(String json) {
        return ScopeFile.parse(json, "test.cfr-scope").applyTo(Scope.defaultScope(2).toScope(), model);
    }

    @Test
    public void testPrint() {
        Scope scope = Scope.setScope(b, 3).defaultScope(1).intLow(-4).intHigh(3).mulLow(-4).mulHigh(3).stringLength(5)
                .decimalScale(2).decimalLow(new BigDecimal("-100.5")).decimalHigh(new BigDecimal("100.25")).toScope();
        assertEquals("""
                [
                {"lpqName": "A", "scope": 1},
                {"lpqName": "T", "scope": 1},
                {"lpqName": "b", "scope": 3},
                {"defaultScope": 1},
                {"int": [-4, 3]},
                {"mul": [-4, 3]},
                {"stringLength": 5},
                {"chars": [32, 126]},
                {"decimalDigits": 2},
                {"decimal": [-100.50, 100.25]}
                ]
                """, ScopeFile.print(model, scope));
        assertEquals(ScopeFile.print(model, scope), ScopeFile.print(model, apply(ScopeFile.print(model, scope))));
    }

    @Test
    public void testRanges() {
        Scope scope = apply("""
                [{"int": [-8, 7]}, {"stringLength": 4}, {"chars": [97, 122]},
                 {"decimalDigits": 1}, {"decimal": [-2.5, 2.5]}]
                """);
        assertEquals(-8, scope.getIntLow());
        assertEquals(7, scope.getIntHigh());
        assertEquals(4, scope.getStringLength());
        assertEquals('a', scope.getCharLow());
        assertEquals('z', scope.getCharHigh());
        assertEquals(1, scope.getDecimalScale());
        assertEquals(new BigDecimal("-2.5"), scope.unscaleDecimal(scope.getDecimalLow()));
        assertEquals(new BigDecimal("2.5"), scope.unscaleDecimal(scope.getDecimalHigh()));
        // Clafers keep their scopes.
        assertEquals(2, scope.getScope(b));
        assertTrue(ScopeFile.parse("[{\"int\": [0, 1]}]", "x").hasIntRange());
    }

    @Test
    public void testNames() {
        assertEquals(5, apply("[{\"lpqName\": \"b\", \"scope\": 5}]").getScope(b));
        assertEquals(5, apply("[{\"lpqName\": \"A::b\", \"scope\": 5}]").getScope(b));
        // the root, as the REPL saves it
        assertEquals(2, apply("[{\"lpqName\": \"\", \"scope\": 1}]").getScope(b));
        assertEquals(2, apply("[ ]").getScope(b));
    }

    private static void assertMalformed(String json, String message) {
        try {
            ScopeFile.parse(json, "test.cfr-scope");
            fail("Expected an error: " + json);
        } catch (IllegalArgumentException e) {
            assertTrue(e.getMessage(), e.getMessage().startsWith("test.cfr-scope: ") && e.getMessage().contains(message));
        }
    }

    @Test
    public void testMalformed() {
        assertMalformed("[{\"lpqName\": \"b\", \"scope\": two}]", "expected a number");
        assertMalformed("[{\"lpqName\": \"b\", \"scope\": -1}]", "natural number");
        assertMalformed("[{\"lpqName\": \"b\"}]", "\"lpqName\": name");
        assertMalformed("[{\"int\": [3, 1]}]", "empty");
        assertMalformed("[{\"int\": [1.5, 2]}]", "an integer");
        assertMalformed("[{\"ints\": [1, 2]}]", "unknown entry \"ints\"");
        assertMalformed("[{\"int\": [1, 2], \"mul\": [1, 2]}]", "one range per entry");
        assertMalformed("[{\"int\": [1, 2]}] x", "after the list");
        assertMalformed("{\"int\": [1, 2]}", "expected '['");
    }

    @Test(expected = IllegalArgumentException.class)
    public void testUnknownClafer() {
        apply("[{\"lpqName\": \"c0_nope\", \"scope\": 2}]");
    }
}
