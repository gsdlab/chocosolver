package org.clafer.ast;

import static org.clafer.ast.Asts.Mandatory;
import static org.clafer.ast.Asts.Optional;
import static org.clafer.ast.Asts.equal;
import static org.clafer.ast.Asts.global;
import static org.clafer.ast.Asts.newModel;
import static org.clafer.ast.Asts.some;
import static org.clafer.ast.Asts.union;
import org.clafer.assertion.Assertion;
import org.clafer.ast.analysis.Analysis;
import org.clafer.ast.analysis.TypeAnalyzer;
import org.clafer.ast.analysis.TypeException;
import org.clafer.scope.Scope;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import org.junit.Test;

/**
 * Source positions on clafers, constraints and assertions.
 */
public class SourceSpanTest {

    @Test
    public void testFormat() {
        SourceSpan span = new SourceSpan(3, 5, 3, 12);
        assertEquals("3:5-3:12", span.toString());
        assertEquals("3:5: Cannot join", span.format("Cannot join"));
        assertEquals(new SourceSpan(4, 1, 4, 1), SourceSpan.at(4, 1));
    }

    @Test(expected = IllegalArgumentException.class)
    public void testEndBeforeStart() {
        new SourceSpan(3, 5, 3, 4);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testZeroLine() {
        new SourceSpan(0, 1, 1, 1);
    }

    @Test
    public void testClaferSpan() {
        AstModel model = newModel();
        AstConcreteClafer a = model.addChild("A");
        assertFalse(a.hasSpan());
        assertNull(a.getSpan());

        // at keeps the builder's type
        AstConcreteClafer b = model.addChild("B").at(new SourceSpan(2, 1, 2, 6)).withCard(Mandatory);
        assertEquals(new SourceSpan(2, 1, 2, 6), b.getSpan());
        AstAbstractClafer c = model.addAbstract("C").at(SourceSpan.at(1, 10));
        assertTrue(c.hasSpan());
    }

    @Test
    public void testConstraintSpan() {
        AstModel model = newModel();
        AstConcreteClafer a = model.addChild("A").withCard(Optional);
        AstConstraint without = model.addConstraint(some(global(a)));
        AstConstraint with = a.addConstraint(some(global(a)), new SourceSpan(2, 5, 2, 14));
        assertFalse(without.hasSpan());
        assertEquals(new SourceSpan(2, 5, 2, 14), with.getSpan());
        assertEquals("[#(A) >= 1]", without.toString());
        assertEquals("[#(A) >= 1] at 2:5-2:14", with.toString());
        assertEquals("assert #(A) >= 1 at 7:1-7:9",
                new Assertion(some(global(a)), new SourceSpan(7, 1, 7, 9)).toString());
    }

    /**
     * <pre>
     * abstract A
     * B : A
     * C ?
     *
     * [C = A ++ B]   // line 5
     * </pre>
     */
    @Test
    public void testTypeErrorNamesThePosition() {
        AstModel model = newModel();
        AstAbstractClafer a = model.addAbstract("A");
        AstConcreteClafer b = model.addChild("B").extending(a).withCard(Mandatory);
        AstConcreteClafer c = model.addChild("C").withCard(Optional);
        model.addConstraint(equal(global(c), union(global(a), global(b))), new SourceSpan(5, 1, 5, 12));
        try {
            Analysis.analyze(model, Scope.defaultScope(1), new TypeAnalyzer());
            fail("Expected a type error.");
        } catch (TypeException e) {
            assertTrue(e.getMessage(), e.getMessage().startsWith("5:1: "));
        }
    }

    /**
     * Expressions are records: equal when their parts are, except locals,
     * which are variables.
     */
    @Test
    public void testExpressionEquality() {
        AstModel model = newModel();
        AstConcreteClafer a = model.addChild("A");
        AstConcreteClafer b = a.addChild("B");
        assertEquals(Asts.join(global(a), b), Asts.join(global(a), b));
        assertEquals(Asts.constant(Asts.IntType, 1, 2), Asts.constant(Asts.IntType, 1, 2));
        assertEquals(Asts.join(global(a), b).hashCode(), Asts.join(global(a), b).hashCode());
        assertFalse(Asts.local("x").equals(Asts.local("x")));
        AstLocal x = Asts.local("x");
        assertSame(x, ((AstJoin) Asts.join(x, b)).left());
    }
}
