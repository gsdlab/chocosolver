package org.clafer.frontend;

import java.util.List;
import java.util.stream.Collectors;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstUtil;
import org.clafer.ast.SourceSpan;
import org.clafer.ast.analysis.Analysis;
import org.clafer.ast.analysis.TypeAnalyzer;
import org.clafer.ast.analysis.TypeException;
import org.clafer.compiler.ClaferCompiler;
import org.clafer.compiler.ClaferSolver;
import org.clafer.scope.Scope;
import org.clafer.scope.ScopeFile;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import org.junit.Test;

public class ExplicitParserTest {

    private static List<String> diagnostics(String source) {
        return ExplicitParser.parse(source).diagnostics().stream().map(Diagnostic::toString).collect(Collectors.toList());
    }

    @Test
    public void testSolve() {
        ExplicitParser.Result result = ExplicitParser.parse("""
                abstract 0..* c0_T -> int 0..* {}
                0..* c0_A 1..1 {
                  [(#this . c0_b) = 2]
                  0..* c0_b : c0_T -> int 0..* {
                    [this . dref > 0]
                  }
                }
                """);
        assertEquals(List.of(), result.diagnostics());
        Scope scope = ScopeFile.parse("""
                [{"lpqName": "c0_A", "scope": 1}, {"lpqName": "c0_b", "scope": 3}, {"int": [-4, 3]}]
                """, "test.cfr-scope").applyTo(Scope.builder().toScope(), result.model());
        ClaferSolver solver = ClaferCompiler.compile(result.model(), scope);
        assertTrue(solver.find());
        assertEquals(2, solver.instance().getTopClafers()[0].getChildren().length);
    }

    /**
     * A clafer without a written reference inherits its super clafer's; a
     * written one is its own, a redefinition even with the same target.
     */
    @Test
    public void testReferences() {
        ExplicitParser.Result result = ExplicitParser.parse("""
                abstract 0..* c0_T ->> int 0..* {}
                0..* c0_A : c0_T 1..1 {}
                0..* c0_B : c0_T ->> int 1..1 {}
                """);
        assertEquals(List.of(), result.diagnostics());
        List<AstConcreteClafer> clafers = result.model().getChildren();
        AstConcreteClafer a = clafers.get(0);
        AstConcreteClafer b = clafers.get(1);
        assertTrue(!a.hasRef());
        assertEquals(a.getSuperClafer().getRef(), AstUtil.getInheritedRef(a));
        assertTrue(b.hasRef());
        assertTrue(b.getRef() != a.getSuperClafer().getRef());
    }

    /**
     * noclafer, the empty set, prints and parses back, with the type of the
     * set beside it, or of the model's first clafer when it stands alone.
     */
    @Test
    public void testNoClafer() {
        String source = """
                [(if ((#c0_A) >= 1) then c0_B else noclafer) = c0_B]
                [(c0_B ++ noclafer) = c0_B]
                [(#noclafer) = 0]
                0..* c0_A 0..2 {}
                0..* c0_B 0..1 {}
                """;
        ExplicitParser.Result result = ExplicitParser.parse(source);
        assertEquals(List.of(), result.diagnostics());
        assertEquals(source, org.clafer.ast.AstPrinter.print(result.model()));
        // typed by the set beside it: an empty set of c0_B
        AstConstraintTypes types = new AstConstraintTypes(result);
        assertEquals("c0_B", types.emptyOf(0));
        assertEquals("c0_B", types.emptyOf(1));
        // alone: of the model's first clafer
        assertEquals("c0_A", types.emptyOf(2));
        ClaferSolver solver = ClaferCompiler.compile(result.model(), Scope.defaultScope(2).toScope());
        assertTrue(solver.find());
    }

    /**
     * The type of the empty constant in each top-level constraint.
     */
    private record AstConstraintTypes(ExplicitParser.Result result) {

        String emptyOf(int constraint) {
            org.clafer.ast.AstExpr expr = result.model().getAbstractRoot().getConstraints().get(constraint).getExpr();
            return find(expr);
        }

        private static String find(Object node) {
            if (node instanceof org.clafer.ast.AstConstant c && c.value()[0].length == 0) {
                return c.type().get(0).getName();
            }
            if (node instanceof Record r) {
                for (java.lang.reflect.RecordComponent rc : r.getClass().getRecordComponents()) {
                    try {
                        Object value = rc.getAccessor().invoke(r);
                        Object[] children = value instanceof Object[] array ? array : new Object[]{value};
                        for (Object child : children) {
                            String found = find(child);
                            if (found != null) {
                                return found;
                            }
                        }
                    } catch (ReflectiveOperationException e) {
                        throw new AssertionError(e);
                    }
                }
            }
            return null;
        }
    }

    @Test
    public void testSpans() {
        ExplicitParser.Result result = ExplicitParser.parse("0..* c0_A 1..1 {\n  [this . dref = 1]\n}\n");
        AstClafer a = result.model().getChildren().get(0);
        assertEquals(new SourceSpan(1, 6, 1, 10), a.getSpan());
        assertEquals(new SourceSpan(2, 3, 2, 20), a.getConstraints().get(0).getSpan());
    }

    /**
     * A type error in a parsed constraint names its position.
     */
    @Test
    public void testTypeErrorHasThePosition() {
        ExplicitParser.Result result = ExplicitParser.parse("0..* c0_A 1..1 {\n  [this . dref = 1]\n}\n");
        assertEquals(List.of(), result.diagnostics());
        try {
            Analysis.analyze(result.model(), Scope.builder().toScope(), new TypeAnalyzer());
            fail("Expected a type error: c0_A has no reference");
        } catch (TypeException e) {
            assertTrue(e.getMessage(), e.getMessage().startsWith("2:3: "));
        }
    }

    @Test
    public void testErrorsAndRecovery() {
        assertEquals(List.of(
                "1:13: No clafer named c0_X",
                "2:20: Parenthesize: '=' and '+' without parentheses",
                "5:6: Expected the name of a clafer but found '{'"),
                diagnostics("""
                        0..* c0_A : c0_X 1..1 {
                          [this . dref = 1 + 2 - 3]
                          0..* c0_b 0..1 {}
                        }
                        0..* { }
                        0..* c0_C 1..1 {}
                        """));
    }

    @Test
    public void testOneMistakePerConstraint() {
        ExplicitParser.Result result = ExplicitParser.parse("""
                0..* c0_A 1..1 {
                  [c0_nope = 1]
                  [this = c0_A]
                }
                """);
        assertEquals(List.of("2:4: No clafer named c0_nope"),
                result.diagnostics().stream().map(Diagnostic::toString).collect(Collectors.toList()));
        // The other constraint is kept.
        assertEquals(1, ((AstConcreteClafer) result.model().getChildren().get(0)).getConstraints().size());
    }
}
