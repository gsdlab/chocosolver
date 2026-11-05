package org.clafer.frontend;

import java.util.List;
import java.util.stream.Collectors;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.SourceSpan;
import org.clafer.ast.analysis.Analysis;
import org.clafer.ast.analysis.TypeAnalyzer;
import org.clafer.ast.analysis.TypeException;
import org.clafer.compiler.ClaferCompiler;
import org.clafer.compiler.ClaferSolver;
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
                scope c0_A 1
                scope c0_b 3
                abstract 0..* c0_T -> int 0..* {}
                0..* c0_A 1..1 {
                  [(#this . c0_b) = 2]
                  0..* c0_b : c0_T -> int 0..* {
                    [this . dref > 0]
                  }
                }
                """);
        assertEquals(List.of(), result.diagnostics());
        ClaferSolver solver = ClaferCompiler.compile(result.model(), result.scope());
        assertTrue(solver.find());
        assertEquals(2, solver.instance().getTopClafers()[0].getChildren().length);
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
            Analysis.analyze(result.model(), result.scope(), new TypeAnalyzer());
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
