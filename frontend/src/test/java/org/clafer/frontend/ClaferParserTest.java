package org.clafer.frontend;

import java.util.List;
import java.util.stream.Collectors;
import org.clafer.frontend.Syntax.*;
import static org.junit.Assert.assertEquals;
import org.junit.Test;

/**
 * The parser of the full language on what the compiler's trees do not show:
 * the errors on several lines, the recovery, the spans, and the lexer's
 * columns and characters. {@link ClaferParserConformanceTest} compares the
 * trees with the compiler's.
 */
public class ClaferParserTest {

    private static List<String> diagnostics(String source) {
        return ClaferParser.parse(source).diagnostics().stream().map(Diagnostic::toString).collect(Collectors.toList());
    }

    private static List<String> names(List<? extends Declaration> declarations) {
        return declarations.stream().map(d -> d instanceof Clafer c ? c.name().text() : d.getClass().getSimpleName())
                .collect(Collectors.toList());
    }

    /**
     * Each line with an error is reported, and the parser goes on at the next
     * line at the same or a lower indentation: the rest of the tree is kept.
     * An error at a token that the layout inserted is where the compiler puts
     * it, past one space after the token before it.
     */
    @Test
    public void testErrorsOnSeveralLines() {
        String source = """
                A
                  b -> int =
                  c
                    [ c + ]
                  d
                B : -> A
                C
                """;
        assertEquals(List.of(
                "2:14: Expected an expression but found a new line",
                "4:11: Expected an expression but found ']'",
                "6:5: Expected an expression but found '->'"),
                diagnostics(source));
        Model model = ClaferParser.parse(source).model();
        assertEquals(List.of("A", "C"), names(model.declarations()));
        Clafer a = (Clafer) model.declarations().get(0);
        assertEquals(List.of("c", "d"), names(a.elements().get().elements()));
    }

    /**
     * The errors of the lexer, the layout and the parser, in the order of
     * their positions, with the clafers on one line.
     */
    @Test
    public void testAllKindsOfErrors() {
        assertEquals(List.of(
                "1:3: Unexpected character '$'",
                "2:1: Expected a clafer, a constraint, an objective, an assertion or an enumeration"
                + " but found the reserved word 'some'",
                "3:8: clafer c must start on a new line"),
                diagnostics("""
                        a $
                        some
                        b 2..3 c
                        """));
    }

    @Test
    public void testSpans() {
        Model model = ClaferParser.parse("abstract A : B -> int ? = 3\n  [ this.dref > 1 ]\n").model();
        Clafer a = (Clafer) model.declarations().get(0);
        assertEquals(new SourceSpan(1, 1, 2, 20), a.span());
        assertEquals(new SourceSpan(1, 10, 1, 11), a.name().span());
        assertEquals(new SourceSpan(1, 16, 1, 22), a.reference().get().span());
        assertEquals(new SourceSpan(1, 25, 1, 28), a.init().get().span());
        Constraint c = (Constraint) a.elements().get().elements().get(0);
        assertEquals(new SourceSpan(2, 3, 2, 20), c.span());
        assertEquals(new SourceSpan(2, 5, 2, 18), c.exps().get(0).span());
    }

    /**
     * A tab moves to the column after the next multiple of 8, as in the
     * compiler, so a tab indents as 8 spaces do.
     */
    @Test
    public void testTabs() {
        Model model = ClaferParser.parse("a\n\tb\n        c\n\t\td $\n").model();
        Clafer a = (Clafer) model.declarations().get(0);
        assertEquals(List.of("b", "c"), names(a.elements().get().elements()));
        assertEquals(List.of("4:19: Unexpected character '$'"), diagnostics("a\n\tb\n        c\n\t\td $\n"));
    }

    /**
     * The compiler's characters are those of Latin-1: a name may have
     * {@code é}, and a character after {@code U+00FF} is an error, in a
     * string or a comment too, once in each.
     */
    @Test
    public void testLatin1() {
        assertEquals(List.of(), diagnostics("café -> string = \"naïve\" // déjà\n"));
        assertEquals(List.of(
                "1:2: Unexpected character U+00D7",
                "1:3: clafer b must start on a new line",
                "2:18: Unexpected character U+017C: only Latin-1 characters are allowed",
                "3:5: Unexpected character U+0105: only Latin-1 characters are allowed"),
                diagnostics("a×b\nc -> string = \"zażółć\"\n// Bąk ł\n"));
    }

    /**
     * Layout is off inside brackets and braces; a line that starts with a
     * comment is indented at the comment.
     */
    @Test
    public void testLayout() {
        Model model = ClaferParser.parse("a {\nb\n}\nc\n  [ c\n= c ]\n/* x */ d\n").model();
        assertEquals(List.of("a", "c", "d"), names(model.declarations()));
        assertEquals(List.of("b"), names(((Clafer) model.declarations().get(0)).elements().get().elements()));
    }
}
