package org.clafer.frontend;

import java.util.List;
import java.util.stream.Collectors;
import static org.junit.Assert.assertEquals;
import org.junit.Test;

/**
 * The symbols on small models, each checked against the compiler's rich
 * explicit model or its error; {@link SymbolsConformanceTest} compares the
 * symbols of Clafer's test models.
 */
public class SymbolsTest {

    private static Symbols.Result analyze(String source) {
        ClaferParser.Result parsed = ClaferParser.parse(source);
        assertEquals(List.of(), parsed.diagnostics());
        return Symbols.analyze(parsed.model());
    }

    private static String outline(String source) {
        Symbols.Result symbols = analyze(source);
        assertEquals(List.of(), symbols.diagnostics());
        return Symbols.outline(symbols);
    }

    private static List<String> errors(String source) {
        return analyze(source).diagnostics().stream().map(Diagnostic::toString).collect(Collectors.toList());
    }

    /**
     * An abstract clafer is {@code 0..*}; a clafer is mandatory at the top
     * level and under the default group or a written {@code 0..*}, and
     * optional under a keyword group, {@code opt} included; a group card is
     * inherited.
     */
    @Test
    public void testDefaultCards() {
        assertEquals("""
                abstract 0..* Base 0..*
                  1..1 kind 1..1
                    0..* a 0..1
                    0..* b 0..1
                  0..* extras 1..1
                    0..* e 0..1
                  0..* list 1..1
                    0..* l 1..1
                0..* A : Base 1..1
                  0..* c 2..3
                abstract 1..* Group 0..*
                  0..* m 0..1
                1..* Gr : Group 1..1
                  0..* g 0..1
                  0..* h 0..1
                """, outline("""
                abstract Base
                  xor kind
                    a
                    b
                  opt extras
                    e
                  0..* list
                    l
                A : Base
                  c 2..3
                abstract or Group
                  m
                Gr : Group
                  g
                  h ?
                """));
    }

    /**
     * A UID is the name, qualified only as much as needed, from the root if
     * the whole path is the end of another; an apostrophe is
     * {@code _prime}, and a UID taken already gets a suffix.
     */
    @Test
    public void testUids() {
        assertEquals("""
                0..* root__size 1..1
                0..* x 1..1
                  0..* x__size 1..1
                0..* y 1..1
                  0..* z 1..1
                    0..* z__size 1..1
                0..* a_prime 1..1
                0..* a_prime__2 1..1
                enum Color = red | green
                0..* p 1..1
                  0..* red_prime 1..1
                """, outline("""
                size
                x
                  size
                y
                  z
                    size
                a'
                a_prime
                enum Color = red | green
                p
                  red'
                """));
    }

    /**
     * The type of a reference: a primitive super, a constant, a set
     * expression of primitives or of clafers, a path; the modifiers and the
     * clocks of the initializers.
     */
    @Test
    public void testReferences() {
        assertEquals("""
                abstract 0..* Person 0..*
                  abstract 0..* Head 0..*
                0..* Alice : Person 1..1
                0..* Bob : Person 1..1
                0..* n -> int 1..1
                0..* k -> decimal 1..1 =
                0..* s ->> string 1..1 =
                0..* d -> decimal 1..1
                0..* friend -> Person 1..1
                0..* head -> Head 1..1
                0..* nonzero -> int 1..1
                final 0..* f -> Person 1..1
                finalref finaltarget 0..* r -> Person 1..1
                initial 0..* i -> int 1..1 := each
                0..* t -> int 0..* := together
                """, outline("""
                abstract Person
                  abstract Head
                Alice : Person
                Bob : Person
                n : integer
                k -> 2.5
                s ->> "x"
                d -> 1, 2.5
                friend -> Alice ++ Bob
                head -> Person.Head
                nonzero -> int -- 0
                final f -> Person
                finalref finaltarget r -> Person
                initial i -> integer := 3
                t -> integer * := 0
                """));
    }

    /**
     * A clafer use is a clafer extending the clafer it names, and a
     * top-level abstract clafer extending a nested one becomes a sibling of
     * its super (gsdlab/clafer#67).
     */
    @Test
    public void testNesting() {
        assertEquals("""
                abstract 0..* A 0..*
                  abstract 0..* B 0..*
                    0..* c 1..1
                  abstract 0..* D : B 0..*
                0..* E : A 1..1
                  0..* f : B 1..1
                  0..* E__Fe : root__Fe 0..1
                abstract 0..* root__Fe 0..*
                """, outline("""
                abstract A
                  abstract B
                    c
                abstract D : A.B
                E : A
                  f : B
                  `Fe ?
                abstract Fe
                """));
    }

    @Test
    public void testSymbols() {
        Symbols.Result symbols = analyze("abstract A\n  b\nC : A\n");
        Syntax.Clafer c = (Syntax.Clafer) ClaferParser.parse("abstract A\n  b\nC : A\n").model().declarations().get(1);
        assertEquals(List.of("A", "b", "C"), symbols.all().stream().map(Symbols.ClaferSymbol::uid).toList());
        Symbols.ClaferSymbol a = symbols.topLevel().get(0);
        Symbols.ClaferSymbol sub = symbols.topLevel().get(1);
        assertEquals(a, sub.superClafer().orElseThrow());
        assertEquals(a, a.children().get(0).parent().orElseThrow());
        assertEquals(sub, symbols.of(sub.declaration()).orElseThrow());
        assertEquals(new SourceSpan(3, 1, 3, 2), sub.nameSpan());
        assertEquals(true, symbols.of(c).isEmpty());
    }

    /**
     * The errors of the supers, at the compiler's positions.
     */
    @Test
    public void testSuperErrors() {
        assertEquals(List.of("2:5: No superclafer found: B"), errors("B\nC : B\n"));
        assertEquals(List.of("1:1: clafer A inherits from itself", "2:1: clafer B inherits from itself"),
                errors("abstract A : B\nabstract B : A\n"));
        assertEquals(List.of("1:1: clafer A inherits from itself"), errors("abstract A : A\n"));
        assertEquals(List.of("5:5: clafer \"T\" is not unique. Available paths: A.T, B.T"),
                errors("abstract A\n  abstract T\nabstract B\n  abstract T\nC : T\n"));
        assertEquals(List.of("3:5: No superclafer found: A.b is not abstract, and only abstract clafers can be supers"),
                errors("A\n  b\nC : A.b\n"));
        assertEquals(List.of("2:5: A clafer with a reference cannot have a primitive type as its super: integer"),
                errors("abstract A\nb : integer -> A\n"));
    }

    /**
     * The nesting and the refinement of a redefined clafer, the
     * satisfiability of a group card, and duplicate siblings.
     */
    @Test
    public void testChecks() {
        assertEquals(List.of("7:1: Improperly nested clafer 'D'"),
                errors("abstract A\n  abstract B\n\n\nC : A\n\nD : B\n"));
        assertEquals(List.of("4:3: Improper bag to set refinement for clafer 'x'"),
                errors("abstract A\n  abstract x -> integer\nB : A\n  x : x ->> integer\n"));
        assertEquals(List.of("6:3: Improper target subtyping for clafer 'x'"),
                errors("abstract A\n  abstract x -> double\nB : A\n  x : x -> integer\nC : A\n  x : x -> string\n"));
        assertEquals(List.of("6:3: Improper target subtyping for clafer 'x'"),
                errors("abstract T\nabstract V\nabstract A\n  abstract x -> T\nB : A\n  x : x -> V\n"));
        assertEquals(List.of("2:3: The group cardinality 2..3 of c cannot be satisfied: its children,"
                + " including the inherited ones, can have at most 1 instances"),
                errors("A\n  2..3 c\n    d\n"));
        assertEquals(List.of("2:3: Duplicate name: b"), errors("a\n  b\n  b\n"));
        assertEquals(List.of("1:6: Name resolver: 'B' not found"), errors("a -> B\n"));
    }
}
