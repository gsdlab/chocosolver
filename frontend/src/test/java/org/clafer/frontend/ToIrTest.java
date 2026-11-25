package org.clafer.frontend;

import java.util.List;
import static org.junit.Assert.assertEquals;
import org.junit.Test;

/**
 * Models in the full syntax with explicit constraints, each checked against
 * the rich explicit model that {@code clafer -m clafer} gives for it;
 * {@link ToIrConformanceTest} reads every explicit regression this way.
 */
public class ToIrTest {

    private static String explicit(String source) {
        ToIr.Result result = ToIr.toIr(source);
        assertEquals(List.of(), result.diagnostics());
        return ExplicitPrinter.print(result.model());
    }

    private static List<String> errors(String source) {
        return ToIr.toIr(source).diagnostics().stream().map(Diagnostic::toString).toList();
    }

    /**
     * The constraints of a reference to a set expression, a constant
     * reference, an enumeration, a nested assertion and objective, negative
     * numbers, binary chains and quantifiers.
     */
    @Test
    public void testConstructs() {
        assertEquals("""
                //# OPTIONS --explicit
                abstract 0..* P 0..* {}
                0..* Alice : P 1..1 {}
                0..* Bob : P 1..1 {}
                0..* friend -> P 1..1 {
                  [this.dref in (Alice ++ Bob)]
                }
                0..* nonzero -> int 1..1 {
                  [!(this.dref = 0)]
                }
                0..* small -> int 1..1 {
                  [(this.dref = 1) || (this.dref = 2)]
                }
                0..* k -> decimal 1..1 = 2.5 {}
                enum Color = red | green
                0..* paint -> Color 1..1 {}
                0..* A 1..1 {
                  0..* x -> int 1..1 {}
                  [this.x.dref != -1]
                  [this.x.dref != -1]
                  [this.x.dref = ((1 + 2) + 3)]
                  [all disj a; b : P | a != b]
                }
                [(A.x.dref < 5) && ((no friend) || (some paint))]

                << max this.x.dref >>
                assert [all this_ : A | this_.x.dref > 0]
                assert [some A]
                """, explicit("""
                abstract P
                Alice : P
                Bob : P
                friend -> Alice ++ Bob
                nonzero -> int -- 0
                small -> 1 ++ 2
                k -> 2.5
                enum Color = red | green
                paint -> Color
                A
                  x -> int
                  assert [this.x.dref > 0]
                  [this.x.dref != -1]
                  [this.x.dref != -(1)]
                  [this.x.dref = 1 + 2 + 3]
                  [all disj a; b : P | a != b]
                  << max this.x.dref >>
                [A.x.dref < 5 && (no friend || some paint)]
                assert [some A]
                """));
    }

    /**
     * A top-level abstract clafer that extends a nested one is built with
     * its constraints under its super's parent (gsdlab/clafer#67).
     */
    @Test
    public void testRelocated() {
        assertEquals("""
                //# OPTIONS --explicit
                abstract 0..* A 0..* {
                  abstract 0..* B 0..* {
                    0..* c -> int 1..1 {}
                  }
                  abstract 0..* D : B 0..* {
                    [this.c.dref > 1]
                  }
                }
                0..* E : A 1..1 {
                  0..* f : D 1..1 {
                    [this.c.dref = 2]
                  }
                }
                """, explicit("""
                abstract A
                  abstract B
                    c -> int
                abstract D : A.B
                  [this.c.dref > 1]
                E : A
                  f : D
                    [this.c.dref = 2]
                """));
    }

    /**
     * The constraints of each line of a clafer's elements, as the compiler
     * prints them.
     */
    private static List<String> constraints(String source) {
        ToIr.Result result = ToIr.toIr(source);
        assertEquals(List.of(), result.diagnostics().stream().map(Diagnostic::toString).toList());
        return ExplicitPrinter.print(result.model(), false).lines().filter(l -> l.contains("[")).map(String::strip)
                .toList();
    }

    /**
     * The names and types of constraints resolved as the compiler resolves
     * them ({@code clafer -m clafer}): the value of a reference, a nested
     * clafer from {@code this}, a name made unique, a let, a bare parent, sum,
     * the conjunction of a constraint's expressions, a path, and an unknown
     * name.
     */
    @Test
    public void testResolution() {
        String model = "abstract Fe\n  cost -> int\nGr : Fe\n  x\n  y\n    x\n";
        assertEquals(List.of("[Gr.cost.dref = 2]"), constraints(model + "[Gr.cost = 2]\n"));
        assertEquals(List.of("[some this.y]"), constraints(model + "  [some y]\n"));
        assertEquals(List.of("[some this.Gr__x]"), constraints(model + "  [this.x]\n"));
        assertEquals(List.of("[1 = 1]"), constraints(model + "  [let v = 1 in v = 1]\n"));
        assertEquals(List.of("[0 < 1]"), constraints(model + "  [parent]\n"));
        assertEquals(List.of("[(sum Fe.cost.dref) > 1]"), constraints(model + "  [sum Fe.cost > 1]\n"));
        assertEquals(List.of("[((Gr.cost.dref + 1) = 3) && (some this.Gr__x)]"),
                constraints(model + "  [Gr.cost.dref + 1 = 3 x]\n"));
        assertEquals(List.of("[some this.y.y__x]"), constraints(model + "  [ y.x ]\n"));
        assertEquals(List.of("7:9: Name resolver: 'z' not found within paths:Gr\nin context of '\"Gr\"'"),
                errors(model + "  [this.z]\n"));
    }
}
