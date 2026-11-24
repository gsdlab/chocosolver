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
     * What the compiler resolves, and this pass does not.
     */
    @Test
    public void testNeedsResolution() {
        String model = "abstract Fe\n  cost -> int\nGr : Fe\n  x\n  y\n    x\n";
        assertEquals(List.of("7:2: The value of cost without .dref (name and type resolution, step 10)"),
                errors(model + "[Gr.cost = 2]\n"));
        assertEquals(List.of("7:9: The nested clafer y without a path from this or the top level"
                + " (name and type resolution, step 10)"), errors(model + "  [some y]\n"));
        assertEquals(List.of("7:9: The name x is not unique, and not a UID (name and type resolution, step 10)"),
                errors(model + "  [this.x]\n"));
        assertEquals(List.of("7:4: let is not lowered yet (name and type resolution, step 10)"),
                errors(model + "  [let v = 1 in v = 1]\n"));
        assertEquals(List.of("7:4: A bare parent (name and type resolution, step 10)"), errors(model + "  [parent]\n"));
        assertEquals(List.of("7:4: sum without .dref (name and type resolution, step 10)"),
                errors(model + "  [sum Fe.cost > 1]\n"));
        assertEquals(List.of("7:9: No clafer has the name or UID z (name and type resolution, step 10)"),
                errors(model + "  [this.z]\n"));
    }
}
