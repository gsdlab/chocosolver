package org.clafer.frontend;

import java.util.List;
import static org.junit.Assert.assertEquals;
import org.junit.Test;

/**
 * The compiler's checks of a whole model ({@link ModelChecks}), each with the
 * compiler's errors ({@code clafer -m clafer}) as the expected ones. The
 * compiler stops at the first error of a check; the frontend gives all the
 * errors of the check, the first of which is the compiler's.
 */
public class ModelChecksTest {

    private static List<String> messages(String model) {
        return ToIr.toIr(model).messages();
    }

    private static String clockless(String name) {
        return "The initializer ':=' of " + name + " has no clock, which an explicit model writes: ':= each'"
                + " initializes each instance of " + name + " at its birth, and ':= together' all the instances of "
                + name + " under a parent together.";
    }

    private static String bound(String name, String multiplicity) {
        return "The initializer ':=' of the top-level abstract clafer " + name + " with the multiplicity "
                + multiplicity + " requires an explicit clock: ':= each' initializes each instance of " + name
                + " at its birth, and ':= together' all the instances of " + name + " together at time zero."
                + " Without a clock, ':=' initializes each instance without a multiplicity, and all the instances"
                + " together with * (or 0..*).";
    }

    private static String notScalar(String name, String value) {
        return "The initializer ':=' of the abstract clafer " + name + " gives each instance of " + name
                + " its initial value, which must have at most one element, but that could not be established for '"
                + value + "' from the declarations (literals, clafers with at most one instance, and the operators"
                + " that keep one element). To initialize all the instances of " + name + " together, write"
                + " ':= together': abstract " + name + " -> ... := together ...";
    }

    /**
     * Clafers, enumerations, literals and clafer uses named after keywords.
     */
    @Test
    public void testKeywordNames() {
        assertEquals(List.of("The model contains clafers with keywords as names in the following places:\n"
                + "Line 1 column 1\nLine 3 column 3\nLine 4 column 3\nLine 5 column 12\n"),
                messages("abstract integer -> integer\nA\n  string\n  int ?\nenum Col = real | b\nB : integer\n"
                        + "  this2\n"));
        assertEquals(List.of("The model contains clafers with keywords as names in the following places:\n"
                + "Line 1 column 1\nLine 3 column 3\n"), messages("enum integer = a | b\nabstract A\n  int\nB\n  `A\n"));
        assertEquals(List.of("The model contains clafers with keywords as names in the following places:\n"
                + "Line 2 column 3\nLine 3 column 1\n"), messages("A\n  `integer\nabstract integer\n"));
    }

    /**
     * The initializers of an explicit model have clocks.
     */
    @Test
    public void testClocklessInitializers() {
        assertEquals(List.of("2:20: " + clockless("n"), "4:22: " + clockless("k")),
                messages("//# OPTIONS --explicit\n0..* n -> int 1..1 := 3 {\n  0..* m -> int 0..* := together 3 {}\n"
                        + "  0..* k -> int 0..1 := 2 {}\n}\n"));
        assertEquals(List.of("3:13: " + clockless("a")),
                messages("//# OPTIONS --explicit --core\nenum Col = r | g\n0..* a 1..1 := 3 {}\n"));
        // the rich model has no clock: := of a single reference is := each
        assertEquals(List.of(), messages("n -> int := 3\n"));
    }

    /**
     * A top-level abstract clafer with := and no clock has the multiplicity
     * *.
     */
    @Test
    public void testInitializerBounds() {
        assertEquals(List.of("1:25: " + bound("Val", "?"), "2:24: " + bound("Wa", "2..3")),
                messages("abstract Val -> integer ? := 1\nabstract Wa -> integer 2..3 := 1\n"
                        + "abstract Xa -> integer * := 1\nabstract Ya -> integer 0..* := 1\n"
                        + "abstract Za -> integer 1 := each 1\nA\n  abstract N -> integer ? := 1\nv : Val\nw : Wa\n"));
    }

    /**
     * The per-instance initial value of an abstract clafer has at most one
     * element.
     */
    @Test
    public void testInitialValues() {
        String decls = "s -> integer\nt -> integer *\nu\n  w -> integer\nv : Val\n";
        assertEquals(List.of("1:28: " + notScalar("Val", "1 ++ 2"), "5:27: " + notScalar("V5", "t.dref"),
                "11:28: " + notScalar("V11", "1 + 2 * 3 ++ 4")),
                messages("abstract Val -> integer := 1, 2\nabstract V2 -> integer := 1 + 2\n"
                        + "abstract V3 -> integer := 1, 1\nabstract V4 -> integer := s.dref\n"
                        + "abstract V5 -> integer := t.dref\nabstract V6 -> integer := (s.dref, 3) ** 1\n"
                        + "abstract V7 -> integer := if 1 = 1 then 2 else 3\nabstract V8 -> integer := -(s.dref)\n"
                        + "abstract V9 -> integer := #t\nabstract V10 -> integer * := 1, 2\n"
                        + "abstract V11 -> integer := 1 + 2 * 3, 4\nabstract V12 -> integer := u.w.dref\n" + decls));
        // the constraints are not used
        assertEquals(List.of("3:28: " + notScalar("Val", "pool.dref")),
                messages("pool ->> integer *\n[ #pool <= 1 ]\nabstract Val -> integer := pool.dref\nv : Val\n"
                        + "[ G some v ]\n"));
    }

    /**
     * {@code --check-duplicates}, also as {@code -c}.
     */
    @Test
    public void testDuplicateNames() {
        assertEquals(List.of("--check-duplicates: Duplicate clafer names: a, b, x"),
                messages("//# OPTIONS --check-duplicates\na\n  b\nenum E = x | b\nc\n  x\n  a\n"));
        assertEquals(List.of("--check-duplicates: Duplicate clafer names: b"), messages("//# OPTIONS -c\na\n  b\nb\n"));
        assertEquals(List.of(), messages("a\n  b\nb\n"));
    }

    /**
     * The names of an explicit model are the UIDs.
     */
    @Test
    public void testNonUidNames() {
        assertEquals(List.of("3:3: With -f (--skip-resolver), the names of the clafers are their UIDs, so they must"
                + " be unique and without apostrophes, but b is not"),
                messages("//# OPTIONS --explicit\n0..* a 1..1 {\n  0..* b 1..1 {}\n}\n0..* c 1..1 {\n"
                        + "  0..* b 1..1 {}\n}\n"));
    }

    /**
     * A model that declares itself the core has no rich construct.
     */
    @Test
    public void testRichInCore() {
        assertEquals(List.of("2:1: The core has no enumeration: the model declares itself the core (--core)"),
                messages("//# OPTIONS --explicit --core\nenum Color = red | green\n0..* c -> Color 1..1 {}\n"));
        assertEquals(List.of("3:4: The core has no transition: the model declares itself the core (--core)"),
                messages("//# OPTIONS --explicit --core --tl=3\n0..* a 0..1 {}\n[((#a) >= 1) --> ((#a) = 0)]\n"));
    }

    /**
     * {@code --check-afm}: no concrete clafer has more than one instance.
     */
    @Test
    public void testAttributedFeatureModel() {
        assertEquals(List.of("The model is not an attributed feature model.\n"
                + "The following places contain cardinality larger than 1:\nLine 5 column 3\nLine 7 column 1\n"),
                messages("//# OPTIONS --check-afm\na 0..0\nb ?\nc\n  e 0..*\nabstract Fa *\ng : Fa 2\n"
                        + "enum E = p | q\n"));
    }

    /**
     * An explicit model has no scope analysis.
     */
    @Test
    public void testExplicitScopes() {
        ToIr.Result result = ToIr.toIr("//# OPTIONS --explicit\nabstract 0..* Fe 0..* {}\n0..* Gr : Fe 3..3 {\n"
                + "  0..* h 2..2 {}\n}\n[(#Gr) = 3]\n");
        assertEquals(List.of(), result.messages());
        assertEquals(java.util.Map.of("Fe", 1L, "Gr", 1L, "h", 1L), result.scopes().orElseThrow().clafers());
    }
}
