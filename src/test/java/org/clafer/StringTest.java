package org.clafer;

import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstBoolExpr;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstLocal;
import org.clafer.ast.AstModel;
import static org.clafer.ast.Asts.$this;
import static org.clafer.ast.Asts.Mandatory;
import static org.clafer.ast.Asts.Optional;
import static org.clafer.ast.Asts.StringType;
import static org.clafer.ast.Asts.add;
import static org.clafer.ast.Asts.all;
import static org.clafer.ast.Asts.and;
import static org.clafer.ast.Asts.concat;
import static org.clafer.ast.Asts.constant;
import static org.clafer.ast.Asts.decl;
import static org.clafer.ast.Asts.equal;
import static org.clafer.ast.Asts.global;
import static org.clafer.ast.Asts.ifOnlyIf;
import static org.clafer.ast.Asts.in;
import static org.clafer.ast.Asts.joinRef;
import static org.clafer.ast.Asts.length;
import static org.clafer.ast.Asts.local;
import static org.clafer.ast.Asts.newModel;
import static org.clafer.ast.Asts.none;
import static org.clafer.ast.Asts.not;
import static org.clafer.ast.Asts.notEqual;
import static org.clafer.ast.Asts.notIn;
import static org.clafer.ast.Asts.some;
import static org.clafer.ast.Asts.prefix;
import static org.clafer.ast.Asts.suffix;
import org.clafer.ast.JoinSetWithStringException;
import org.clafer.compiler.ClaferCompiler;
import org.clafer.compiler.ClaferSolver;
import org.clafer.instance.InstanceClafer;
import org.clafer.instance.InstanceModel;
import org.clafer.scope.Scope;
import static org.hamcrest.CoreMatchers.endsWith;
import static org.hamcrest.CoreMatchers.startsWith;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import org.junit.Ignore;
import org.junit.Test;

/**
 *
 * @author jimmy
 */
public class StringTest {

    /**
     * <pre>
     * A ->> string 0..2
     * </pre>
     */
    @Test(timeout = 60000)
    public void testStrings() {
        /*
         * import Control.Monad
         *
         * isSorted [] = True
         * isSorted [x] = True
         * isSorted (x:y:zs) = x <= y && isSorted (y:zs)
         *
         * isUnique [] = True
         * isUnique (x:xs) = x `notElem` xs && isUnique xs
         *
         * strings = [0..3] >>= flip replicateM ['a', 'b', 'c']
         *
         * positive = filter isSorted $ [0..2] >>= flip replicateM strings
         */
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A").refTo(StringType).withCard(0, 2);

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.setScope(a, 2)
                .stringLength(3).charLow('a').charHigh('c'));
        assertEquals(861, solver.allInstances().length);
    }

    /**
     * <pre>
     * A -> string 0..2
     * </pre>
     */
    @Test(timeout = 60000)
    public void testUniqueStrings() {
        /*
         * import Control.Monad
         *
         * isSorted [] = True
         * isSorted [x] = True
         * isSorted (x:y:zs) = x <= y && isSorted (y:zs)
         *
         * isUnique [] = True
         * isUnique (x:xs) = x `notElem` xs && isUnique xs
         *
         * strings = [0..3] >>= flip replicateM ['a', 'b', 'c']
         *
         * positive = filter isUnique $ filter isSorted $ [0..2] >>= flip replicateM strings
         */
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A").refToUnique(StringType).withCard(0, 2);

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.setScope(a, 2)
                .stringLength(3).charLow('a').charHigh('c'));
        int k = 0;
        while (solver.find()) {
            InstanceClafer[] ai = solver.instance().getTopClafers(a);
            for (int i = 0; i < ai.length - 1; i++) {
                String ref1 = (String) ai[i].getRef();
                String ref2 = (String) ai[i + 1].getRef();
                assertTrue(ref1.compareTo(ref2) > 0);
            }
        }
        assertEquals(821, solver.instanceCount());
    }

    /**
     * <pre>
     * A -> string
     *     [ this.ref = "abc" ]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testJoinString() {
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A").refToUnique(StringType).withCard(Mandatory);
        a.addConstraint(equal(joinRef($this()), constant("abc")));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.setScope(a, 1)
                .stringLength(5).charLow('a').charHigh('c'));
        assertTrue(solver.find());
        InstanceModel instance = solver.instance();
        InstanceClafer[] as = instance.getTopClafers(a);
        assertEquals(1, as.length);
        assertEquals("abc", as[0].getRef());
        assertFalse(solver.find());
    }

    /**
     * <pre>
     * A -> string ?
     *     [ this.ref = B.ref ]
     * B -> string
     * [ B.ref = "abc" ]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testJoinStringEqualJoinString() {
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A").refToUnique(StringType).withCard(Optional);
        AstConcreteClafer b = model.addChild("B").refToUnique(StringType).withCard(Mandatory);
        a.addConstraint(equal(joinRef($this()), joinRef(b)));
        model.addConstraint(equal(joinRef(b), constant("abc")));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(1)
                .stringLength(5).charLow('a').charHigh('c'));
        assertEquals(2, solver.allInstances().length);
    }

    /**
     * <pre>
     * abstract A -> string
     * B : A ?
     *     [ this.ref = "b" ]
     * C : A ?
     *     [ this.ref = "c" ]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testJoinSuperStringEqualJoinString() {
        AstModel model = newModel();

        AstAbstractClafer a = model.addAbstract("A").refToUnique(StringType);
        AstConcreteClafer b = model.addChild("B").extending(a).withCard(Optional);
        AstConcreteClafer c = model.addChild("C").extending(a).withCard(Optional);
        b.addConstraint(equal(joinRef($this()), constant("b")));
        c.addConstraint(equal(joinRef($this()), constant("c")));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(1)
                .stringLength(5).charLow('a').charHigh('c'));
        while (solver.find()) {
            InstanceModel instance = solver.instance();
            for (InstanceClafer B : instance.getTopClafers(b)) {
                assertEquals("b", B.getRef());
            }
            for (InstanceClafer C : instance.getTopClafers(c)) {
                assertEquals("c", C.getRef());
            }
        }
        assertEquals(4, solver.instanceCount());
    }

    /**
     * <pre>
     * A -> string 0..2
     * [ A.ref = "abc" ]
     * </pre>
     *
     * The set of strings of the A's is {"abc"}: one A, since the references
     * of a set are distinct
     * (https://github.com/gsdlab/chocosolver/issues/49).
     */
    @Test(timeout = 60000)
    public void testJoinSetWithString() {
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A").refToUnique(StringType).withCard(0, 2);
        model.addConstraint(equal(joinRef(a), constant("abc")));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.setScope(a, 2)
                .stringLength(5).charLow('a').charHigh('c'));
        assertTrue(solver.find());
        InstanceClafer[] as = solver.instance().getTopClafers(a);
        assertEquals(1, as.length);
        assertEquals("abc", as[0].getRef());
        assertFalse(solver.find());
    }

    /**
     * <pre>
     * A -> string 0..2
     * [ length(A.ref) = 3 ]
     * </pre>
     *
     * Only comparisons support sets of strings.
     */
    @Test(timeout = 60000, expected = JoinSetWithStringException.class)
    public void testLengthOfSetOfStrings() {
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A").refToUnique(StringType).withCard(0, 2);
        model.addConstraint(equal(length(joinRef(a)), constant(3)));

        ClaferCompiler.compile(model, Scope.setScope(a, 2)
                .stringLength(5).charLow('a').charHigh('c'));
    }

    /*
     * A -> string 0..2 and B -> string 0..2, with strings of at most one
     * character from 'a' to 'c': no instance where the comparison and its
     * quantified form differ, and some instances where they hold.
     */
    private static void assertSameAsQuantified(
            java.util.function.BiFunction<AstConcreteClafer, AstConcreteClafer, AstBoolExpr[]> constraints) {
        for (int i = 0; i < 2; i++) {
            AstModel model = newModel();
            AstConcreteClafer a = model.addChild("A").refToUnique(StringType).withCard(0, 2);
            AstConcreteClafer b = model.addChild("B").refToUnique(StringType).withCard(0, 2);
            AstBoolExpr[] comparisonAndQuantified = constraints.apply(a, b);
            model.addConstraint(i == 0
                    ? not(ifOnlyIf(comparisonAndQuantified))
                    : comparisonAndQuantified[0]);
            ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(2)
                    .stringLength(1).charLow('a').charHigh('c'));
            if (i == 0) {
                assertFalse(solver.find());
            } else {
                assertTrue(solver.find());
            }
        }
    }

    /**
     * <pre>
     * names -> string 2
     * [ "ada" in names ]
     * </pre>
     *
     * As {@code some n : names | n = "ada"}
     * (https://github.com/gsdlab/chocosolver/issues/49).
     */
    @Test(timeout = 60000)
    public void testStringInSetOfStrings() {
        AstModel model = newModel();

        AstConcreteClafer names = model.addChild("names").refToUnique(StringType).withCard(2, 2);
        model.addConstraint(in(constant("ada"), joinRef(names)));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(2)
                .stringLength(3).charLow('a').charHigh('d'));
        int count = 0;
        while (solver.find() && count < 100) {
            InstanceClafer[] ns = solver.instance().getTopClafers(names);
            assertTrue(ns[0].getRef().equals("ada") || ns[1].getRef().equals("ada"));
            count++;
        }
        assertTrue(count > 0);

        assertSameAsQuantified((a, b) -> {
            AstLocal x = local("x");
            return new AstBoolExpr[]{
                in(constant("a"), joinRef(a)),
                some(decl(x, global(a)), equal(joinRef(x), constant("a")))};
        });
        assertSameAsQuantified((a, b) -> {
            AstLocal x = local("x");
            return new AstBoolExpr[]{
                notIn(constant("a"), joinRef(a)),
                not(some(decl(x, global(a)), equal(joinRef(x), constant("a"))))};
        });
    }

    /**
     * <pre>
     * A -> string 0..2
     * B -> string 0..2
     * [ A.ref in B.ref ]
     * [ A.ref = B.ref ]
     * [ A.ref != B.ref ]
     * [ A.ref = "a" ]
     * </pre>
     *
     * As their quantified forms, e.g.
     * {@code all x : A | some y : B | x.ref = y.ref} for {@code in}
     * (https://github.com/gsdlab/chocosolver/issues/49).
     */
    @Test(timeout = 60000)
    public void testCompareSetsOfStrings() {
        assertSameAsQuantified((a, b) -> new AstBoolExpr[]{
            in(joinRef(global(a)), joinRef(global(b))),
            subsetOfStrings(a, b)});
        assertSameAsQuantified((a, b) -> new AstBoolExpr[]{
            equal(joinRef(global(a)), joinRef(global(b))),
            and(subsetOfStrings(a, b), subsetOfStrings(b, a))});
        assertSameAsQuantified((a, b) -> new AstBoolExpr[]{
            notEqual(joinRef(global(a)), joinRef(global(b))),
            not(and(subsetOfStrings(a, b), subsetOfStrings(b, a)))});
        assertSameAsQuantified((a, b) -> {
            AstLocal x = local("x");
            return new AstBoolExpr[]{
                equal(joinRef(global(a)), constant("a")),
                and(some(global(a)), all(decl(x, global(a)), equal(joinRef(x), constant("a"))))};
        });
    }

    // all x : A | some y : B | x.ref = y.ref
    private static AstBoolExpr subsetOfStrings(AstConcreteClafer a, AstConcreteClafer b) {
        AstLocal x = local("x");
        AstLocal y = local("y");
        return all(decl(x, global(a)), some(decl(y, global(b)), equal(joinRef(x), joinRef(y))));
    }

    /**
     * <pre>
     * A -> string ?
     * B -> string
     * [ A.ref = B.ref ]
     * [ no A ]
     * </pre>
     */
    @Ignore("Should find no instances if we allow for empty string sets. B.ref cannot be the set of empty string.")
    @Test(timeout = 60000)
    public void testJoinEmptySetWithString() {
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A").refToUnique(StringType).withCard(Optional);
        AstConcreteClafer b = model.addChild("B").refToUnique(StringType).withCard(Mandatory);
        model.addConstraint(equal(joinRef(a), joinRef(b)));
        model.addConstraint(none(a));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(1));
        assertFalse(solver.find());
    }

    /**
     * <pre>
     * A -> string
     * B -> string
     * [ length(A.ref) = length(B.ref) + 1
     * </pre>
     */
    @Test(timeout = 60000)
    public void testLength() {
        /*
         * import Control.Monad
         *
         * strings = [0..3] >>= flip replicateM ['a', 'b', 'c']
         * positive = do
         *     a <- strings
         *     b <- strings
         *     guard $ length a == length b + 1
         *     return (a, b) 
         */
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A").refToUnique(StringType).withCard(Mandatory);
        AstConcreteClafer b = model.addChild("B").refToUnique(StringType).withCard(Mandatory);
        model.addConstraint(equal(
                length(joinRef(a)),
                add(length(joinRef(b)), constant(1))
        ));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(1)
                .stringLength(3).charLow('a').charHigh('c'));
        int count = 0;
        while (solver.find()) {
            InstanceModel instance = solver.instance();
            for (InstanceClafer A : instance.getTopClafers(a)) {
                for (InstanceClafer B : instance.getTopClafers(b)) {
                    assertEquals(A.getRef().toString().length(),
                            B.getRef().toString().length() + 1);
                }
            }
            count++;
        }
        assertEquals(273, count);
    }

    /**
     * <pre>
     * A -> string
     * B -> string
     *     [ this.ref = "abc" ]
     * C -> string
     *     [ this.ref = A.ref ++ B.ref ]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testConcat() {
        /*
         * import Control.Monad
         *
         * strings = [0..5] >>= flip replicateM ['a', 'b', 'c']
         * positive = do
         *     a <- strings
         *     let b = "abc"
         *     c <- strings
         *     guard $ a ++ b == c
         *     return (a, b, c)
         */
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A").refToUnique(StringType).withCard(Mandatory);
        AstConcreteClafer b = model.addChild("B").refToUnique(StringType).withCard(Mandatory);
        AstConcreteClafer c = model.addChild("C").refToUnique(StringType).withCard(Mandatory);
        b.addConstraint(equal(joinRef($this()), constant("abc")));
        c.addConstraint(equal(joinRef($this()), concat(joinRef(a), joinRef(b))));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(1)
                .stringLength(5).charLow('a').charHigh('c'));
        int count = 0;
        while (solver.find()) {
            InstanceModel instance = solver.instance();
            for (InstanceClafer A : instance.getTopClafers(a)) {
                for (InstanceClafer B : instance.getTopClafers(b)) {
                    for (InstanceClafer C : instance.getTopClafers(c)) {
                        assertEquals(C.getRef(), A.getRef().toString() + B.getRef());
                    }
                }
            }
            count++;
        }
        assertEquals(13, count);
    }

    /**
     * <pre>
     * A -> string
     * B -> string
     * [ A.ref prefix B.ref ]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testPrefix() {
        /*
         * import Control.Monad
         * import Data.List
         *
         * strings = [0..3] >>= flip replicateM ['a', 'b', 'c']
         * positive = do
         *     a <- strings
         *     b <- strings
         *     guard $ a `isPrefixOf` b
         *     return (a, b)
         */
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A").refToUnique(StringType).withCard(Mandatory);
        AstConcreteClafer b = model.addChild("B").refToUnique(StringType).withCard(Mandatory);
        model.addConstraint(prefix(joinRef(a), joinRef(b)));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(1)
                .stringLength(3).charLow('a').charHigh('c'));
        int count = 0;
        while (solver.find()) {
            InstanceModel instance = solver.instance();
            for (InstanceClafer A : instance.getTopClafers(a)) {
                for (InstanceClafer B : instance.getTopClafers(b)) {
                    assertThat((String) B.getRef(), startsWith((String) A.getRef()));
                }
            }
            count++;
        }
        assertEquals(142, count);
    }

    /**
     * <pre>
     * A -> string
     * B -> string
     * [ A.ref suffix B.ref ]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testSuffix() {
        /*
         * import Control.Monad
         * import Data.List
         *
         * strings = [0..3] >>= flip replicateM ['a', 'b', 'c']
         * positive = do
         *     a <- strings
         *     b <- strings
         *     guard $ a `isPrefixOf` (reverse b)
         *     return (a, b)
         */
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A").refToUnique(StringType).withCard(Mandatory);
        AstConcreteClafer b = model.addChild("B").refToUnique(StringType).withCard(Mandatory);
        model.addConstraint(suffix(joinRef(a), joinRef(b)));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(1)
                .stringLength(3).charLow('a').charHigh('c'));
        int count = 0;
        while (solver.find()) {
            InstanceModel instance = solver.instance();
            for (InstanceClafer A : instance.getTopClafers(a)) {
                for (InstanceClafer B : instance.getTopClafers(b)) {
                    assertThat((String) B.getRef(), endsWith((String) A.getRef()));
                }
            }
            count++;
        }
        assertEquals(142, count);
    }

    /**
     * <pre>
     * A -> string ?
     * [ A.ref = "test" ]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testOptional() {
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A").refToUnique(StringType).withCard(Optional);
        model.addConstraint(equal(joinRef(a), constant("test")));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(1));
        int count = 0;
        while (solver.find()) {
            InstanceModel instance = solver.instance();
            for (InstanceClafer A : instance.getTopClafers(a)) {
                assertEquals("test", A.getRef());
            }
            count++;
        }
        assertEquals(1, count);
    }
}
