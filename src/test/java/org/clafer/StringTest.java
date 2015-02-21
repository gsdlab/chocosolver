package org.clafer;

import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstModel;
import static org.clafer.ast.Asts.*;
import org.clafer.ast.JoinSetWithStringException;
import org.clafer.compiler.ClaferCompiler;
import org.clafer.compiler.ClaferSolver;
import org.clafer.instance.InstanceClafer;
import org.clafer.instance.InstanceModel;
import org.clafer.scope.Scope;
import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;
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
        assertEquals(821, solver.allInstances().length);
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
        a.addConstraint(equal(joinRef($this()), joinRef(global(b))));
        model.addConstraint(equal(joinRef(global(b)), constant("abc")));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(1)
                .stringLength(5).charLow('a').charHigh('c'));
        assertEquals(2, solver.allInstances().length);
    }

    /**
     * <pre>
     * abstract A -> string
     * B : A ?
     *     [ this.ref = "b"
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
     */
    @Test(timeout = 60000, expected = JoinSetWithStringException.class)
    public void testJoinSetWithString() {
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A").refToUnique(StringType).withCard(0, 2);
        model.addConstraint(equal(joinRef(global(a)), constant("abc")));

        ClaferCompiler.compile(model, Scope.setScope(a, 2)
                .stringLength(5).charLow('a').charHigh('c'));
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
                length(joinRef(global(a))),
                add(length(joinRef(global(b))), constant(1))
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
        c.addConstraint(equal(joinRef($this()), concat(joinRef(global(a)), joinRef(global(b)))));

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
        model.addConstraint(prefix(joinRef(global(a)), joinRef(global(b))));

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
        model.addConstraint(suffix(joinRef(global(a)), joinRef(global(b))));

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
