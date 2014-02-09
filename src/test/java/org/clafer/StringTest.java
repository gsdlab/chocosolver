package org.clafer;

import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstModel;
import static org.clafer.ast.Asts.*;
import org.clafer.ast.JoinSetWithStringException;
import org.clafer.compiler.ClaferCompiler;
import org.clafer.compiler.ClaferSolver;
import org.clafer.instance.InstanceClafer;
import org.clafer.instance.InstanceModel;
import org.clafer.scope.Scope;
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
        assertEquals("abc", as[0].getRef().getValue());
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
     * A -> string 0..1
     * [ A.ref = "abc" ]
     * </pre>
     */
    @Test(timeout = 60000, expected = JoinSetWithStringException.class)
    public void testJoinSetWithString() {
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A").refToUnique(StringType).withCard(0, 1);
        model.addConstraint(equal(joinRef(global(a)), constant("abc")));

        ClaferCompiler.compile(model, Scope.setScope(a, 1)
                .stringLength(5).charLow('a').charHigh('c'));
    }
}
