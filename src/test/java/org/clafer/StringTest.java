package org.clafer;

import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstModel;
import static org.clafer.ast.Asts.*;
import org.clafer.compiler.ClaferCompiler;
import org.clafer.compiler.ClaferSolver;
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
}
