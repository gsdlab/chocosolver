package test;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import javax.script.ScriptException;
import org.clafer.ast.AstModel;
import org.clafer.collection.Triple;
import org.clafer.compiler.ClaferCompiler;
import org.clafer.compiler.ClaferOptimizer;
import org.clafer.compiler.ClaferSolver;
import org.clafer.javascript.Javascript;
import org.clafer.objective.Objective;
import org.clafer.scope.Scope;
import static org.junit.Assert.assertTrue;
import org.junit.Test;

/**
 *
 * @author jimmy
 */
public class OptimizationTest {


    @Test
    public void testOptimizations() throws IOException, ScriptException, URISyntaxException {
        File dir = new File(OptimizationTest.class.getResource("/optimization").toURI());
        assertTrue(dir.isDirectory());

        for (File test : dir.listFiles()) {
            Triple<AstModel, Scope, Objective[]> p = Javascript.readModel(test);
            ClaferOptimizer s = ClaferCompiler.compile(p.getFst(), p.getSnd(), p.getThd());

            assertTrue(test + " failed", s.find());
            while(s.find()) {
            }
        }
    }
}
