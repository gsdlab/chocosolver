package test;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;
import javax.script.ScriptException;
import org.clafer.compiler.ClaferCompiler;
import org.clafer.compiler.ClaferOptimizer;
import org.clafer.javascript.Javascript;
import org.clafer.javascript.JavascriptFile;
import static org.junit.Assert.assertTrue;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameter;
import org.junit.runners.Parameterized.Parameters;

/**
 *
 * @author jimmy
 */
@RunWith(Parameterized.class)
public class OptimizationTest {

    @Parameter
    public File testFile;

    @Parameters(name = "{0}")
    public static List<File[]> testFiles() throws URISyntaxException {
        File dir = new File(OptimizationTest.class.getResource("/optimization").toURI());
        assertTrue(dir.isDirectory());
        List<File[]> files = new ArrayList<>();
        for (File file : dir.listFiles()) {
            files.add(new File[]{file});
        }
        return files;
    }

    @Test
    public void testOptimizations() throws IOException, ScriptException {
        JavascriptFile p = Javascript.readModel(testFile);
        assert p.getAssertions().length == 0 : "Did not expect an assertion problem.";
        ClaferOptimizer s = ClaferCompiler.compile(p.getModel(), p.getScope(), p.getObjectives());

        assertTrue(s.find());
        while (s.find()) {
        }
    }
}
