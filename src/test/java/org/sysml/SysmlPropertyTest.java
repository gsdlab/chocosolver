package org.sysml;

import org.clafer.ast.AstClafer;
import org.clafer.ast.AstModel;
import org.clafer.cli.Utils;
import org.clafer.compiler.ClaferCompiler;
import org.clafer.compiler.ClaferOption;
import org.clafer.compiler.ClaferSearch;
import org.clafer.instance.InstanceClafer;
import org.clafer.instance.InstanceModel;
import org.clafer.javascript.Javascript;
import org.clafer.javascript.JavascriptFile;
import org.clafer.objective.Objective;
import org.clafer.scope.Scope;
import org.junit.Test;
import org.junit.Ignore;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.sysml.ast.SysmlPropertyDef;
import org.sysml.compiler.AstSysmlCompiler;
import org.sysml.compiler.InstanceSysmlCompiler;
import org.sysml.compiler.SysmlCompilerUtils;
import test.OptimizationTest;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static org.junit.Assert.assertTrue;

@Ignore
@RunWith(Parameterized.class)
public class SysmlPropertyTest {
    @Parameterized.Parameter
    public File testFile;

    @Parameterized.Parameters(name = "{0}")
    public static List<File[]> testFiles() throws URISyntaxException {
        File dir = new File(OptimizationTest.class.getResource("/sysml-samples/assert-positive").toURI());
        assertTrue(dir.isDirectory());
        List<File[]> files = new ArrayList<>();
        for (File file : dir.listFiles()) {
            if (file.getAbsolutePath().endsWith(".cfr")) {
                files.add(new File[]{file});
            }
        }
        return files;
    }

    File getInputFile() throws IOException, InterruptedException {
        Process compilerProcess = Runtime.getRuntime().exec("clafer -k -m choco " + testFile);
        compilerProcess.waitFor();

        // replace the extension to .js
        String testFileName = testFile.getAbsolutePath();
        int extPos = testFileName.lastIndexOf(".");
        if(extPos != -1) {
            testFileName = testFileName.substring(0, extPos) + ".js";
        }

        // change the inputFile to the resulting .js file
        return new File(testFileName);
    }

    SysmlPropertyDef[] getSysmlPropertyDefs(File inputFile) throws IOException {
        // compile the example
        JavascriptFile jsFile = Javascript.readModel(inputFile);
        AstModel top = jsFile.getModel();
        AstSysmlCompiler compiler = new AstSysmlCompiler();
        SysmlPropertyDef[] models = compiler.compile(top, top);
        return models;
    }


    /*
     * Test that the compiler creates SysML Properties
     */
    @Test
    public void testSysmlProperty() throws IOException, URISyntaxException, InterruptedException {
        File inputFile = getInputFile();
        JavascriptFile jsFile = Javascript.readModel(inputFile);

        Objective[] objectives = jsFile.getObjectives();

        // handle scopes
        /* setting the default int range */
        Scope scope = jsFile.getScope();
        int scopeHighDef = 127;
        int scopeLowDef = -(scopeHighDef + 1);
        scope = scope.toBuilder().intLow(scopeLowDef).intHigh(scopeHighDef).toScope();

        // handle search strategy
        ClaferOption compilerOption = jsFile.getOption();

        // pick the right solver
        ClaferSearch solver = objectives.length == 0
                ? ClaferCompiler.compile(jsFile.getModel(), scope,             compilerOption)
                : ClaferCompiler.compile(jsFile.getModel(), scope, objectives, compilerOption);
        assertTrue(solver.find());
        InstanceModel instance = solver.instance();

        // the instance contains something
        assertTrue(instance.getTopClafers().length > 0);

        // get its supers
        for (InstanceClafer clafer: instance.getTopClafers()) {
            String[] superClafers = SysmlCompilerUtils.getSuperClafers(clafer.getType());
            List hierarchy = Arrays.asList(superClafers);
            assertTrue(hierarchy.contains(new InstanceSysmlCompiler().baseSysmlClafer));
        }

        System.out.println(testFile);
    }
}
