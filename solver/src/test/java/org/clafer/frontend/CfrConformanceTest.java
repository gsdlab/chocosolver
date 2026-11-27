package org.clafer.frontend;

import java.io.File;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.clafer.cli.Frontend;
import org.clafer.javascript.Javascript;
import org.clafer.javascript.JavascriptFile;
import org.clafer.scope.Scope;
import org.clafer.scope.ScopeFile;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static org.junit.Assume.assumeTrue;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameter;
import org.junit.runners.Parameterized.Parameters;

/**
 * chocosolver's default path on {@code .cfr} files, the Java frontend
 * ({@link Frontend#readWithJava}), on Clafer's test models (in a Clafer
 * checkout next to chocosolver or at {@code -Dclafer.dir}):
 * <ul>
 * <li>each positive model with a Choco output ({@code test/regression/P.js.reg})
 * solves, with the scopes of its explicit model's scope file, as that output
 * does with its own scopes: the same first instances (optimal instances,
 * counterexamples) in the same order, or both the same error;</li>
 * <li>each other positive model is temporal or has {@code real} or
 * {@code double}, which the Java frontend rejects with a diagnostic, as the
 * compiler gives no Choco output for it;</li>
 * <li>each negative model is rejected.</li>
 * </ul>
 * The {@code [choco| |]} escapes are JavaScript, so chocosolver gives a model
 * with one to the Haskell compiler; it is read here without them.
 */
@RunWith(Parameterized.class)
public class CfrConformanceTest {

    @Parameter(0)
    public String name;

    @Parameter(1)
    public File model;

    @Parameters(name = "{0}")
    public static List<Object[]> models() {
        List<Object[]> parameters = new ArrayList<>();
        for (String suite : List.of("positive", "negative")) {
            File[] files = new File(ExplicitCoreTest.regressions().getParentFile(), suite).listFiles(
                    (d, n) -> n.endsWith(".cfr") && !n.endsWith(".des.cfr") && !n.endsWith(".core.cfr"));
            if (files != null) {
                Arrays.sort(files);
                for (File f : files) {
                    parameters.add(new Object[]{suite + "/" + f.getName(), f});
                }
            }
        }
        return parameters;
    }

    private File jsFile() {
        return new File(ExplicitCoreTest.regressions(), model.getName().replaceFirst("\\.cfr$", ".js.reg"));
    }

    @Test
    public void testSolvesAsTheChocoOutput() throws Exception {
        boolean negative = model.getParentFile().getName().equals("negative");
        JavascriptFile read;
        try {
            read = Frontend.readWithJava(model, List.of());
        } catch (Frontend.ModelException e) {
            if (negative) {
                return;
            }
            String source = Files.readString(model.toPath());
            boolean noChocoOutput = !jsFile().isFile();
            assertTrue(name + ": " + e.getMessage(), noChocoOutput && (e.getMessage().contains(
                    "Chocosolver does not solve temporal models") || source.matches("(?s).*\\b(real|double)\\b.*")));
            return;
        }
        if (negative) {
            fail(name + " is a negative model, which the Java frontend accepts");
        }
        assumeTrue(jsFile().isFile());
        JavascriptFile js = Javascript.readModel(jsFile());
        // the scope that chocosolver solves the Choco output with: without intRange, -128..127
        Scope jsScope = js.hasIntRange() ? js.getScope()
                : js.getScope().toBuilder().intLow(-128).intHigh(127).toScope();
        Scope scope = ScopeFile.parse(ScopeFile.print(js.getModel(), jsScope), "model.cfr-scope")
                .applyTo(jsScope, read.getModel());
        // the scopes of the explicit model are those of the Choco output
        assertEquals(ScopeFile.print(read.getModel(), scope), ScopeFile.print(read.getModel(), read.getScope()));
        List<String> expected = ExplicitCoreTest.solve(js.getModel(), jsScope, js.getObjectives(),
                js.getAssertions(), js.getOption());
        List<String> actual = ExplicitCoreTest.solve(read.getModel(), read.getScope(), read.getObjectives(),
                read.getAssertions(), read.getOption());
        if (ExplicitCoreTest.subs(js.getModel()).equals(ExplicitCoreTest.subs(read.getModel()))) {
            assertEquals(expected, actual);
        } else {
            // The order of the subs of an abstract clafer numbers its instances.
            assertEquals(name, expected.size(), actual.size());
        }
    }

    @Test
    public void testSomeModels() {
        assumeTrue(ExplicitCoreTest.regressions().isDirectory());
        assertTrue(models().size() > 250);
    }
}
