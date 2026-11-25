package org.clafer.frontend;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import static org.junit.Assert.assertEquals;
import static org.junit.Assume.assumeFalse;
import static org.junit.Assume.assumeTrue;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameter;
import org.junit.runners.Parameterized.Parameters;

/**
 * Check 4: the frontend on each positive model {@code P.cfr} of a Clafer
 * checkout next to chocosolver, or at {@code -Dclafer.dir}, prints the rich
 * explicit model that the compiler gives,
 * {@code test/regression/P.des.cfr.reg}, without its provenance comments:
 * parsing, symbols, and the names and types of every expression
 * ({@link Resolver}).
 */
@RunWith(Parameterized.class)
public class ResolverConformanceTest {

    @Parameter(0)
    public String name;

    @Parameter(1)
    public File model;

    @Parameter(2)
    public File explicit;

    @Parameters(name = "{0}")
    public static List<Object[]> models() {
        File test = new File(ClaferParserConformanceTest.clafer(), "test");
        List<Object[]> parameters = new ArrayList<>();
        File[] positive = new File(test, "positive").listFiles(
                (d, n) -> n.endsWith(".cfr") && !n.endsWith(".des.cfr") && !n.endsWith(".core.cfr"));
        if (positive != null) {
            Arrays.sort(positive);
            for (File f : positive) {
                String base = f.getName().substring(0, f.getName().length() - ".cfr".length());
                File reg = new File(test, "regression/" + base + ".des.cfr.reg");
                if (reg.isFile()) {
                    parameters.add(new Object[]{f.getName(), f, reg});
                }
            }
        }
        return parameters;
    }

    @Test
    public void testExplicitModel() throws IOException {
        String source = Files.readString(model.toPath(), StandardCharsets.UTF_8);
        assumeFalse("Imports are step 11", source.contains("--import"));
        ToIr.Result result = ToIr.toIr(source);
        assertEquals(List.of(), result.diagnostics().stream().map(Diagnostic::toString).toList());
        String expected = ExplicitConformanceTest.withoutOrigins(Files.readString(explicit.toPath(), StandardCharsets.UTF_8));
        assertEquals(expected, ExplicitPrinter.print(result.model(), false));
    }

    @Test
    public void testSomeModels() {
        assumeTrue(new File(ClaferParserConformanceTest.clafer(), "test/regression").isDirectory());
        assertEquals(true, models().size() > 200);
    }
}
