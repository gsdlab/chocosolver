package org.clafer.frontend;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import static org.junit.Assert.assertEquals;
import static org.junit.Assume.assumeFalse;
import static org.junit.Assume.assumeTrue;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameter;
import org.junit.runners.Parameterized.Parameters;

/**
 * Step 9: the symbols of a model are the clafers of its rich explicit model.
 * For each positive model {@code P.cfr} of a Clafer checkout next to
 * chocosolver, or at {@code -Dclafer.dir}, the outline of its symbols
 * ({@link Symbols#outline(Symbols.Result)}: the UIDs, the supers, the types of
 * the references, the cards and group cards, the modifiers, the clocks of the
 * initializers, and the order of the subclafers) is that of
 * {@code test/regression/P.des.cfr.reg}. Each rich and core explicit
 * regression, read as a model, has the outline of its explicit IR too.
 */
@RunWith(Parameterized.class)
public class SymbolsConformanceTest {

    @Parameter(0)
    public String name;

    @Parameter(1)
    public File model;

    @Parameter(2)
    public File explicit;

    static File clafer() {
        return ClaferParserConformanceTest.clafer();
    }

    @Parameters(name = "{0}")
    public static List<Object[]> models() {
        File test = new File(clafer(), "test");
        List<Object[]> parameters = new ArrayList<>();
        File[] positive = new File(test, "positive").listFiles(
                (d, n) -> n.endsWith(".cfr") && !n.endsWith(".des.cfr") && !n.endsWith(".core.cfr"));
        if (positive != null) {
            Arrays.sort(positive);
            for (File f : positive) {
                String base = f.getName().substring(0, f.getName().length() - ".cfr".length());
                File reg = new File(test, "regression/" + base + ".des.cfr.reg");
                if (reg.isFile()) {
                    parameters.add(new Object[]{"positive/" + f.getName(), f, reg});
                }
            }
        }
        File[] regressions = new File(test, "regression").listFiles(
                (d, n) -> n.endsWith(".des.cfr.reg") || n.endsWith(".core.cfr.reg"));
        if (regressions != null) {
            Arrays.sort(regressions);
            for (File f : regressions) {
                parameters.add(new Object[]{"regression/" + f.getName(), f, f});
            }
        }
        return parameters;
    }

    @Test
    public void testOutline() throws IOException {
        String source = Files.readString(model.toPath(), StandardCharsets.UTF_8);
        assumeFalse("Imports are a later step", source.contains("--import"));
        ClaferParser.Result parsed = ClaferParser.parse(source);
        assertEquals(List.of(), parsed.diagnostics().stream().map(Diagnostic::toString).collect(Collectors.toList()));
        Symbols.Result symbols = Symbols.analyze(parsed.model());
        assertEquals(List.of(), symbols.diagnostics().stream().map(Diagnostic::toString).collect(Collectors.toList()));

        ExplicitParser.Result reference = ExplicitParser.parse(Files.readString(explicit.toPath(), StandardCharsets.UTF_8));
        assertEquals(List.of(), reference.diagnostics());
        assertEquals(Symbols.outline(reference.model()), Symbols.outline(symbols));
    }

    @Test
    public void testSomeModels() {
        assumeTrue(new File(clafer(), "test/regression").isDirectory());
        assertEquals(true, models().size() > 600);
    }
}
