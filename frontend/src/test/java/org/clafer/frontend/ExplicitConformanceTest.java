package org.clafer.frontend;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assume.assumeTrue;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameter;
import org.junit.runners.Parameterized.Parameters;

/**
 * The conformance of the explicit IR with the Clafer compiler, on the rich and
 * the core explicit models of Clafer's positive tests
 * ({@code test/regression/*.des.cfr.reg} and {@code *.core.cfr.reg}) in a
 * Clafer checkout next to chocosolver, or at {@code -Dclafer.dir}:
 * <ul>
 * <li>check 3: {@code print(parse(m))} is {@code m}, with and without the
 * provenance comments;</li>
 * <li>a core model has no rich node, and a rich model is not declared the
 * core (the first part of check 6).</li>
 * </ul>
 */
@RunWith(Parameterized.class)
public class ExplicitConformanceTest {

    @Parameter
    public File file;

    static File regressions() {
        return new File(System.getProperty("clafer.dir", "../../clafer"), "test/regression");
    }

    @Parameters(name = "{0}")
    public static List<File[]> files() {
        File[] files = regressions().listFiles((d, n) -> n.endsWith(".des.cfr.reg") || n.endsWith(".core.cfr.reg"));
        List<File[]> parameters = new ArrayList<>();
        if (files != null) {
            Arrays.sort(files);
            for (File f : files) {
                parameters.add(new File[]{f});
            }
        }
        return parameters;
    }

    /**
     * The model without its provenance comments, as clafer's
     * {@code withoutOrigins} removes them: a comment at the end of a line
     * that ends with a position.
     */
    static String withoutOrigins(String model) {
        return Arrays.stream(model.split("\n", -1))
                .map(l -> l.replaceFirst(" // [^\"]*[0-9]+:[0-9]+$", ""))
                .collect(Collectors.joining("\n"));
    }

    @Test
    public void testPrintParse() throws IOException {
        String source = Files.readString(file.toPath());
        ExplicitParser.Result parsed = ExplicitParser.parse(source);
        assertEquals(List.of(), parsed.diagnostics());
        assertEquals(source, ExplicitPrinter.print(parsed.model()));
        assertEquals(withoutOrigins(source), ExplicitPrinter.print(parsed.model(), false));

        String plain = withoutOrigins(source);
        ExplicitParser.Result parsedPlain = ExplicitParser.parse(plain);
        assertEquals(List.of(), parsedPlain.diagnostics());
        assertEquals(plain, ExplicitPrinter.print(parsedPlain.model()));
    }

    @Test
    public void testCoreHasNoRichNode() throws IOException {
        ExplicitParser.Result parsed = ExplicitParser.parse(Files.readString(file.toPath()));
        boolean core = file.getName().endsWith(".core.cfr.reg");
        assertEquals(core, parsed.model().options().core());
        if (core) {
            assertEquals(List.of(), Core.richNodes(parsed.model()).stream().map(Core::describe).toList());
        }
    }

    @Test
    public void testSomeModels() {
        assumeTrue(regressions().isDirectory());
        assertTrue(files().size() > 400);
    }
}
