package org.clafer.frontend;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import static org.clafer.frontend.ExplicitConformanceTest.regressions;
import static org.clafer.frontend.ExplicitConformanceTest.withoutOrigins;
import static org.junit.Assert.assertEquals;
import static org.junit.Assume.assumeTrue;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameter;
import org.junit.runners.Parameterized.Parameters;

/**
 * The lowering against the Clafer compiler, on the explicit models of Clafer's
 * positive tests ({@code test/regression/*.des.cfr.reg} and
 * {@code *.core.cfr.reg}):
 * <ul>
 * <li>check 5: {@code toCore} of the rich model is the core model, without
 * the provenance comments;</li>
 * <li>check 6: {@code toCore} of the core model is the core model, and the
 * output of {@code toCore} has no rich node.</li>
 * </ul>
 */
@RunWith(Parameterized.class)
public class ToCoreTest {

    @Parameter
    public String name;

    @Parameters(name = "{0}")
    public static List<String[]> files() {
        File[] files = regressions().listFiles((d, n) -> n.endsWith(".des.cfr.reg"));
        List<String[]> parameters = new ArrayList<>();
        if (files != null) {
            Arrays.sort(files);
            for (File f : files) {
                parameters.add(new String[]{f.getName().replace(".des.cfr.reg", "")});
            }
        }
        return parameters;
    }

    private static Ir.Model parse(File file) throws IOException {
        ExplicitParser.Result parsed = ExplicitParser.parse(Files.readString(file.toPath()));
        assertEquals(List.of(), parsed.diagnostics());
        return parsed.model();
    }

    private File core() {
        return new File(regressions(), name + ".core.cfr.reg");
    }

    @Test
    public void testRichToCore() throws IOException {
        assumeTrue(core().isFile());
        Ir.Model lowered = ToCore.toCore(parse(new File(regressions(), name + ".des.cfr.reg")));
        assertEquals(List.of(), Core.richNodes(lowered).stream().map(Core::describe).toList());
        assertEquals(withoutOrigins(Files.readString(core().toPath())), ExplicitPrinter.print(lowered, false));
    }

    @Test
    public void testCoreIsAFixpoint() throws IOException {
        assumeTrue(core().isFile());
        String core = Files.readString(core().toPath());
        assertEquals(core, ExplicitPrinter.print(ToCore.toCore(parse(core()))));
    }
}
