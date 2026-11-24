package org.clafer.frontend;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.List;
import static org.junit.Assert.assertEquals;
import static org.junit.Assume.assumeFalse;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameter;
import org.junit.runners.Parameterized.Parameters;

/**
 * Step 9b on every explicit regression rewritten in the full syntax: each
 * rich and core explicit regression, printed by {@link FullSyntaxPrinter}
 * without braces and without the cards and group cards that the symbols
 * infer, gives the regression back through {@link ToIr}.
 * <p>
 * With {@code -Dfull.dir=DIR}, the rewritten models are also written there,
 * as {@code DIR/NAME.cfr}, for the compiler to check:
 * {@code clafer -m clafer DIR/NAME.cfr} gives the regression too.
 */
@RunWith(Parameterized.class)
public class FullSyntaxConformanceTest {

    @Parameter
    public File file;

    @Parameters(name = "{0}")
    public static List<File[]> files() {
        return ExplicitConformanceTest.files();
    }

    @Test
    public void testFullSyntax() throws IOException {
        String source = Files.readString(file.toPath(), StandardCharsets.UTF_8);
        assumeFalse("Imports are a later step", source.contains("--import"));
        ExplicitParser.Result explicit = ExplicitParser.parse(source);
        assertEquals(List.of(), explicit.diagnostics());
        String full = FullSyntaxPrinter.print(explicit.model());
        String dir = System.getProperty("full.dir");
        if (dir != null) {
            String name = file.getName().replaceFirst("\\.cfr\\.reg$", "").replace('.', '_') + ".cfr";
            Files.writeString(new File(dir, name).toPath(), full, StandardCharsets.UTF_8);
        }
        ToIr.Result result = ToIr.toIr(full);
        assertEquals(full, List.of(), result.diagnostics().stream().map(Diagnostic::toString).toList());
        assertEquals(full, ExplicitConformanceTest.withoutOrigins(source), ExplicitPrinter.print(result.model(), false));
    }
}
