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
 * Step 9b: every rich and core explicit regression of a Clafer checkout next
 * to chocosolver, or at {@code -Dclafer.dir}, read as a model in the full
 * syntax ({@link ClaferParser}, {@link Symbols}, {@link ToIr}), prints as
 * itself, without its provenance comments.
 */
@RunWith(Parameterized.class)
public class ToIrConformanceTest {

    @Parameter
    public File file;

    @Parameters(name = "{0}")
    public static List<File[]> files() {
        return ExplicitConformanceTest.files();
    }

    @Test
    public void testFixpoint() throws IOException {
        String source = Files.readString(file.toPath(), StandardCharsets.UTF_8);
        assumeFalse("Imports are a later step", source.contains("--import"));
        ToIr.Result result = ToIr.toIr(source);
        assertEquals(List.of(), result.diagnostics().stream().map(Diagnostic::toString).toList());
        assertEquals(ExplicitConformanceTest.withoutOrigins(source), ExplicitPrinter.print(result.model(), false));
    }
}
