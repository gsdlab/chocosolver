package org.clafer.frontend;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import static org.junit.Assume.assumeTrue;
import org.junit.Test;

/**
 * For comparisons with the compiler on any set of models: with
 * {@code -Dresolver.corpus=DIR}, writes for each {@code DIR/NAME.cfr} the
 * frontend's rich explicit model, or its errors, to {@code DIR/NAME.java.out},
 * to compare with {@code clafer -m clafer DIR/NAME.cfr}. Without the property,
 * it does nothing.
 */
public class ResolverCorpusDump {

    @Test
    public void dump() throws IOException {
        String dir = System.getProperty("resolver.corpus");
        assumeTrue(dir != null);
        File[] models = new File(dir).listFiles((d, n) -> n.endsWith(".cfr") && !n.endsWith(".des.cfr")
                && !n.endsWith(".core.cfr"));
        for (File f : models) {
            String source = Files.readString(f.toPath(), StandardCharsets.UTF_8);
            String out;
            try {
                ToIr.Result result = ToIr.toIr(source);
                out = result.diagnostics().isEmpty() ? ExplicitPrinter.print(result.model(), false)
                        : "ERRORS\n" + String.join("\n", result.diagnostics().stream().map(Diagnostic::toString).toList())
                        + "\n";
            } catch (RuntimeException e) {
                out = "CRASH " + e + "\n";
            }
            String name = f.getName().substring(0, f.getName().length() - ".cfr".length());
            Files.writeString(new File(dir, name + ".java.out").toPath(), out, StandardCharsets.UTF_8);
        }
    }
}
