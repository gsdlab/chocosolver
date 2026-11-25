package org.clafer.frontend;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.StandardCopyOption;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.TimeUnit;
import static org.junit.Assert.assertEquals;
import static org.junit.Assume.assumeTrue;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameter;
import org.junit.runners.Parameterized.Parameters;

/**
 * The scopes: for each positive model {@code P.cfr} of a Clafer checkout,
 * imports included, the frontend's scope file is the one that the compiler
 * writes for the explicit model, {@code P.des.cfr-scope} of
 * {@code clafer -m clafer --meta-data P.cfr}. The compiler's files are in
 * {@code -Dclafer.scopes=DIR}, or written to {@code target/clafer-scopes} by
 * {@code clafer} on the path; without either, the test is skipped.
 */
@RunWith(Parameterized.class)
public class ScopeConformanceTest {

    @Parameter(0)
    public String name;

    @Parameter(1)
    public File model;

    @Parameters(name = "{0}")
    public static List<Object[]> models() {
        List<Object[]> parameters = new ArrayList<>();
        File[] positive = new File(ClaferParserConformanceTest.clafer(), "test/positive").listFiles(
                (d, n) -> n.endsWith(".cfr") && !n.endsWith(".des.cfr") && !n.endsWith(".core.cfr"));
        if (positive != null) {
            Arrays.sort(positive);
            for (File f : positive) {
                parameters.add(new Object[]{f.getName(), f});
            }
        }
        return parameters;
    }

    /**
     * The compiler's scope file of the model, or null without the compiler.
     */
    private File expected() throws IOException, InterruptedException {
        String base = name.substring(0, name.length() - ".cfr".length());
        String given = System.getProperty("clafer.scopes");
        if (given != null) {
            return new File(given, base + ".des.cfr-scope");
        }
        File dir = new File("target/clafer-scopes");
        File scopes = new File(dir, base + ".des.cfr-scope");
        if (scopes.isFile()) {
            return scopes;
        }
        copyModels(model.getParentFile(), dir);
        try {
            Process p = new ProcessBuilder("clafer", "-m", "clafer", "--meta-data", "-s", name).directory(dir)
                    .redirectErrorStream(true).redirectOutput(ProcessBuilder.Redirect.DISCARD).start();
            if (!p.waitFor(300, TimeUnit.SECONDS)) {
                p.destroyForcibly();
            }
        } catch (IOException e) {
            return null;
        }
        return scopes;
    }

    /**
     * The positive models, which a model may import, copied once.
     */
    private static synchronized void copyModels(File models, File dir) throws IOException {
        File done = new File(dir, ".copied");
        if (done.isFile()) {
            return;
        }
        dir.mkdirs();
        for (File f : models.listFiles((d, n) -> n.endsWith(".cfr"))) {
            Files.copy(f.toPath(), new File(dir, f.getName()).toPath(), StandardCopyOption.REPLACE_EXISTING);
        }
        done.createNewFile();
    }

    @Test
    public void testScopeFile() throws IOException, InterruptedException {
        File expected = expected();
        assumeTrue("The compiler's scope file", expected != null && expected.isFile());
        ToIr.Result result = ToIr.read(model.getPath());
        assertEquals(List.of(), result.messages());
        assertEquals(Files.readString(expected.toPath(), StandardCharsets.UTF_8),
                result.scopes().orElseThrow().scopeFile());
    }

    @Test
    public void testSomeModels() {
        assumeTrue(new File(ClaferParserConformanceTest.clafer(), "test/positive").isDirectory());
        assertEquals(true, models().size() > 200);
    }
}
