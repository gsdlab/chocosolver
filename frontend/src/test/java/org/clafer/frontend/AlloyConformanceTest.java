package org.clafer.frontend;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.StandardCopyOption;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Set;
import java.util.concurrent.TimeUnit;
import org.clafer.alloy.AlloyGenerator;
import static org.junit.Assert.assertEquals;
import static org.junit.Assume.assumeFalse;
import static org.junit.Assume.assumeTrue;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameter;
import org.junit.runners.Parameterized.Parameters;

/**
 * The port of the compiler's {@code AlloyLtl.hs} against the compiler, on
 * each positive model of a Clafer checkout:
 * <ul>
 * <li>a temporal model gives its {@code P.als.reg}, byte for byte (the
 * regressions are written with {@code -v}, so without the {@code run}
 * command, and without the statistics);</li>
 * <li>{@code claferc -m alloy} gives what {@code clafer -m alloy -m alloyltl}
 * gives, the compiler's temporal generator, static models included: the
 * standard output, the exit code and {@code P.als}. Without {@code clafer}
 * on the path, this part is skipped.</li>
 * </ul>
 */
@RunWith(Parameterized.class)
public class AlloyConformanceTest {

    @Parameter
    public String name;

    /**
     * The models whose Alloy output differs from the compiler's, with the
     * reason.
     * <ul>
     * <li>{@code escapes}: an {@code [alloy| |]} escape, which
     * {@code claferc} forwards to {@code clafer -m alloy}, whose static
     * generator writes it;</li>
     * <li>{@code comments_and_html_directives}: the options
     * {@code --self-contained --add-graph --add-comments}, which
     * {@code claferc} does not read and forwards, to the same static
     * generator;</li>
     * <li>{@code parent_of_top_level}: {@code root.a.x}, which the explicit
     * IR writes as {@code a.x}, so the port writes {@code alive_a} where the
     * compiler writes {@code (alive_a)}, with the same meaning.</li>
     * </ul>
     */
    private static final Set<String> Differ = Set.of("comments_and_html_directives.cfr", "escapes.cfr",
            "parent_of_top_level.cfr");

    @Parameters(name = "{0}")
    public static List<Object[]> models() {
        List<Object[]> parameters = new ArrayList<>();
        File[] positive = positive().listFiles(
                (d, n) -> n.endsWith(".cfr") && !n.endsWith(".des.cfr") && !n.endsWith(".core.cfr"));
        if (positive != null) {
            Arrays.sort(positive);
            for (File f : positive) {
                parameters.add(new Object[]{f.getName()});
            }
        }
        return parameters;
    }

    private static File positive() {
        return new File(ClaferParserConformanceTest.clafer(), "test/positive");
    }

    private String base() {
        return name.substring(0, name.length() - ".cfr".length());
    }

    @Test
    public void testTemporalRegression() throws IOException, AlloyGenerator.UnsupportedModel {
        File reg = new File(ClaferParserConformanceTest.clafer(), "test/regression/" + base() + ".als.reg");
        assumeTrue("an Alloy regression", reg.isFile());
        ToIr.Result r = ToIr.toIr(ModelFiles.read(new File(positive(), name).getPath(), List.of()));
        assertEquals(List.of(), r.messages());
        assumeTrue("a temporal model", r.model().options().traceLength().isPresent());
        boolean keep = r.files().options().others().contains("-k")
                || r.files().options().others().contains("--keep-unused");
        String code = AlloyGenerator.generate(r.model(), r.scopes().orElseThrow().clafers(),
                r.model().options().traceLength().getAsInt(), keep, true);
        assertEquals(Files.readString(reg.toPath(), StandardCharsets.UTF_8), code);
    }

    private static Boolean clafer;

    private static synchronized boolean clafer() {
        if (clafer == null) {
            try {
                Process p = new ProcessBuilder("clafer", "--numeric-version").redirectErrorStream(true)
                        .redirectOutput(ProcessBuilder.Redirect.DISCARD).start();
                clafer = p.waitFor(60, TimeUnit.SECONDS) && p.exitValue() == 0;
            } catch (IOException | InterruptedException e) {
                clafer = false;
            }
        }
        return clafer;
    }

    private static synchronized File copy(String side) throws IOException {
        File dir = new File("target/clafer-alloy/" + side).getAbsoluteFile();
        File done = new File(dir, ".copied");
        if (!done.isFile()) {
            dir.mkdirs();
            for (File f : positive().listFiles((d, n) -> n.endsWith(".cfr") || n.endsWith(".cfr-scope"))) {
                Files.copy(f.toPath(), new File(dir, f.getName()).toPath(), StandardCopyOption.REPLACE_EXISTING);
            }
            done.createNewFile();
        }
        return dir;
    }

    private record Outcome(int code, String out, String als) {
    }

    private Outcome outcome(File dir, int code, String out) throws IOException {
        File f = new File(dir, base() + ".als");
        String als = f.isFile() ? Files.readString(f.toPath(), StandardCharsets.UTF_8) : "none";
        f.delete();
        // the messages name the model by its path, which is in the folder of each side
        return new Outcome(code, out.replace(dir.getPath() + "/", ""), als.replace(dir.getPath() + "/", ""));
    }

    @Test
    public void testSameAsCompiler() throws IOException, InterruptedException {
        assumeTrue("clafer on the path", clafer());
        assumeFalse("a known difference", Differ.contains(name));
        File hs = copy("hs");
        Process p = new ProcessBuilder("clafer", "-m", "alloy", "-m", "alloyltl", new File(hs, name).getPath())
                .directory(hs).redirectErrorStream(true).start();
        String hsOut = new String(p.getInputStream().readAllBytes(), StandardCharsets.UTF_8);
        assumeTrue("clafer finishes", p.waitFor(300, TimeUnit.SECONDS));
        Outcome expected = outcome(hs, p.exitValue(), hsOut);

        File java = copy("java");
        ByteArrayOutputStream bytes = new ByteArrayOutputStream();
        int code = new ClaferCommand(new ByteArrayInputStream(new byte[0]),
                new PrintStream(bytes, true, StandardCharsets.UTF_8), "clafer")
                .run(List.of("-m", "alloy", new File(java, name).getPath()));
        Outcome actual = outcome(java, code, bytes.toString(StandardCharsets.UTF_8));
        assertEquals(expected, actual);
    }
}
