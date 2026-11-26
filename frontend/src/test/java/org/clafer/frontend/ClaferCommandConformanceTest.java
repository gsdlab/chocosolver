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
import java.util.concurrent.TimeUnit;
import static org.junit.Assert.assertEquals;
import static org.junit.Assume.assumeTrue;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameter;
import org.junit.runners.Parameterized.Parameters;

/**
 * {@code claferc} against the compiler on each positive model of a Clafer
 * checkout, imports included, in three runs: {@code -m clafer --meta-data},
 * {@code -m clafer --core --meta-data} and
 * {@code -m clafer --core --record-provenance}. The standard output, the exit
 * code and every file written ({@code P.des.cfr}, {@code P.core.cfr},
 * {@code P.cfr-map}, {@code P.des.cfr-scope}, {@code P.core.cfr-scope}) are
 * the compiler's, byte for byte. The models are copied to
 * {@code target/clafer-command}, once for the compiler and once for the Java
 * side; without {@code clafer} on the path, the test is skipped.
 */
@RunWith(Parameterized.class)
public class ClaferCommandConformanceTest {

    @Parameter(0)
    public String name;

    @Parameter(1)
    public String run;

    private static final List<List<String>> Runs = List.of(List.of("-m", "clafer", "--meta-data"),
            List.of("-m", "clafer", "--core", "--meta-data"), List.of("-m", "clafer", "--core", "--record-provenance"));

    private static final List<String> Outputs = List.of(".des.cfr", ".core.cfr", ".cfr-map", ".des.cfr-scope",
            ".core.cfr-scope");

    @Parameters(name = "{0} {1}")
    public static List<Object[]> models() {
        List<Object[]> parameters = new ArrayList<>();
        File[] positive = positive().listFiles(
                (d, n) -> n.endsWith(".cfr") && !n.endsWith(".des.cfr") && !n.endsWith(".core.cfr"));
        if (positive != null) {
            Arrays.sort(positive);
            for (File f : positive) {
                for (int i = 0; i < Runs.size(); i++) {
                    parameters.add(new Object[]{f.getName(), Integer.toString(i)});
                }
            }
        }
        return parameters;
    }

    private static File positive() {
        return new File(ClaferParserConformanceTest.clafer(), "test/positive");
    }

    private static boolean hasClafer() {
        try {
            Process p = new ProcessBuilder("clafer", "--numeric-version").redirectErrorStream(true)
                    .redirectOutput(ProcessBuilder.Redirect.DISCARD).start();
            return p.waitFor(60, TimeUnit.SECONDS) && p.exitValue() == 0;
        } catch (IOException | InterruptedException e) {
            return false;
        }
    }

    private static Boolean clafer;

    private static synchronized boolean clafer() {
        if (clafer == null) {
            clafer = hasClafer();
        }
        return clafer;
    }

    /**
     * The positive models, which a model may import, copied once to the
     * folder.
     */
    private static synchronized File copy(String side) throws IOException {
        File dir = new File("target/clafer-command/" + side).getAbsoluteFile();
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

    private record Outcome(int code, String out, List<String> files) {
    }

    private Outcome outcome(File dir, int code, String out) throws IOException {
        String base = name.substring(0, name.length() - ".cfr".length());
        List<String> files = new ArrayList<>();
        for (String ext : Outputs) {
            File f = new File(dir, base + ext);
            files.add(ext + ": " + (f.isFile() ? Files.readString(f.toPath(), StandardCharsets.UTF_8) : "none"));
            f.delete();
        }
        // the messages name the model by its path, which is in the folder of each side
        return new Outcome(code, out.replace(dir.getPath() + "/", ""), files);
    }

    @Test
    public void testSameOutputs() throws IOException, InterruptedException {
        assumeTrue("clafer on the path", clafer());
        List<String> args = new ArrayList<>(Runs.get(Integer.parseInt(run)));
        File hs = copy("hs");
        args.add(new File(hs, name).getPath());
        Process p = new ProcessBuilder(new ArrayList<>(List.of("clafer")) {
            {
                addAll(args);
            }
        }).directory(hs).redirectErrorStream(true).start();
        String hsOut = new String(p.getInputStream().readAllBytes(), StandardCharsets.UTF_8);
        assumeTrue("clafer finishes", p.waitFor(300, TimeUnit.SECONDS));
        Outcome expected = outcome(hs, p.exitValue(), hsOut);

        File java = copy("java");
        args.set(args.size() - 1, new File(java, name).getPath());
        ByteArrayOutputStream bytes = new ByteArrayOutputStream();
        int code = new ClaferCommand(new ByteArrayInputStream(new byte[0]),
                new PrintStream(bytes, true, StandardCharsets.UTF_8), "clafer").run(args);
        Outcome actual = outcome(java, code, bytes.toString(StandardCharsets.UTF_8));
        assertEquals(expected, actual);
    }
}
