package org.clafer.frontend;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.List;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

/**
 * {@code claferc} ({@link ClaferCommand}): its outputs, statistics and
 * errors, each as {@code clafer -m clafer} gives them, and what it forwards
 * to the compiler, here a stub that prints its arguments.
 * {@link ClaferCommandConformanceTest} compares it with the compiler.
 */
public class ClaferCommandTest {

    @Rule
    public TemporaryFolder folder = new TemporaryFolder();

    private File stub;

    @Before
    public void setUp() throws IOException {
        stub = folder.newFile("clafer");
        Files.writeString(stub.toPath(), "#!/bin/sh\necho \"clafer $*\"\nexit 3\n");
        assertTrue(stub.setExecutable(true));
    }

    private record Run(int code, String out) {
    }

    private Run runWithInput(String stdin, String... args) {
        ByteArrayOutputStream bytes = new ByteArrayOutputStream();
        PrintStream out = new PrintStream(bytes, true, StandardCharsets.UTF_8);
        int code = new ClaferCommand(new ByteArrayInputStream(stdin.getBytes(StandardCharsets.UTF_8)), out,
                stub.getPath()).run(List.of(args));
        return new Run(code, bytes.toString(StandardCharsets.UTF_8));
    }

    private Run run(String... args) {
        return runWithInput("", args);
    }

    private String model(String name, String text) throws IOException {
        File f = new File(folder.getRoot(), name);
        Files.writeString(f.toPath(), text);
        return f.getPath();
    }

    private String read(String path) throws IOException {
        return Files.readString(new File(path).toPath());
    }

    private static final String Stats = "All clafers: 2 | Abstract: 0 | Concrete: 2 | Reference: 1\n"
            + "Constraints: 1\nGoals: 0\nGlobal scope: 3..3\nCan skip name resolver: yes\n\n";

    @Test
    public void testExplicitModel() throws IOException {
        String m = model("m.cfr", "A\n  b -> integer 3\n  [ b = 1 ]\n");
        String base = m.substring(0, m.length() - ".cfr".length());
        assertEquals(new Run(0, Stats), run("-m", "clafer", m));
        assertEquals("//# OPTIONS --explicit\n0..* A 1..1 {\n  0..* b -> int 3..3 {}\n  [this.b.dref = 1]\n}\n",
                read(base + ".des.cfr"));
        assertFalse(new File(base + ".cfr-map").exists());
        // the options are cmdargs': grouped short flags, -mclafer, --mode=clafer
        assertEquals(new Run(0, ""), run("-smclafer", "--meta-data", m));
        assertEquals("[\n{\n\"fqName\":\"root__A\",\n\"lpqName\":\"A\",\n\"uid\":\"A\"\n},\n{\n"
                + "\"fqName\":\"root__A__b\",\n\"lpqName\":\"b\",\n\"uid\":\"b\"\n}\n]", read(base + ".cfr-map"));
        assertTrue(read(base + ".des.cfr-scope").startsWith("[\n{\"lpqName\": \"A\", \"scope\": 1},\n"));
        assertEquals(new Run(0, ""), run("--mode=clafer", "-s", "--core", "--meta-data", m));
        assertTrue(new File(base + ".core.cfr").isFile());
        assertEquals(read(base + ".des.cfr-scope"), read(base + ".core.cfr-scope"));
    }

    @Test
    public void testConsoleAndStdin() throws IOException {
        String explicit = "//# OPTIONS --explicit\n0..* A 0..1 {}\n";
        String m = model("m.cfr", "A ?\n");
        assertEquals(new Run(0, explicit + "\n"), run("-m", "clafer", "-o", "-s", m));
        assertFalse(new File(folder.getRoot(), "m.des.cfr").exists());
        // the standard input is the model, and the output goes to the console
        assertEquals(new Run(0, explicit + "\n"), runWithInput("A ?\n", "-m", "clafer", "-s"));
    }

    @Test
    public void testCoreWithProvenance() throws IOException {
        String m = model("e.cfr", "enum C = r | g\nA\n  x -> C := r\n");
        assertEquals(0, run("-m", "clafer", "-s", "--core", "--record-provenance", m).code());
        String core = read(m.replace(".cfr", ".core.cfr"));
        assertTrue(core, core.contains("abstract 0..* C 0..* {} // enum, e.cfr:1:1\n"));
        assertTrue(core, core.contains(" // :=, e.cfr:3:3\n"));
        assertEquals(0, run("-m", "clafer", "-s", "--core", m).code());
        assertFalse(read(m.replace(".cfr", ".core.cfr")).contains("//"
                + " enum"));
    }

    @Test
    public void testOptionsOfTheModel() throws IOException {
        // the mode and the options in the model's first line
        String m = model("o.cfr", "//# OPTIONS -m clafer --core -s\nA ?\n");
        assertEquals(new Run(0, ""), run(m));
        assertTrue(read(m.replace(".cfr", ".core.cfr")).startsWith("//# OPTIONS --explicit --core\n"));
        String imported = model("i.cfr", "B\n");
        String importing = model("j.cfr", "//# OPTIONS --import=i.cfr\nA\n");
        assertEquals(new Run(0, ""), run("-m", "clafer", "-s", importing));
        assertEquals("//# OPTIONS --explicit\n0..* B 1..1 {}\n0..* A 1..1 {}\n",
                read(importing.replace(".cfr", ".des.cfr")));
        assertTrue(imported.endsWith("i.cfr"));
    }

    @Test
    public void testErrors() throws IOException {
        String m = model("x.cfr", "A\n  [ c ]\n");
        assertEquals(new Run(1, "\nCompile error at " + m + ":2:5...\nName resolver: 'c' not found within paths:A\n"
                + "in context of '\"A\"'\n"), run("-m", "clafer", m));
        assertEquals(new Run(1, "\nError...\nthe file " + folder.getRoot() + "/none.cfr does not exist\n"),
                run("-m", "clafer", folder.getRoot() + "/none.cfr"));
        String p = model("p.cfr", "A\n  [ ]]\n");
        assertTrue(run("-m", "clafer", p).out().startsWith("\nParse failed at " + p + ":2:6...\n"));
    }

    @Test
    public void testForwarded() throws IOException {
        String m = model("f.cfr", "A\n");
        List<List<String>> forwarded = new ArrayList<>();
        for (String mode : List.of("json", "html", "graph", "cvlgraph", "alloy", "choco")) {
            forwarded.add(List.of("-m", mode, m));
        }
        forwarded.add(List.of("-m", "clafer", "-m", "html", m));
        // the default mode, alloy
        forwarded.add(List.of(m));
        // the options that the Java side does not implement, and those it does not know
        for (String option : List.of("-i", "-v", "--validate", "--tooldir=.", "-c", "--check-afm", "-l", "--nl",
                "-a", "--self-contained", "--add-graph", "--sr", "--add-comments", "-e", "--unknown", "-x",
                "--ss=other")) {
            forwarded.add(List.of("-m", "clafer", option, m));
        }
        forwarded.add(List.of("-m", "clafer", m, m));
        for (List<String> args : forwarded) {
            assertEquals(args.toString(), new Run(3, "clafer " + String.join(" ", args) + "\n"),
                    run(args.toArray(String[]::new)));
        }
        // a mode of the model's options, which the command line's overrides, and an import of a URL in it
        String json = model("g.cfr", "//# OPTIONS -m json\nA\n");
        assertEquals(new Run(3, "clafer " + json + "\n"), run(json));
        assertEquals(0, run("-m", "clafer", "-s", json).code());
        assertTrue(new File(folder.getRoot(), "g.des.cfr").isFile());
        // the restrictions of the imports are read: the import is outside the root
        String imports = model("r.cfr", "//# OPTIONS --import=f.cfr\nB\n");
        assertEquals(new Run(1, "\nError...\n" + imports + ": the import " + m
                + " is outside the folders from which files can be imported\n"),
                run("-m", "clafer", "--import-root=" + folder.getRoot() + "/none", "--public-imports", imports));
    }

    @Test
    public void testNoCompiler() throws IOException {
        String m = model("n.cfr", "A\n");
        ByteArrayOutputStream bytes = new ByteArrayOutputStream();
        int code = new ClaferCommand(new ByteArrayInputStream(new byte[0]),
                new PrintStream(bytes, true, StandardCharsets.UTF_8), folder.getRoot() + "/missing")
                .run(List.of("-m", "json", m));
        assertEquals(1, code);
        assertTrue(bytes.toString(), bytes.toString().startsWith("\nError...\nThe Java frontend does not implement"
                + " the mode json, which needs the compiler " + folder.getRoot() + "/missing, but it cannot be run"));
    }

    @Test
    public void testDropExtension() {
        assertEquals("a/b", ClaferCommand.dropExtension("a/b.cfr"));
        assertEquals("a.b/c", ClaferCommand.dropExtension("a.b/c"));
        assertEquals("m.des", ClaferCommand.dropExtension("m.des.cfr"));
        assertEquals("", ClaferCommand.dropExtension(""));
    }
}
