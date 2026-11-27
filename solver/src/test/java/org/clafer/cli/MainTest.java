package org.clafer.cli;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.TimeUnit;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import org.junit.Test;

/**
 * The command line: what chocosolver prints and its exit code. Main exits with System.exit, so
 * each test runs it in a JVM of its own, as a user or a script (Clafer's make test, clafer -v)
 * runs it.
 *
 * @author Michał Antkiewicz
 */
public class MainTest {

    private static final String MODEL
            = "defaultScope(1);\n"
            + "intRange(-8, 7);\n"
            + "stringLength(16);\n"
            + "\n"
            + "age = Clafer(\"age\").withCard(1, 1);\n"
            + "age.refTo(Int);\n"
            + "age.addConstraint(equal(joinRef($this()), constant(10)));\n"
            + "\n"
            + "assert(greaterThanEqual(joinRef(global(age)), constant(0)));\n";

    private record Run(int exitCode, String output) {
    }

    // runs chocosolver with the arguments in a new JVM, with the classpath of the tests
    private static Run chocosolver(String... args) throws IOException, InterruptedException {
        List<String> command = new ArrayList<>(List.of(
                Path.of(System.getProperty("java.home"), "bin", "java").toString(),
                "-cp", System.getProperty("java.class.path"),
                Main.class.getName()));
        command.addAll(List.of(args));
        Process process = new ProcessBuilder(command).redirectErrorStream(true).start();
        String output = new String(process.getInputStream().readAllBytes());
        assertTrue("chocosolver did not end", process.waitFor(60, TimeUnit.SECONDS));
        return new Run(process.exitValue(), output);
    }

    // runs chocosolver with a stub clafer, alone on the path: it records its arguments in the file
    // calls next to it and, with js, writes the model's .js file and exits with 0, otherwise 1
    private static Run chocosolverWithStub(Path stubDir, String js, String... args)
            throws IOException, InterruptedException {
        Path stub = stubDir.resolve("clafer");
        Files.writeString(stub, "#!/bin/bash\n"
                + "echo \"$@\" >> \"${0%/*}/calls\"\n"
                + "f=\"${@: -1}\"\n"
                + "if [ -n \"$STUB_JS\" ]; then printf '%s' \"$STUB_JS\" > \"${f%.cfr}.js\"; exit 0; fi\n"
                + "echo 'stub: the model has errors'; exit 1\n");
        stub.toFile().setExecutable(true);
        List<String> command = new ArrayList<>(List.of(
                Path.of(System.getProperty("java.home"), "bin", "java").toString(),
                "-cp", System.getProperty("java.class.path"),
                Main.class.getName()));
        command.addAll(List.of(args));
        ProcessBuilder builder = new ProcessBuilder(command).redirectErrorStream(true);
        builder.environment().put("PATH", stubDir.toString());
        if (js != null)
            builder.environment().put("STUB_JS", js);
        Process process = builder.start();
        String output = new String(process.getInputStream().readAllBytes());
        assertTrue("chocosolver did not end", process.waitFor(60, TimeUnit.SECONDS));
        return new Run(process.exitValue(), output);
    }

    // runs chocosolver with the arguments in a new JVM, with the input on its standard input
    private static Run chocosolverWithInput(String input, String... args) throws IOException, InterruptedException {
        return chocosolverWithInput(null, input, args);
    }

    // ... in the given working directory, e.g., where the REPL saves its files
    private static Run chocosolverWithInput(Path dir, String input, String... args)
            throws IOException, InterruptedException {
        List<String> command = new ArrayList<>(List.of(
                Path.of(System.getProperty("java.home"), "bin", "java").toString(),
                "--enable-native-access=ALL-UNNAMED", "-cp", System.getProperty("java.class.path"),
                Main.class.getName()));
        command.addAll(List.of(args));
        Process process = new ProcessBuilder(command).directory(dir == null ? null : dir.toFile())
                .redirectErrorStream(true).start();
        try (var in = process.getOutputStream()) {
            in.write(input.getBytes());
        }
        String output = new String(process.getInputStream().readAllBytes());
        assertTrue("chocosolver did not end", process.waitFor(60, TimeUnit.SECONDS));
        return new Run(process.exitValue(), output);
    }

    private static String calls(Path stubDir) throws IOException {
        Path calls = stubDir.resolve("calls");
        return Files.exists(calls) ? Files.readString(calls) : "";
    }

    private static File cfr(Path dir, String name, String text) throws IOException {
        Path file = dir.resolve(name);
        Files.writeString(file, text);
        return file.toFile();
    }

    private static File model(String text) throws IOException {
        File file = Files.createTempFile("model", ".js").toFile();
        file.deleteOnExit();
        Files.writeString(file.toPath(), text);
        return file;
    }

    // a wrong command line is one line and a pointer to --help, not a Java stack trace
    private static void assertUsageError(Run run, String message) {
        assertEquals(run.output(), 1, run.exitCode());
        assertTrue(run.output(), run.output().startsWith("clafersg: " + message + "\n"));
        assertTrue(run.output(), run.output().contains("Run clafersg --help to see the options."));
        assertFalse(run.output(), run.output().contains("Exception"));
        assertFalse(run.output(), run.output().contains("\tat "));
    }

    @Test
    public void testModel() throws Exception {
        Run run = chocosolver("--file", model(MODEL).getPath(), "-n", "1");
        assertEquals(run.output(), 0, run.exitCode());
        assertTrue(run.output(), run.output().contains("age"));
    }

    // 28 bubbles map one-to-one onto 28 products: up to symmetry, there is one instance
    private static final String BUBBLES
            = "scope({$Bubble:28, $Product:28, $bubbles:28, $prod:28, $prods:28});\n"
            + "defaultScope(1);\n"
            + "stringLength(16);\n"
            + "\n"
            + "$Product = Abstract(\"Product\");\n"
            + "$Bubble = Abstract(\"Bubble\");\n"
            + "$prod = $Bubble.addChild(\"prod\").withCard(1, 1);\n"
            + "$prods = Clafer(\"prods\").withCard(28, 28);\n"
            + "$bubbles = Clafer(\"bubbles\").withCard(28, 28);\n"
            + "$prods.extending($Product);\n"
            + "$bubbles.extending($Bubble);\n"
            + "$prod.refToUnique($Product);\n"
            + "Constraint(all([disjDecl([$_b1 = local(\"b1\"), $_b2 = local(\"b2\")], global($Bubble))], "
            + "notEqual(joinRef(join($_b1, $prod)), joinRef(join($_b2, $prod)))));\n";

    // After the n-th instance, -n n searched for one more and dropped it: proving that the
    // bubbles have no second instance takes much longer than finding the first, so -n 1 printed
    // the instance and then did not end (gsdlab/chocosolver#46).
    @Test(timeout = 60000)
    public void testNInstancesStopsSearching() throws Exception {
        Run run = chocosolver("--file", model(BUBBLES).getPath(), "-n", "1");
        assertEquals(run.output(), 0, run.exitCode());
        assertTrue(run.output(), run.output().contains("--- Instance 1 End ---"));
        assertTrue(run.output(), run.output().contains("Generated 1 instance(s) within the scope"));
    }

    // jopt-simple's OptionException ended in a stack trace
    @Test
    public void testUnknownOption() throws Exception {
        assertUsageError(chocosolver("--bogus"), "bogus is not a recognized option");
    }

    @Test
    public void testMissingArgument() throws Exception {
        assertUsageError(chocosolver("--file"), "Option file requires an argument");
    }

    // jopt-simple converts a value only when it is read: -n abc failed after the model was solved
    @Test
    public void testWrongArgument() throws Exception {
        Run run = chocosolver("--file", model(MODEL).getPath(), "-n", "abc");
        assertUsageError(run, "Cannot parse argument 'abc' of option n");
        assertFalse(run.output(), run.output().contains("age"));
    }

    @Test
    public void testNoFile() throws Exception {
        assertUsageError(chocosolver("-n", "1"), "use --file to provide the input file");
    }

    @Test
    public void testMissingFile() throws Exception {
        assertUsageError(chocosolver("--file", "missing.js"), "the input file does not exist: missing.js");
    }

    // a model that cannot be read exited with 0 after "Unhandled compilation error occured", also
    // with -v, so clafer -v and Clafer's make test missed it
    @Test
    public void testUnreadableModel() throws Exception {
        File file = model("age = Clafer(\"age\".withCard(1, 1);\n");
        Run run = chocosolver("--file", file.getPath());
        assertEquals(run.output(), 1, run.exitCode());
        assertTrue(run.output(), run.output().contains("Unhandled compilation error occured"));
    }

    @Test
    public void testUnreadableModelValidation() throws Exception {
        File file = model("age = Clafer(\"age\".withCard(1, 1);\n");
        Run run = chocosolver("--file", file.getPath(), "-v");
        assertEquals(run.output(), 1, run.exitCode());
        assertTrue(run.output(), run.output().contains("Unhandled compilation error occured"));
    }

    // a .cfr model is read by the Java frontend: the compiler is not run, and no .js is written
    @Test
    public void testJavaFrontend() throws Exception {
        Path dir = Files.createTempDirectory("cfr");
        File model = cfr(dir, "model.cfr", "A\n  b ?\n[some A.b]\n");
        Run run = chocosolverWithStub(dir, null, "--file", model.getPath(), "-n", "2");
        assertEquals(run.output(), 0, run.exitCode());
        assertTrue(run.output(), run.output().contains("Compiling the Clafer model...\n"));
        assertTrue(run.output(), run.output().contains("A \n  b \n"));
        assertTrue(run.output(), run.output().contains("Generated 1 instance(s) within the scope"));
        assertEquals("", calls(dir));
        assertFalse(new File(dir.toFile(), "model.js").exists());
    }

    // the errors of a model are the Java frontend's, at their positions, if the compiler rejects it too
    @Test
    public void testJavaFrontendErrors() throws Exception {
        Path dir = Files.createTempDirectory("cfr");
        File model = cfr(dir, "model.cfr", "A\n  b -> integer\n[b = c]\n");
        Run run = chocosolverWithStub(dir, null, "--file", model.getPath());
        assertEquals(run.output(), 1, run.exitCode());
        assertTrue(run.output(), run.output().contains(model.getPath() + ":3:6: Name resolver: 'c' not found"));
        assertTrue(run.output(), run.output().contains("Clafer compilation failed. Aborting..."));
        assertFalse(run.output(), run.output().contains("stub"));
        assertTrue(calls(dir), calls(dir).endsWith(model.getPath() + "\n"));
    }

    // a temporal model: the Java frontend's message at its temporal construct
    // a temporal model: its traces, which Alloy finds
    @Test
    public void testTemporalModel() throws Exception {
        Path dir = Files.createTempDirectory("cfr");
        File model = cfr(dir, "model.cfr", "//# OPTIONS --tl=3\nA ?\n[always some A]\n");
        Run run = chocosolverWithStub(dir, null, "--file", model.getPath(), "-n", "1");
        assertEquals(run.output(), 0, run.exitCode());
        assertTrue(run.output(), run.output().contains("=== Trace 1 Begin ===\n\n--- State 0 ---\nA\n"));
        assertTrue(run.output(), run.output().contains("Generated 1 trace(s)"));
        assertEquals(run.output(), "", calls(dir));
    }

    // the REPL of a temporal model: claferIG's, with Alloy
    @Test
    public void testTemporalRepl() throws Exception {
        Path dir = Files.createTempDirectory("cfr");
        File model = cfr(dir, "model.cfr", "//# OPTIONS --tl=3\nA ?\n[always some A]\n");
        Run run = chocosolverWithInput("n\nscope\nfork 1\nx A\nq\n", "--file", model.getPath(), "--repl");
        assertEquals(run.output(), 0, run.exitCode());
        assertTrue(run.output(), run.output().startsWith("Compiling the Clafer model...\n"
                + "Type 'h' for the list of available REPL commands\n\n=== Trace 1 Begin ===\n"));
        assertTrue(run.output(), run.output().contains("claferIG> Global scope = 1\n  A scope = 1\n"));
        assertTrue(run.output(), run.output().contains("No other trace has the states of the last trace before"
                + " state 1."));
        assertTrue(run.output(), run.output().contains("The command 'x' is for static models, which Choco solves."));
    }

    // the REPL of a static model: clafersg's, with the views of the loop
    @Test
    public void testStaticRepl() throws Exception {
        Path dir = Files.createTempDirectory("cfr");
        File model = cfr(dir, "model.cfr", "A\n  b -> integer\n  [b > 2]\n");
        Run run = chocosolverWithInput("minimize b\nd\nn\nf b\ntl 3\nfoo\nq\n", "--file", model.getPath(),
                "--repl");
        assertEquals(run.output(), 0, run.exitCode());
        assertTrue(run.output(), run.output().contains("Type 'help' for the list of available REPL commands\n\n"
                + "=== Instance 1 Begin ===\n\n"));
        // minimi'z'e, also by its name
        assertTrue(run.output(), run.output().matches("(?s).*Minimize: minimize b\n\n=== Instance 2 Begin ===\n\n"
                + "A \n  b -> int = 3 \n.*"));
        assertTrue(run.output(), run.output().contains("Each next instance or state is shown as its changes"));
        assertTrue(run.output(), run.output().contains("The command 'tl' is for temporal models, which Alloy"
                + " solves."));
        assertTrue(run.output(), run.output().contains("Unhandled command: foo"));
    }

    // --diff: the REPL shows each next instance as its changes, and its graph too
    @Test
    public void testStaticReplDiff() throws Exception {
        Path dir = Files.createTempDirectory("cfr");
        File model = cfr(dir, "model.cfr", "A\n  xor B\n    C\n    D\n");
        Run run = chocosolverWithInput(dir, "n\ndot\nq\n", "--file", model.getPath(), "--repl", "--prettify", "--diff");
        assertEquals(run.output(), 0, run.exitCode());
        assertTrue(run.output(), run.output().contains("=== Instance 1 Begin ===\n\n+ A\n"));
        assertTrue(run.output(), run.output().matches("(?s).*=== Instance 2 Begin ===\n\n-     [CD]\n\\+     [CD]\n.*"));
        String dot = Files.readString(dir.resolve("model.cfr.2.dot"));
        assertTrue(dot, dot.matches("(?s).*instance \\[label=\"-     [CD]\\\\l\\+     [CD]\\\\l\"\\];.*"));
    }

    // a temporal model's REPL takes --diff, --scope and --maxint, as a static model's
    @Test
    public void testTemporalReplOptions() throws Exception {
        Path dir = Files.createTempDirectory("cfr");
        File model = cfr(dir, "model.cfr", "//# OPTIONS --tl=3\nA *\n[always some A]\n");
        Run run = chocosolverWithInput("c\nq\n", "--file", model.getPath(), "--repl", "--diff", "--scope=3",
                "--maxint=127");
        assertEquals(run.output(), 0, run.exitCode());
        assertTrue(run.output(), run.output().contains("--- State 0 ---\n+ A"));
        assertTrue(run.output(), run.output().contains("Integer range = -128..127 (bitwidth = 8)\nGlobal scope  = 3\n"));
    }

    // a model that the Java frontend rejects and the compiler compiles: the compiler's output, with a note
    @Test
    public void testFallbackOnRejection() throws Exception {
        Path dir = Files.createTempDirectory("cfr");
        File model = cfr(dir, "model.cfr", "A\n  b -> integer\n[b = c]\n");
        Run run = chocosolverWithStub(dir, MODEL, "--file", model.getPath(), "-n", "1");
        assertEquals(run.output(), 0, run.exitCode());
        assertTrue(run.output(), run.output().contains("The Java frontend rejected the model, which the Haskell"
                + " compiler (clafer) compiles"));
        assertTrue(run.output(), run.output().contains(model.getPath() + ":3:6: Name resolver: 'c' not found"));
        assertTrue(run.output(), run.output().contains("age"));
    }

    // --haskell-frontend: the compiler's output, with the imports of the command line
    @Test
    public void testHaskellFrontend() throws Exception {
        Path dir = Files.createTempDirectory("cfr");
        File model = cfr(dir, "model.cfr", "A\n");
        Run run = chocosolverWithStub(dir, MODEL, "--file", model.getPath(), "-n", "1", "--haskell-frontend",
                "--import", "lib.cfr");
        assertEquals(run.output(), 0, run.exitCode());
        assertTrue(run.output(), run.output().contains("age"));
        assertEquals("-k -m choco --import=lib.cfr " + model.getPath() + "\n", calls(dir));
        // the compiler's errors, as before the Java frontend
        run = chocosolverWithStub(dir, null, "--file", model.getPath(), "--haskell-frontend");
        assertEquals(run.output(), 1, run.exitCode());
        assertTrue(run.output(), run.output().contains("stub: the model has errors\nClafer compilation failed."));
    }

    // a [choco| |] escape is JavaScript, which only the compiler's output runs
    @Test
    public void testChocoEscape() throws Exception {
        Path dir = Files.createTempDirectory("cfr");
        File model = cfr(dir, "model.cfr", "A\n[choco|// a comment|]\n");
        Run run = chocosolverWithStub(dir, MODEL, "--file", model.getPath(), "-n", "1");
        assertEquals(run.output(), 0, run.exitCode());
        assertTrue(run.output(), run.output().contains("has a [choco| |] escape"));
        assertTrue(run.output(), run.output().contains("age"));
    }

    // serves the files of the map on 127.0.0.1, at /m/NAME; the others are 404
    private static com.sun.net.httpserver.HttpServer serve(java.util.Map<String, String> files) throws IOException {
        com.sun.net.httpserver.HttpServer server = com.sun.net.httpserver.HttpServer.create(
                new java.net.InetSocketAddress(java.net.InetAddress.getLoopbackAddress(), 0), 0);
        server.createContext("/m/", exchange -> {
            String text = files.get(exchange.getRequestURI().getPath().substring("/m/".length()));
            if (text == null) {
                exchange.sendResponseHeaders(404, -1);
            } else {
                byte[] body = text.getBytes(java.nio.charset.StandardCharsets.UTF_8);
                exchange.getResponseHeaders().add("Content-Type", "text/plain");
                exchange.sendResponseHeaders(200, body.length);
                exchange.getResponseBody().write(body);
            }
            exchange.close();
        });
        server.start();
        return server;
    }

    private static String base(com.sun.net.httpserver.HttpServer server) {
        return "http://127.0.0.1:" + server.getAddress().getPort() + "/m/";
    }

    private static final String Lib = "abstract L\n  age -> integer\n  [ age = 10 ]\n";

    // the web tools' restrictions of the imports, as the compiler has them, in the Java frontend
    @Test
    public void testImportRestrictions() throws Exception {
        com.sun.net.httpserver.HttpServer server = serve(java.util.Map.of("lib.cfr", Lib));
        try {
            Path dir = Files.createTempDirectory("cfr");
            Files.createDirectory(dir.resolve("models"));
            cfr(dir, "lib.cfr", Lib);
            File inside = cfr(dir.resolve("models"), "inside.cfr", "A : L\n");
            File imports = cfr(dir.resolve("models"), "imports.cfr", "//# OPTIONS --import=../lib.cfr\nA : L\n");
            // inside the root
            Run run = chocosolverWithStub(dir, null, "--import-root=" + dir, "--file", imports.getPath(), "-n", "1");
            assertEquals(run.output(), 0, run.exitCode());
            assertTrue(run.output(), run.output().contains("10"));
            // outside it: the compiler's error, which the stub compiler does not undo
            run = chocosolverWithStub(dir, null, "--import-root=" + dir.resolve("models"), "--file",
                    imports.getPath(), "-n", "1");
            assertEquals(run.output(), 1, run.exitCode());
            assertTrue(run.output(), run.output().contains(imports.getPath() + ": the import " + dir
                    + "/models/../lib.cfr is outside the folders from which files can be imported"));
            // the model's imports relative to the URL of --import-base, from which only URLs can be imported
            run = chocosolverWithStub(dir, null, "--import-base", base(server) + "inside.cfr", "--file",
                    imports.getPath(), "-n", "1");
            assertEquals(run.output(), 1, run.exitCode());
            assertTrue(run.output(), run.output().contains("cannot read " + base(server).replace("/m/", "/lib.cfr")
                    + ": HTTP 404 Not Found"));
            File relative = cfr(dir.resolve("models"), "relative.cfr", "//# OPTIONS --import=lib.cfr\nA : L\n");
            run = chocosolverWithStub(dir, null, "--import-base", base(server) + "inside.cfr", "--file",
                    relative.getPath(), "-n", "1");
            assertEquals(run.output(), 0, run.exitCode());
            assertTrue(run.output(), run.output().contains("10"));
            // only public addresses, which 127.0.0.1 is not
            run = chocosolverWithStub(dir, null, "--public-imports", "--import", base(server) + "lib.cfr", "--file",
                    inside.getPath(), "-n", "1");
            assertEquals(run.output(), 1, run.exitCode());
            assertTrue(run.output(), run.output().contains("cannot read " + base(server) + "lib.cfr: the host"
                    + " 127.0.0.1 has no public address"));
            assertEquals("-k -m choco --import-root=" + dir.resolve("models") + " " + imports.getPath() + "\n"
                    + "-k -m choco --import-base=" + base(server) + "inside.cfr " + imports.getPath() + "\n"
                    + "-k -m choco --import=" + base(server) + "lib.cfr --public-imports " + inside.getPath()
                    + "\n", calls(dir));
        } finally {
            server.stop(0);
        }
    }

    // an import of a URL, in the model or on the command line, read by the Java frontend
    @Test
    public void testUrlImports() throws Exception {
        com.sun.net.httpserver.HttpServer server = serve(java.util.Map.of("lib.cfr", Lib));
        try {
            Path dir = Files.createTempDirectory("cfr");
            File model = cfr(dir, "model.cfr", "//# OPTIONS --import=" + base(server) + "lib.cfr\nA : L\n");
            Run run = chocosolverWithStub(dir, null, "--file", model.getPath(), "-n", "1");
            assertEquals(run.output(), 0, run.exitCode());
            assertTrue(run.output(), run.output().contains("10"));
            File plain = cfr(dir, "plain.cfr", "A : L\n");
            run = chocosolverWithStub(dir, null, "--file", plain.getPath(), "-n", "1", "--import",
                    base(server) + "lib.cfr");
            assertEquals(run.output(), 0, run.exitCode());
            assertTrue(run.output(), run.output().contains("10"));
            assertEquals("", calls(dir));
            run = chocosolverWithStub(dir, null, "--file", plain.getPath(), "-n", "1", "--import",
                    base(server) + "none.cfr");
            assertEquals(run.output(), 1, run.exitCode());
            assertTrue(run.output(), run.output().contains(plain.getPath() + ": cannot read " + base(server)
                    + "none.cfr: HTTP 404 Not Found"));
        } finally {
            server.stop(0);
        }
    }
}
