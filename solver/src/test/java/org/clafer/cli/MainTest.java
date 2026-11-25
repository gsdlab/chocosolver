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
        assertTrue(run.output(), run.output().startsWith("chocosolver: " + message + "\n"));
        assertTrue(run.output(), run.output().contains("Run chocosolver --help to see the options."));
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
    @Test
    public void testTemporalModel() throws Exception {
        Path dir = Files.createTempDirectory("cfr");
        File model = cfr(dir, "model.cfr", "//# OPTIONS --tl=3\nA ?\n[always some A]\n");
        Run run = chocosolverWithStub(dir, null, "--file", model.getPath());
        assertEquals(run.output(), 1, run.exitCode());
        assertTrue(run.output(), run.output().contains(model.getPath()
                + ":3:2: Chocosolver does not solve temporal models"));
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

    // the web tools' restrictions of the imports apply only to a model with imports
    @Test
    public void testImportRestrictions() throws Exception {
        Path dir = Files.createTempDirectory("cfr");
        cfr(dir, "lib.cfr", "abstract L\n");
        File plain = cfr(dir, "plain.cfr", "A\n");
        Run run = chocosolverWithStub(dir, MODEL, "--public-imports", "--import-root=" + dir, "--file",
                plain.getPath(), "-n", "1");
        assertEquals(run.output(), 0, run.exitCode());
        assertEquals("", calls(dir));
        assertFalse(run.output(), run.output().contains("age"));
        File imports = cfr(dir, "imports.cfr", "//# OPTIONS --import=lib.cfr\nA : L\n");
        run = chocosolverWithStub(dir, MODEL, "--public-imports", "--import-root=" + dir, "--import-base",
                "https://example.org/m.cfr", "--file", imports.getPath(), "-n", "1");
        assertEquals(run.output(), 0, run.exitCode());
        assertTrue(run.output(), run.output().contains("has imports, which --import-root restricts"));
        assertEquals("-k -m choco --import-root=" + dir + " --public-imports --import-base=https://example.org/m.cfr "
                + imports.getPath() + "\n", calls(dir));
    }

    // an import of a URL, in the model or on the command line
    @Test
    public void testUrlImports() throws Exception {
        Path dir = Files.createTempDirectory("cfr");
        File model = cfr(dir, "model.cfr", "//# OPTIONS --import=https://example.org/lib.cfr\nA\n");
        Run run = chocosolverWithStub(dir, MODEL, "--file", model.getPath(), "-n", "1");
        assertEquals(run.output(), 0, run.exitCode());
        assertTrue(run.output(), run.output().contains("The model imports a URL"));
        File plain = cfr(dir, "plain.cfr", "A\n");
        run = chocosolverWithStub(dir, MODEL, "--file", plain.getPath(), "-n", "1", "--import",
                "http://example.org/lib.cfr");
        assertEquals(run.output(), 0, run.exitCode());
        assertTrue(run.output(), run.output().contains("The model imports a URL (--import)"));
    }
}
