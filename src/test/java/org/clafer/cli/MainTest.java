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
}
