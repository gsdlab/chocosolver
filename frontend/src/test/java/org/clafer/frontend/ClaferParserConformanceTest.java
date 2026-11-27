package org.clafer.frontend;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assume.assumeTrue;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameter;
import org.junit.runners.Parameterized.Parameters;

/**
 * Check 8: the parser of the full language gives the compiler's parse trees.
 * The models are those of a Clafer checkout next to chocosolver, or at
 * {@code -Dclafer.dir}: the sources of the positive suite, their rich and core
 * explicit regressions, and the negative suite. The compiler's trees are in
 * {@code test/parse-trees}, which {@code test/print-parse-trees.sh} writes;
 * without them the test is skipped.
 * <ul>
 * <li>A model that the compiler parses parses without an error, and
 * {@link SyntaxPrinter} prints its tree as the compiler's {@code printTree}
 * prints the compiler's.</li>
 * <li>A model that the compiler rejects is rejected, with an error at the
 * position of the compiler's; one on which the compiler fails, e.g., with a
 * syntax error at its end, is rejected.</li>
 * </ul>
 */
@RunWith(Parameterized.class)
public class ClaferParserConformanceTest {

    private static final Pattern ParseError = Pattern.compile("^parse error (\\d+):(\\d+): (.*)", Pattern.DOTALL);

    @Parameter(0)
    public String name;

    @Parameter(1)
    public File model;

    @Parameter(2)
    public File tree;

    static File clafer() {
        return new File(System.getProperty("clafer.dir", "../../clafer"));
    }

    @Parameters(name = "{0}")
    public static List<Object[]> models() {
        File test = new File(clafer(), "test");
        List<Object[]> parameters = new ArrayList<>();
        add(parameters, test, "positive", n -> n.endsWith(".cfr") && !n.endsWith(".des.cfr") && !n.endsWith(".core.cfr"));
        add(parameters, test, "regression", n -> n.endsWith(".des.cfr.reg") || n.endsWith(".core.cfr.reg"));
        add(parameters, test, "negative", n -> n.endsWith(".cfr"));
        return parameters;
    }

    private static void add(List<Object[]> parameters, File test, String dir, java.util.function.Predicate<String> isModel) {
        File[] files = new File(test, dir).listFiles((d, n) -> isModel.test(n));
        if (files == null) {
            return;
        }
        Arrays.sort(files);
        for (File f : files) {
            parameters.add(new Object[]{dir + "/" + f.getName(), f,
                new File(test, "parse-trees/" + dir + "/" + f.getName() + ".tree")});
        }
    }

    @Test
    public void testParseTree() throws IOException {
        assumeTrue("No compiler parse tree: run test/print-parse-trees.sh in clafer", tree.isFile());
        String source = Files.readString(model.toPath(), StandardCharsets.UTF_8);
        String expected = Files.readString(tree.toPath(), StandardCharsets.UTF_8);
        ClaferParser.Result result = ClaferParser.parse(source);
        List<String> diagnostics = result.diagnostics().stream().map(Diagnostic::toString).collect(Collectors.toList());
        Matcher error = ParseError.matcher(expected);
        if (expected.startsWith("the compiler fails: ")) {
            assertTrue("The compiler fails on the model (" + expected.strip() + ") but the parser accepts it",
                    !diagnostics.isEmpty());
        } else if (error.find()) {
            String position = error.group(1) + ":" + error.group(2) + ": ";
            assertTrue("The compiler rejects the model at " + position + error.group(3).strip()
                    + "\nbut the parser reports " + diagnostics,
                    diagnostics.stream().anyMatch(d -> d.startsWith(position)));
        } else {
            assertEquals("The compiler parses the model", List.of(), diagnostics);
            assertEquals(expected.stripTrailing(), SyntaxPrinter.print(result.model()).stripTrailing());
        }
    }
}
