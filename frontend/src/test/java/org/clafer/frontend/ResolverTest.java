package org.clafer.frontend;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.List;
import static org.junit.Assert.assertEquals;
import static org.junit.Assume.assumeTrue;
import org.junit.Test;

/**
 * The errors of the name and type resolution, each the compiler's message at
 * its position: on clafer's negative models with such an error, and on small
 * models.
 */
public class ResolverTest {

    private static List<String> errors(String source) {
        return ToIr.toIr(source).diagnostics().stream().map(Diagnostic::toString).toList();
    }

    private static void assertNegative(String model, String error) throws IOException {
        File file = new File(ClaferParserConformanceTest.clafer(), "test/negative/" + model + ".cfr");
        assumeTrue(file.isFile());
        assertEquals(model, List.of(error), errors(Files.readString(file.toPath(), StandardCharsets.UTF_8)));
    }

    /**
     * The negative models whose error is the resolvers', with the compiler's
     * messages ({@code clafer -m clafer}).
     */
    @Test
    public void testNegativeModels() throws IOException {
        assertNegative("ambiguous_name", "8:8: clafer \"claferB\" is not unique. Available paths:\n"
                + "claferA.claferA__claferB\nclaferA.claferA__claferB.claferB__claferB\n");
        assertNegative("compare_root_with_clafer", "4:3: Function '=' cannot be performed on\n"
                + "TClafer {_hi = [\"root\"]}\n=\nTClafer {_hi = [\"a\"]}");
        assertNegative("equality_of_plain_clafers", "7:3: Function '=' cannot be performed on\n"
                + "TClafer {_hi = [\"a\"]}\n=\nTClafer {_hi = [\"b\"]}");
        assertNegative("parent_of_mixed_abstract_variable", "7:23: The parent of an instance of Feat is the root for"
                + " its top-level subclafers (f1) and an instance for the others: take the parent of `this` in a"
                + " constraint of Feat");
        assertNegative("parent_of_optional_top_level", "5:3: The parent of extra is the root when an instance"
                + " exists, and nothing otherwise: it can be navigated from, quantified or counted, e.g.,"
                + " `extra.parent.a`, `some extra.parent` or `#extra.parent`");
        assertNegative("quantifier_over_boolean", "6:3: The type of body of a quantified expression without local"
                + " declarations must not be 'TBoolean'");
        assertNegative("root_as_child", "3:8: The root is not a child of any clafer");
        assertNegative("root_in_union", "5:9: The root has no instance: `root` can be navigated from, e.g.,"
                + " `root.a`, counted (`#root` is 1) and quantified (`some root` holds), but not used in '++'");
        assertNegative("undefined_name_in_constraint", "3:2: Name resolver: 'A' not found within paths:\n"
                + "in context of '\"none\"'");
        assertNegative("variable_over_root", "4:3: A variable cannot range over the root, which has no instance");
    }
}
