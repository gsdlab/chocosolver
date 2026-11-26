package org.clafer.lsp;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutionException;
import org.eclipse.lsp4j.CompletionItem;
import org.eclipse.lsp4j.Position;
import org.eclipse.lsp4j.PrepareRenameResult;
import org.eclipse.lsp4j.Range;
import org.eclipse.lsp4j.TextEdit;
import org.eclipse.lsp4j.WorkspaceEdit;
import org.eclipse.lsp4j.jsonrpc.ResponseErrorException;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

/**
 * Completion and rename, through LSP4J's launchers.
 */
public class CompletionAndRenameTest {

    private static final String Uri = "file:///tmp/phone.cfr";
    private static final String Model = """
            abstract Device
              cost -> integer
            abstract Comp
              price -> integer
            Phone : Device
              apps -> Comp *
              [ cost = 3 ]
              [ all x : apps | x.price > 1 ]
            enum Color = red | green
            c -> Color
            [ c = red ]
            [ let y = Phone.cost in y > 2 ]
            """;

    @Rule
    public TemporaryFolder folder = new TemporaryFolder();

    private ServerHarness h;

    @Before
    public void start() throws Exception {
        h = new ServerHarness();
        h.initialize();
    }

    @After
    public void stop() throws Exception {
        h.close();
    }

    private void openModel() throws Exception {
        h.open(Uri, Model);
        assertEquals(List.of(), h.nextDiagnostics(Uri).getDiagnostics());
    }

    private static List<String> labels(List<CompletionItem> items) {
        return items.stream().sorted(Comparator.comparing(CompletionItem::getSortText))
                .map(CompletionItem::getLabel).toList();
    }

    private static Range range(int line, int character, int endCharacter) {
        return new Range(new Position(line, character), new Position(line, endCharacter));
    }

    /**
     * Applies the edits of one document to its text.
     */
    private static String apply(String text, List<TextEdit> edits) {
        Positions p = new Positions(text);
        List<TextEdit> sorted = new ArrayList<>(edits);
        sorted.sort(Comparator.comparing((TextEdit e) -> e.getRange().getStart().getLine())
                .thenComparing(e -> e.getRange().getStart().getCharacter()).reversed());
        StringBuilder sb = new StringBuilder(text);
        for (TextEdit e : sorted) {
            sb.replace(offset(p, e.getRange().getStart()), offset(p, e.getRange().getEnd()), e.getNewText());
        }
        return sb.toString();
    }

    private static int offset(Positions p, Position pos) {
        int o = 0;
        for (int i = 0; i < pos.getLine(); i++) {
            o += p.line(i).length() + 1;
        }
        return o + pos.getCharacter();
    }

    private void assertRefused(String uri, int line, int character, String newName, String message)
            throws Exception {
        try {
            h.rename(uri, line, character, newName);
            fail("renamed to " + newName);
        } catch (ExecutionException e) {
            assertTrue(e.getCause().toString(), e.getCause() instanceof ResponseErrorException);
            assertTrue(e.getCause().getMessage(), e.getCause().getMessage().contains(message));
        }
    }

    @Test
    public void completionAfterADotOffersTheChildrenOfTheTarget() throws Exception {
        openModel();
        // while the constraint is being written the model has a syntax error; the latest clafers are used
        h.change(Uri, Model.replace("x.price > 1", "x."));
        assertEquals(1, h.nextDiagnostics(Uri).getDiagnostics().size());
        List<String> l = labels(h.completion(Uri, 7, 21));
        assertEquals("price", l.get(0));
        assertTrue(l.toString(), l.contains("dref") && l.contains("parent"));
        assertFalse(l.toString(), l.contains("Phone") || l.contains("all"));
        // this. in Phone: its children and its super's
        h.change(Uri, Model.replace("[ cost = 3 ]", "[ this. ]"));
        h.nextDiagnostics(Uri);
        List<String> t = labels(h.completion(Uri, 6, 9));
        assertEquals(List.of("apps", "cost"), t.subList(0, 2).stream().sorted().toList());
    }

    @Test
    public void completionOffersTheNamesInScopeMostLikelyFirst() throws Exception {
        openModel();
        List<CompletionItem> inBody = h.completion(Uri, 7, 19);
        List<String> l = labels(inBody);
        assertEquals("x", l.get(0));
        // the children of Phone and of its super before the other clafers, and those before the keywords
        assertTrue(l.toString(), l.indexOf("apps") < l.indexOf("Device") && l.indexOf("cost") < l.indexOf("Device"));
        assertTrue(l.toString(), l.indexOf("Color") < l.indexOf("some") && l.contains("red") && l.contains("this"));
        CompletionItem cost = inBody.stream().filter(i -> i.getLabel().equals("cost")).findFirst().orElseThrow();
        assertEquals("0..* cost -> int 1..1", cost.getDetail());
        assertFalse(labels(h.completion(Uri, 6, 4)).contains("x"));
        assertTrue(labels(h.completion(Uri, 11, 24)).get(0).equals("y"));
    }

    @Test
    public void renameAClaferEverywhere() throws Exception {
        openModel();
        PrepareRenameResult p = h.prepareRename(Uri, 6, 5);
        assertEquals(range(6, 4, 8), p.getRange());
        assertEquals("cost", p.getPlaceholder());
        WorkspaceEdit edit = h.rename(Uri, 6, 5, "weight");
        List<TextEdit> edits = edit.getChanges().get(Uri);
        assertEquals(List.of(range(1, 2, 6), range(6, 4, 8), range(11, 16, 20)),
                edits.stream().map(TextEdit::getRange).toList());
        String renamed = apply(Model, edits);
        assertTrue(renamed, renamed.contains("  weight -> integer\n") && renamed.contains("Phone.weight in"));
        h.change(Uri, renamed);
        assertEquals(List.of(), h.nextDiagnostics(Uri).getDiagnostics());
    }

    @Test
    public void renameASuperAndAnEnumerationLiteral() throws Exception {
        openModel();
        List<TextEdit> edits = h.rename(Uri, 0, 10, "Gadget").getChanges().get(Uri);
        assertEquals(List.of(range(0, 9, 15), range(4, 8, 14)), edits.stream().map(TextEdit::getRange).toList());
        List<TextEdit> red = h.rename(Uri, 10, 7, "crimson").getChanges().get(Uri);
        assertEquals(List.of(range(8, 13, 16), range(10, 6, 9)), red.stream().map(TextEdit::getRange).toList());
    }

    @Test
    public void renameALocal() throws Exception {
        openModel();
        List<TextEdit> edits = h.rename(Uri, 7, 19, "a").getChanges().get(Uri);
        assertEquals(List.of(range(7, 8, 9), range(7, 19, 20)), edits.stream().map(TextEdit::getRange).toList());
        assertRefused(Uri, 7, 19, "apps", "taken");
    }

    @Test
    public void renameIsRefusedWhereItCannotBeDone() throws Exception {
        openModel();
        assertNull(h.prepareRename(Uri, 6, 2));
        assertRefused(Uri, 6, 5, "Phone", "taken");
        assertRefused(Uri, 6, 5, "some", "cannot be the name");
        assertRefused(Uri, 6, 5, "integer", "cannot be the name");
        assertRefused(Uri, 6, 5, "1x", "cannot be the name");
        assertRefused(Uri, 6, 5, "a b", "cannot be the name");
        assertRefused(Uri, 6, 2, "z", "Only the names");
        // a model with errors: its names are not all known
        h.change(Uri, Model + "[ some Nothing ]\n");
        assertEquals(1, h.nextDiagnostics(Uri).getDiagnostics().size());
        assertRefused(Uri, 6, 5, "weight", "Fix the errors");
    }

    @Test
    public void renameAcrossOpenDocumentsAndImports() throws Exception {
        Path b = folder.getRoot().toPath().resolve("b.cfr");
        Path a = folder.getRoot().toPath().resolve("a.cfr");
        String bText = "abstract Base\n  size -> integer\n";
        String aText = "//# OPTIONS --import=b.cfr\nY : Base\n[ Y.size > 2 ]\n";
        Files.writeString(b, bText);
        String aUri = a.toUri().toString();
        String bUri = b.toUri().toString();
        h.open(aUri, aText);
        h.open(bUri, bText);
        // renamed from b.cfr, which knows nothing of a.cfr; a.cfr's model has b.cfr
        Map<String, List<TextEdit>> changes = h.rename(bUri, 1, 3, "length").getChanges();
        assertEquals(List.of(range(1, 2, 6)), changes.get(bUri).stream().map(TextEdit::getRange).toList());
        assertEquals(List.of(range(2, 4, 8)), changes.get(aUri).stream().map(TextEdit::getRange).toList());
        assertEquals(2, h.references(bUri, 1, 3, true).size());
    }
}
