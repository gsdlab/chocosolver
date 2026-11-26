package org.clafer.lsp;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.concurrent.TimeUnit;
import org.eclipse.lsp4j.Diagnostic;
import org.eclipse.lsp4j.DiagnosticSeverity;
import org.eclipse.lsp4j.DocumentSymbol;
import org.eclipse.lsp4j.InitializeResult;
import org.eclipse.lsp4j.Position;
import org.eclipse.lsp4j.PublishDiagnosticsParams;
import org.eclipse.lsp4j.Range;
import org.eclipse.lsp4j.SymbolKind;
import org.eclipse.lsp4j.TextDocumentSyncKind;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

/**
 * The server driven through LSP4J's launchers over in-memory streams.
 */
public class ClaferLanguageServerTest {

    @Rule
    public TemporaryFolder folder = new TemporaryFolder();

    private ServerHarness h;

    @Before
    public void start() throws Exception {
        h = new ServerHarness();
    }

    @After
    public void stop() throws Exception {
        h.close();
    }

    private static Range range(int line, int character, int endLine, int endCharacter) {
        return new Range(new Position(line, character), new Position(endLine, endCharacter));
    }

    private static String uri(Path p) {
        return p.toUri().toString();
    }

    @Test
    public void initializeAdvertisesFullSyncAndOutline() throws Exception {
        InitializeResult r = h.initialize();
        assertEquals(TextDocumentSyncKind.Full, r.getCapabilities().getTextDocumentSync().getLeft());
        assertTrue(r.getCapabilities().getDocumentSymbolProvider().getLeft());
        assertEquals("clafer-ls", r.getServerInfo().getName());
    }

    @Test
    public void brokenModelReportsEverySyntaxErrorAtItsRange() throws Exception {
        h.initialize();
        String uri = "file:///tmp/broken.cfr";
        h.open(uri, "A\n  B ->\n  C\n[ x = ]\nD\n");
        List<Diagnostic> ds = h.nextDiagnostics(uri).getDiagnostics();
        assertEquals(2, ds.size());
        assertEquals("Expected an expression but found a new line", ds.get(0).getMessage());
        // at the end of "  B ->", where the frontend's 2:8 is, past the line's last column
        assertEquals(range(1, 6, 1, 6), ds.get(0).getRange());
        assertEquals("Expected an expression but found ']'", ds.get(1).getMessage());
        // the ']' of "[ x = ]", widened from the empty span to the character
        assertEquals(range(3, 6, 3, 7), ds.get(1).getRange());
        assertEquals(DiagnosticSeverity.Error, ds.get(1).getSeverity());
        assertEquals("clafer", ds.get(1).getSource());
    }

    @Test
    public void resolverErrorAfterATabIsAtTheRightCharacter() throws Exception {
        h.initialize();
        String uri = "file:///tmp/tab.cfr";
        // the compiler's column of E is 16, since the tab moves to column 9; it is character 8 of the line
        h.open(uri, "A\n\t[ some E ]\n");
        List<Diagnostic> ds = h.nextDiagnostics(uri).getDiagnostics();
        assertEquals(1, ds.size());
        assertTrue(ds.get(0).getMessage(), ds.get(0).getMessage().startsWith("Name resolver: 'E' not found"));
        assertEquals(new Position(1, 8), ds.get(0).getRange().getStart());
    }

    @Test
    public void fixingTheModelClearsItsDiagnostics() throws Exception {
        h.initialize();
        String uri = "file:///tmp/fix.cfr";
        h.open(uri, "A\n  B\n[ some E ]\n");
        assertEquals(1, h.nextDiagnostics(uri).getDiagnostics().size());
        h.change(uri, "A\n  B\n[ some B ]\n");
        assertEquals(List.of(), h.nextDiagnostics(uri).getDiagnostics());
        h.change(uri, "A\n  B ->\n");
        assertEquals(1, h.nextDiagnostics(uri).getDiagnostics().size());
        h.close(uri);
        assertEquals(List.of(), h.nextDiagnostics(uri).getDiagnostics());
    }

    @Test
    public void outlineOfNestedClafers() throws Exception {
        h.initialize();
        String uri = "file:///tmp/phone.cfr";
        String model = """
                abstract Device
                  cost -> integer
                  xor size
                    small
                    large
                Phone : Device ?
                  apps -> Device *
                  [ cost = 3 ]
                enum Color = red | green
                assert [ some Phone ]
                << min Phone.cost >>
                """;
        h.open(uri, model);
        assertEquals(List.of(), h.nextDiagnostics(uri).getDiagnostics());
        List<DocumentSymbol> outline = h.outline(uri);
        assertEquals(List.of("Device", "Phone", "Color", "assert [ some Phone ]", "<< min Phone.cost >>"),
                outline.stream().map(DocumentSymbol::getName).toList());

        DocumentSymbol device = outline.get(0);
        assertEquals(SymbolKind.Class, device.getKind());
        assertEquals(range(0, 9, 0, 15), device.getSelectionRange());
        assertEquals(0, device.getRange().getStart().getLine());
        assertEquals(List.of("cost", "size"), device.getChildren().stream().map(DocumentSymbol::getName).toList());
        assertEquals("-> integer", device.getChildren().get(0).getDetail());
        DocumentSymbol size = device.getChildren().get(1);
        assertEquals(SymbolKind.Field, size.getKind());
        assertEquals(List.of("small", "large"), size.getChildren().stream().map(DocumentSymbol::getName).toList());
        assertEquals(range(4, 4, 4, 9), size.getChildren().get(1).getSelectionRange());

        DocumentSymbol phone = outline.get(1);
        assertEquals(": Device ?", phone.getDetail());
        assertEquals(List.of("apps", "[ cost = 3 ]"), phone.getChildren().stream().map(DocumentSymbol::getName).toList());
        assertEquals("-> Device *", phone.getChildren().get(0).getDetail());
        assertEquals(SymbolKind.Event, phone.getChildren().get(1).getKind());

        DocumentSymbol color = outline.get(2);
        assertEquals(SymbolKind.Enum, color.getKind());
        assertEquals(List.of("red", "green"), color.getChildren().stream().map(DocumentSymbol::getName).toList());
        assertEquals(SymbolKind.EnumMember, color.getChildren().get(0).getKind());
        assertEquals(SymbolKind.Boolean, outline.get(3).getKind());
        assertEquals(SymbolKind.Operator, outline.get(4).getKind());
    }

    @Test
    public void outlineOfABrokenModelKeepsWhatWasRead() throws Exception {
        h.initialize();
        String uri = "file:///tmp/partial.cfr";
        h.open(uri, "A\n  B ->\n  C\nD\n  E\n");
        assertEquals(1, h.nextDiagnostics(uri).getDiagnostics().size());
        List<DocumentSymbol> outline = h.outline(uri);
        assertEquals(List.of("A", "D"), outline.stream().map(DocumentSymbol::getName).toList());
        assertTrue(outline.get(0).getChildren().stream().anyMatch(s -> s.getName().equals("C")));
        assertEquals(List.of("E"), outline.get(1).getChildren().stream().map(DocumentSymbol::getName).toList());
    }

    @Test
    public void importsAreReadFromOpenDocumentsAndTheirErrorsShownOnTheFirstLine() throws Exception {
        h.initialize();
        Path b = folder.getRoot().toPath().resolve("b.cfr");
        Path a = folder.getRoot().toPath().resolve("a.cfr");
        Files.writeString(b, "abstract Base\n");
        String aText = "//# OPTIONS --import=b.cfr\nY : Base\n";
        h.open(uri(a), aText);
        assertEquals(List.of(), h.nextDiagnostics(uri(a)).getDiagnostics());

        // the open, edited b.cfr is what a.cfr imports, not the file on disk
        h.open(uri(b), "abstract Base\n  [ some Nothing ]\n");
        PublishDiagnosticsParams first = h.nextDiagnostics(uri(a));
        List<Diagnostic> ds = first.getDiagnostics();
        assertEquals(1, ds.size());
        assertEquals(range(0, 0, 0, aText.indexOf('\n')), ds.get(0).getRange());
        assertTrue(ds.get(0).getMessage(), ds.get(0).getMessage().startsWith("in " + b + ":"));
    }

    @Test
    public void errorsOfAModelWithImportsAreOnItsOwnLines() throws Exception {
        h.initialize();
        Path b = folder.getRoot().toPath().resolve("b.cfr");
        Path a = folder.getRoot().toPath().resolve("a.cfr");
        Files.writeString(b, "abstract Base\n  size -> integer\n  [ size >= 0 ]\n");
        // the model is b.cfr then a.cfr, so the error is on its line 6, which is line 3 of a.cfr
        h.open(uri(a), "//# OPTIONS --import=b.cfr\nY : Base\n[ some Nothing ]\n");
        List<Diagnostic> ds = h.nextDiagnostics(uri(a)).getDiagnostics();
        assertEquals(1, ds.size());
        assertEquals(range(2, 7, 2, 14), ds.get(0).getRange());
    }

    @Test
    public void importErrorWithoutAPositionIsOnTheFirstLine() throws Exception {
        h.initialize();
        Path a = folder.getRoot().toPath().resolve("a.cfr");
        h.open(uri(a), "//# OPTIONS --import=missing.cfr\nY\n");
        List<Diagnostic> ds = h.nextDiagnostics(uri(a)).getDiagnostics();
        assertEquals(1, ds.size());
        assertTrue(ds.get(0).getMessage(), ds.get(0).getMessage().contains("does not exist"));
        assertEquals(0, ds.get(0).getRange().getStart().getLine());
    }

    @Test
    public void shutdownThenExitExitsWithZero() throws Exception {
        h.initialize();
        h.server.shutdown().get(10, TimeUnit.SECONDS);
        h.server.exit();
        assertEquals(Integer.valueOf(0), h.exitCode.get(10, TimeUnit.SECONDS));
    }
}
