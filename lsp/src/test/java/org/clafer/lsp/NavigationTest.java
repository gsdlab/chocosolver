package org.clafer.lsp;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import org.eclipse.lsp4j.Location;
import org.eclipse.lsp4j.Position;
import org.eclipse.lsp4j.Range;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

/**
 * Go to definition, find references and hover, through LSP4J's launchers.
 */
public class NavigationTest {

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

    private static Location at(String uri, int line, int character, int endCharacter) {
        return new Location(uri, new Range(new Position(line, character), new Position(line, endCharacter)));
    }

    @Test
    public void definitionOfAnInheritedChildInAConstraint() throws Exception {
        openModel();
        assertEquals(List.of(at(Uri, 1, 2, 6)), h.definition(Uri, 6, 5));
        // the cursor right after the name is on it
        assertEquals(List.of(at(Uri, 1, 2, 6)), h.definition(Uri, 6, 8));
    }

    @Test
    public void definitionOfASuperAndAReferenceTarget() throws Exception {
        openModel();
        assertEquals(List.of(at(Uri, 0, 9, 15)), h.definition(Uri, 4, 10));
        assertEquals(List.of(at(Uri, 2, 9, 13)), h.definition(Uri, 5, 11));
        assertEquals(List.of(at(Uri, 8, 5, 10)), h.definition(Uri, 9, 6));
    }

    @Test
    public void definitionOfANameAfterADotIsTheChildOfTheTarget() throws Exception {
        openModel();
        // x.price: price is the child of Comp, the target of apps
        assertEquals(List.of(at(Uri, 3, 2, 7)), h.definition(Uri, 7, 22));
    }

    @Test
    public void definitionOfLocals() throws Exception {
        openModel();
        assertEquals(List.of(at(Uri, 7, 8, 9)), h.definition(Uri, 7, 19));
        assertEquals(List.of(at(Uri, 11, 6, 7)), h.definition(Uri, 11, 24));
        assertEquals(List.of(at(Uri, 8, 13, 16)), h.definition(Uri, 10, 6));
    }

    @Test
    public void noDefinitionOutsideANameOrOfASpecialName() throws Exception {
        openModel();
        assertEquals(List.of(), h.definition(Uri, 6, 2));
        assertEquals(List.of(), h.definition(Uri, 1, 13));
    }

    @Test
    public void referencesOfAClafer() throws Exception {
        openModel();
        List<Location> withDeclaration = List.of(at(Uri, 1, 2, 6), at(Uri, 6, 4, 8), at(Uri, 11, 16, 20));
        assertEquals(withDeclaration, h.references(Uri, 1, 3, true));
        assertEquals(withDeclaration, h.references(Uri, 6, 4, true));
        assertEquals(withDeclaration.subList(1, 3), h.references(Uri, 1, 3, false));
        assertEquals(List.of(at(Uri, 2, 9, 13), at(Uri, 5, 10, 14)), h.references(Uri, 2, 10, true));
    }

    @Test
    public void referencesOfALocal() throws Exception {
        openModel();
        assertEquals(List.of(at(Uri, 7, 8, 9), at(Uri, 7, 19, 20)), h.references(Uri, 7, 8, true));
    }

    @Test
    public void hoverShowsTheClaferAsTheExplicitModelWritesIt() throws Exception {
        openModel();
        String phone = h.hover(Uri, 11, 12);
        assertTrue(phone, phone.contains("0..* Phone : Device 1..1"));
        assertTrue(phone, phone.contains("\nclafer"));
        String cost = h.hover(Uri, 6, 4);
        assertTrue(cost, cost.contains("abstract") == false && cost.contains("0..* cost -> int 1..1"));
        assertTrue(cost, cost.contains("clafer in Device"));
        String device = h.hover(Uri, 4, 9);
        assertTrue(device, device.contains("abstract 0..* Device 0..*"));
        String c = h.hover(Uri, 10, 2);
        assertTrue(c, c.contains("0..* c -> Color 1..1"));
        String red = h.hover(Uri, 10, 6);
        assertTrue(red, red.contains("red : Color") && red.contains("literal of the enumeration Color"));
        String color = h.hover(Uri, 9, 6);
        assertTrue(color, color.contains("enum Color = red | green"));
        String x = h.hover(Uri, 7, 19);
        assertTrue(x, x.contains("x : apps") && x.contains("local"));
        assertNull(h.hover(Uri, 6, 2));
    }

    @Test
    public void definitionAndReferencesAcrossAnImport() throws Exception {
        Path b = folder.getRoot().toPath().resolve("b.cfr");
        Path a = folder.getRoot().toPath().resolve("a.cfr");
        Files.writeString(b, "abstract Base\n  size -> integer\n");
        String aUri = a.toUri().toString();
        String bUri = b.toUri().toString();
        h.open(aUri, "//# OPTIONS --import=b.cfr\nY : Base\n[ Y.size > 2 ]\n");
        assertEquals(List.of(), h.nextDiagnostics(aUri).getDiagnostics());
        assertEquals(List.of(at(bUri, 0, 9, 13)), h.definition(aUri, 1, 5));
        assertEquals(List.of(at(bUri, 1, 2, 6)), h.definition(aUri, 2, 5));
        assertEquals(List.of(at(bUri, 0, 9, 13), at(aUri, 1, 4, 8)), h.references(aUri, 1, 5, true));
        String base = h.hover(aUri, 1, 5);
        assertTrue(base, base.contains("declared in `" + b + ":1`"));
    }

    @Test
    public void aModelWithASyntaxErrorStillHasItsLocals() throws Exception {
        h.open(Uri, "A\n  B\n[ all x : A | x.B ]\nC ->\n");
        assertEquals(1, h.nextDiagnostics(Uri).getDiagnostics().size());
        assertEquals(List.of(), h.definition(Uri, 2, 16));
        assertEquals(List.of(at(Uri, 2, 6, 7)), h.definition(Uri, 2, 14));
    }
}
