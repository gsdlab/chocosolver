package org.clafer.lsp;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assume.assumeTrue;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Stream;
import org.clafer.frontend.ModelFiles;
import org.clafer.frontend.SourceSpan;
import org.clafer.frontend.Symbols.ClaferSymbol;
import org.clafer.frontend.ToIr;
import org.eclipse.lsp4j.Location;
import org.eclipse.lsp4j.Position;
import org.junit.Test;

/**
 * The table of names on clafer's positive models (at {@code -Dclafer.dir}):
 * each name the table records is written there as the name of its clafer,
 * and the server's go to definition at each name in the model's own file
 * goes to that clafer's name.
 */
public class NamesConformanceTest {

    private static List<Path> models() throws IOException {
        Path dir = Path.of(System.getProperty("clafer.dir", "../../clafer"), "test", "positive");
        assumeTrue("no clafer checkout at " + dir, Files.isDirectory(dir));
        try (Stream<Path> s = Files.list(dir)) {
            return s.filter(p -> p.toString().endsWith(".cfr") && !p.toString().endsWith(".des.cfr")
                    && !p.toString().endsWith(".core.cfr")).sorted().toList();
        }
    }

    private static String textAt(Positions p, SourceSpan s) {
        return p.text(p.toLsp(s));
    }

    @Test
    public void everyNameIsWrittenAsItsClaferAndGoesToIt() throws Exception {
        int models = 0, uses = 0, checked = 0;
        List<String> failures = new ArrayList<>();
        for (Path model : models()) {
            String path = model.toAbsolutePath().normalize().toString();
            ModelFiles files = ModelFiles.read(path, List.of());
            ToIr.Result r = ToIr.toIr(files);
            if (r.names().clafers().isEmpty()) {
                continue;
            }
            models++;
            Positions all = new Positions(files.text());
            String text = Files.readString(model);
            Analysis a = Analysis.of(Path.of(path).toUri().toString(), path, text, p -> Optional.empty());
            // the first line of the model's own file, after its imports
            int start = 1;
            for (ModelFiles.ModelFile f : files.files().subList(0, files.files().size() - 1)) {
                start += (int) f.text().chars().filter(ch -> ch == '\n').count();
            }
            for (var e : r.names().uses().entrySet()) {
                uses++;
                ClaferSymbol c = e.getValue();
                String written = textAt(all, e.getKey());
                if (!written.equals(c.name())) {
                    failures.add(model.getFileName() + " " + e.getKey() + ": '" + written + "' for " + c.uid());
                    continue;
                }
                if (!textAt(all, c.nameSpan()).equals(c.name())) {
                    failures.add(model.getFileName() + " declaration of " + c.uid() + " at " + c.nameSpan());
                }
                SourceSpan s = e.getKey();
                if (s.line() >= start && files.position(s.line()).path().equals(path)) {
                    checked++;
                    Position at = a.positions().toLsp(s.line() - start + 1, s.column());
                    Optional<Location> d = a.index().definition(at);
                    ModelFiles.Position declared = files.position(c.nameSpan().line());
                    boolean ok = d.isPresent() && d.get().getRange().getStart().getLine() == declared.line() - 1
                            && Path.of(java.net.URI.create(d.get().getUri())).toString().equals(declared.path());
                    if (!ok) {
                        failures.add(model.getFileName() + " definition at " + s + " of " + c.uid() + ": " + d);
                    }
                }
            }
        }
        assertEquals(String.join("\n", failures), 0, failures.size());
        assertTrue("models " + models, models > 150);
        assertTrue("uses " + uses, uses > 1000);
        System.out.println("NamesConformanceTest: " + models + " models, " + uses + " names, " + checked
                + " definitions checked");
    }
}
