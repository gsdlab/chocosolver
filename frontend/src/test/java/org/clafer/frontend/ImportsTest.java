package org.clafer.frontend;

import com.sun.net.httpserver.HttpServer;
import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import org.clafer.frontend.ModelFiles.ImportOptions;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

/**
 * The imports of URLs and the restrictions of the imports
 * ({@code --import-root}, {@code --public-imports}, {@code --import-base}),
 * on a local HTTP server: the files, their order and the errors are the
 * compiler's ({@code clafer -m clafer} on the same files, 2026-11-26).
 */
public class ImportsTest {

    @Rule
    public TemporaryFolder folder = new TemporaryFolder();

    private HttpServer server;
    private String u;

    private static final Map<String, String[]> Files_ = Map.ofEntries(
            Map.entry("/m/a.cfr", new String[]{"text/plain", "//# OPTIONS --import=b.cfr\nA\n  [ some B ]\n"}),
            Map.entry("/m/b.cfr", new String[]{"text/plain", "B\n"}),
            Map.entry("/m/sub/c.cfr", new String[]{"text/plain", "//# OPTIONS --import=../b.cfr\nC\n"}),
            Map.entry("/m/nested.cfr", new String[]{"text/plain", "//# OPTIONS --import=sub/c.cfr\nN\n"}),
            Map.entry("/m/html.cfr", new String[]{"text/html; charset=utf-8", "H\n"}),
            Map.entry("/m/svg.cfr", new String[]{"image/svg+xml", "H\n"}),
            Map.entry("/m/noct.cfr", new String[]{null, "NC\n"}),
            Map.entry("/m/page.txt", new String[]{"text/plain", "P\n"}),
            Map.entry("/m/local.cfr", new String[]{"text/plain", "//# OPTIONS --import=/etc/x.cfr\nL\n"}),
            Map.entry("/m/fileurl.cfr", new String[]{"text/plain", "//# OPTIONS --import=file:///tmp/x.cfr\nL\n"}),
            Map.entry("/m/notcfr.cfr", new String[]{"text/plain", "//# OPTIONS --import=b.txt\nL\n"}),
            Map.entry("/m/cyc1.cfr", new String[]{"text/plain", "//# OPTIONS --import=cyc2.cfr\nP\n"}),
            Map.entry("/m/cyc2.cfr", new String[]{"text/plain", "//# OPTIONS --import=cyc1.cfr\nQ\n"}),
            Map.entry("/m/space.cfr", new String[]{"text/plain", "//# OPTIONS --import=\"my b.cfr\"\nS\n"}),
            Map.entry("/m/my b.cfr", new String[]{"text/plain", "MB\n"}),
            Map.entry("/m/opt.cfr", new String[]{"text/plain", "//# OPTIONS -s\nO\n"}),
            Map.entry("/m/", new String[]{"text/plain", "D\n"}));

    @Before
    public void setUp() throws IOException {
        server = HttpServer.create(new InetSocketAddress(InetAddress.getLoopbackAddress(), 0), 0);
        server.createContext("/m/", exchange -> {
            String path = exchange.getRequestURI().getPath();
            String[] file = Files_.get(path);
            byte[] body;
            switch (path) {
                case "/m/redir.cfr" -> redirect(exchange, "/m/page.txt");
                case "/m/redir-ok.cfr" -> redirect(exchange, "b.cfr");
                case "/m/loop.cfr" -> redirect(exchange, "/m/loop.cfr");
                case "/m/big.cfr" -> {
                    exchange.getResponseHeaders().add("Content-Type", "text/plain");
                    exchange.sendResponseHeaders(200, 0);
                    try (OutputStream out = exchange.getResponseBody()) {
                        byte[] chunk = new byte[1 << 16];
                        java.util.Arrays.fill(chunk, (byte) ' ');
                        for (int i = 0; i <= 256; i++) {
                            out.write(chunk);
                        }
                    } catch (IOException e) {
                        // the client stopped reading
                    }
                }
                case "/m/500.cfr" -> exchange.sendResponseHeaders(500, -1);
                default -> {
                    if (file == null) {
                        exchange.sendResponseHeaders(404, -1);
                    } else {
                        body = file[1].getBytes(StandardCharsets.UTF_8);
                        if (file[0] != null) {
                            exchange.getResponseHeaders().add("Content-Type", file[0]);
                        }
                        exchange.sendResponseHeaders(200, body.length);
                        exchange.getResponseBody().write(body);
                    }
                }
            }
            exchange.close();
        });
        server.start();
        u = "http://127.0.0.1:" + server.getAddress().getPort() + "/m/";
    }

    private static void redirect(com.sun.net.httpserver.HttpExchange exchange, String to) throws IOException {
        exchange.getResponseHeaders().add("Location", to);
        exchange.sendResponseHeaders(302, -1);
    }

    @After
    public void tearDown() {
        server.stop(0);
    }

    private String model(String name, String text) throws IOException {
        File f = new File(folder.getRoot(), name);
        f.getParentFile().mkdirs();
        Files.writeString(f.toPath(), text);
        return f.getPath();
    }

    private static ModelFiles read(String path, List<String> imports, ImportOptions restrictions) {
        return ModelFiles.read(path, imports, p -> Optional.empty(), restrictions);
    }

    private static ModelFiles read(String path, String... imports) {
        return read(path, List.of(imports), ImportOptions.Default);
    }

    private static List<String> paths(ModelFiles files) {
        assertEquals(List.of(), files.errors());
        return files.files().stream().map(ModelFiles.ModelFile::path).toList();
    }

    private static String error(ModelFiles files) {
        assertEquals(1, files.errors().size());
        return files.errors().get(0);
    }

    @Test
    public void testUrlImports() throws IOException {
        String x = model("x.cfr", "Xx\n");
        assertEquals(List.of(u + "b.cfr", u + "a.cfr", x), paths(read(x, u + "a.cfr")));
        assertEquals(List.of(u + "b.cfr", u + "sub/c.cfr", u + "nested.cfr", x), paths(read(x, u + "nested.cfr")));
        assertEquals(List.of(u + "noct.cfr", x), paths(read(x, u + "noct.cfr")));
        assertEquals(List.of(u + "my%20b.cfr", u + "space.cfr", x), paths(read(x, u + "space.cfr")));
        // a redirect to a .cfr file
        assertEquals(List.of(u + "redir-ok.cfr", x), paths(read(x, u + "redir-ok.cfr")));
        assertEquals("B\n", read(x, u + "redir-ok.cfr").files().get(0).text());
        // the model's imports are relative to its URL
        ModelFiles a = read(u + "a.cfr");
        assertEquals(List.of(u + "b.cfr", u + "a.cfr"), paths(a));
        ToIr.Result ir = ToIr.toIr(a);
        assertEquals(List.of(), ir.messages());
        assertEquals("//# OPTIONS --explicit\n0..* B 1..1 {}\n0..* A 1..1 {\n  [some B]\n}\n",
                ExplicitPrinter.print(ir.model(), false));
        // a file:// URL is a path
        assertEquals(List.of(x), paths(read("file://" + x)));
    }

    @Test
    public void testUrlErrors() throws IOException {
        String x = model("x.cfr", "Xx\n");
        assertEquals(x + ": cannot read " + u + "html.cfr: it is served as text/html; charset=utf-8, not as a Clafer"
                + " model", error(read(x, u + "html.cfr")));
        assertEquals(x + ": cannot read " + u + "svg.cfr: it is served as image/svg+xml, not as a Clafer model",
                error(read(x, u + "svg.cfr")));
        assertEquals(x + ": cannot read " + u + "redir.cfr: redirected to " + u + "page.txt, which is not a .cfr file",
                error(read(x, u + "redir.cfr")));
        assertEquals(x + ": cannot read " + u + "loop.cfr: TooManyRedirects", error(read(x, u + "loop.cfr")));
        assertEquals(x + ": cannot read " + u + "missing.cfr: HTTP 404 Not Found", error(read(x, u + "missing.cfr")));
        assertEquals(x + ": cannot read " + u + "500.cfr: HTTP 500 Internal Server Error",
                error(read(x, u + "500.cfr")));
        assertEquals(x + ": cannot read " + u + "big.cfr: larger than 16 MB", error(read(x, u + "big.cfr")));
        assertEquals(u + "local.cfr: a model read from a URL cannot import the local file /etc/x.cfr",
                error(read(x, u + "local.cfr")));
        assertEquals(u + "fileurl.cfr: a model read from a URL cannot import the local file file:///tmp/x.cfr",
                error(read(x, u + "fileurl.cfr")));
        assertEquals(u + "notcfr.cfr: the import " + u + "b.txt is not a .cfr file", error(read(x, u + "notcfr.cfr")));
        assertEquals("Import cycle: " + u + "cyc1.cfr imports " + u + "cyc2.cfr imports " + u + "cyc1.cfr",
                error(read(x, u + "cyc1.cfr")));
        assertEquals(u + "opt.cfr: the option -s can only be given in the compiled model or on the command line, not in"
                + " an imported file", error(read(x, u + "opt.cfr")));
        assertEquals(x + ": the import " + u + "page.txt is not a .cfr file", error(read(x, u + "page.txt")));
        assertEquals(x + ": the import " + u + " is not a .cfr file", error(read(x, u)));
        // a URL with a fragment is not a URL, as for the compiler's parseAbsoluteURI
        assertEquals(x + ": the import " + u + "b.cfr#f is not a .cfr file", error(read(x, u + "b.cfr#f")));
        assertEquals(x + ": cannot read http://127.0.0.1:9/a.cfr: does not exist",
                error(read(x, "http://127.0.0.1:9/a.cfr")));
        assertEquals("cannot read " + u + "missing.cfr: HTTP 404 Not Found", error(read(u + "missing.cfr")));
        // the model itself may be anything
        assertEquals(List.of(u + "html.cfr"), paths(read(u + "html.cfr")));
    }

    @Test
    public void testLocalName() {
        assertEquals("a.cfr", ModelFiles.localName(u + "a.cfr"));
        assertEquals("my b.cfr", ModelFiles.localName(u + "my%20b.cfr"));
        assertEquals("model.cfr", ModelFiles.localName(u));
        assertEquals("d/x.cfr", ModelFiles.localName("d/x.cfr"));
    }

    @Test
    public void testPublicImports() throws IOException {
        String x = model("x.cfr", "Xx\n");
        ImportOptions publicOnly = new ImportOptions(List.of(), true, Optional.empty(), true);
        assertEquals(x + ": cannot read " + u + "a.cfr: the host 127.0.0.1 has no public address",
                error(read(x, List.of(u + "a.cfr"), publicOnly)));
        assertEquals("cannot read " + u + "a.cfr: the host 127.0.0.1 has no public address",
                error(read(u + "a.cfr", List.of(), publicOnly)));
        String y = model("root/y.cfr", "Y\n");
        String z = model("z.cfr", "//# OPTIONS --import=root/y.cfr\nZ\n");
        assertEquals(List.of(y, z), paths(read(z, List.of(), publicOnly)));
        for (String address : List.of("0.0.0.0", "10.1.2.3", "100.64.0.1", "127.0.0.1", "169.254.1.1", "172.16.0.1",
                "192.0.0.1", "192.0.2.1", "192.168.1.1", "198.18.0.1", "198.51.100.1", "203.0.113.1", "224.0.0.1",
                "255.255.255.255")) {
            assertFalse(address, Http.isPublic((java.net.Inet4Address) InetAddress.getByName(address)));
        }
        for (String address : List.of("8.8.8.8", "100.128.0.1", "172.32.0.1", "192.0.1.1", "198.20.0.1")) {
            assertTrue(address, Http.isPublic((java.net.Inet4Address) InetAddress.getByName(address)));
        }
    }

    @Test
    public void testImportBase() throws IOException {
        String rel = model("rel.cfr", "//# OPTIONS --import=b.cfr\nR\n");
        assertEquals(List.of(u + "b.cfr", rel), paths(read(rel, List.of(),
                new ImportOptions(List.of(), false, Optional.of(u), true))));
        assertEquals(rel + ": cannot read " + u + "sub/b.cfr: HTTP 404 Not Found", error(read(rel, List.of(),
                new ImportOptions(List.of(), false, Optional.of(u + "sub/c.cfr"), true))));
        String abs = model("abs.cfr", "//# OPTIONS --import=/etc/x.cfr\nR\n");
        assertEquals(abs + ": a model read from a URL cannot import the local file /etc/x.cfr", error(read(abs,
                List.of(), new ImportOptions(List.of(), false, Optional.of(u), true))));
        // the imports of the command line are relative to the working folder
        String x = model("x.cfr", "Xx\n");
        assertEquals(List.of(x, u + "b.cfr", rel), paths(read(rel, List.of(x),
                new ImportOptions(List.of(), false, Optional.of(u), true))));
    }

    @Test
    public void testImportRoots() throws IOException {
        String root = new File(folder.getRoot(), "root").getPath();
        String x = model("x.cfr", "Xx\n");
        String y = model("root/y.cfr", "Y\n");
        String w = model("root/w.cfr", "//# OPTIONS --import=../x.cfr\nW\n");
        String z = model("z.cfr", "//# OPTIONS --import=root/y.cfr\nZ\n");
        Files.createSymbolicLink(Path.of(root, "link.cfr"), Path.of("../x.cfr"));
        String v = model("root/v.cfr", "//# OPTIONS --import=link.cfr\nV\n");
        ImportOptions inRoot = new ImportOptions(List.of(root), false, Optional.empty(), true);
        // the model need not be inside
        assertEquals(List.of(y, z), paths(read(z, List.of(), inRoot)));
        assertEquals(w + ": the import " + root + "/../x.cfr is outside the folders from which files can be imported",
                error(read(w, List.of(), inRoot)));
        assertEquals(v + ": the import " + root + "/link.cfr is outside the folders from which files can be imported",
                error(read(v, List.of(), inRoot)));
        assertEquals(y + ": the import " + x + " is outside the folders from which files can be imported",
                error(read(y, List.of(x), inRoot)));
        assertEquals(List.of(root + "/../x.cfr", w), paths(read(w, List.of(),
                new ImportOptions(List.of(root, folder.getRoot().getPath()), false, Optional.empty(), true))));
        assertEquals(z + ": the import " + y + " is outside the folders from which files can be imported",
                error(read(z, List.of(), new ImportOptions(List.of(folder.getRoot() + "/none"), false,
                        Optional.empty(), true))));
        // the roots do not restrict URLs
        assertEquals(List.of(u + "b.cfr", y), paths(read(y, List.of(u + "b.cfr"), inRoot)));
    }

    @Test
    public void testLocalOnly() throws IOException {
        String x = model("x.cfr", "Xx\n");
        ModelFiles files = ModelFiles.read(x, List.of(u + "b.cfr"), p -> Optional.empty());
        assertTrue(files.importsUrl());
        assertTrue(ModelFiles.isUrl(u + "b.cfr"));
        assertFalse(ModelFiles.isUrl(u + "b.cfr#f"));
        assertEquals(URI.create(u + "b.cfr"), ModelFiles.location(u + "b.cfr").url().orElseThrow());
    }
}
