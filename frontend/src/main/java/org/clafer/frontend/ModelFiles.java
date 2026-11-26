package org.clafer.frontend;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.function.Function;

/**
 * The files of a model: the model and the files it imports, transitively,
 * with {@code --import=FILE} in their {@code //# OPTIONS} lines, as the
 * compiler reads them ({@code Import.readModel}): local files and http or
 * https URLs ({@link Http}). An import is relative to the folder or the URL of
 * the importing file; the imported files come before the files that import
 * them, each once, and the model last. The model is their concatenation, and
 * a line of it is shown in its file ({@link #position}). The web tools'
 * restrictions of the imports are {@link ImportOptions}.
 *
 * @param files the files, in order, the model last
 * @param options the options of all the files, combined
 * @param errors the compiler's errors of reading the files and combining
 * their options; the files are empty if there is one
 */
public record ModelFiles(List<ModelFile> files, ModelOptions options, List<String> errors) {

    /**
     * @param path the path or URL as the messages show it
     * @param text the text, which ends with a newline
     */
    public record ModelFile(String path, String text) {
    }

    /**
     * A model in one file, whose imports are not read: its text and options.
     *
     * @param path the path that the messages show, or empty
     */
    public static ModelFiles of(String path, String text) {
        return new ModelFiles(List.of(new ModelFile(path, withNewline(text))), ModelOptions.of(text), List.of());
    }

    /**
     * @return the text of the model, the files in order
     */
    public String text() {
        StringBuilder sb = new StringBuilder();
        for (ModelFile f : files) {
            sb.append(f.text());
        }
        return sb.toString();
    }

    /**
     * The restrictions of the imports, which only the command line gives, as
     * the web tools give them to the compiler:
     *
     * @param roots {@code --import-root}: a local import must be inside one
     * of these folders, after resolving {@code ..} and symbolic links; any
     * local file without them
     * @param publicOnly {@code --public-imports}: URLs only from public
     * addresses, without a proxy
     * @param base {@code --import-base}: the URL from which the model's text
     * was read, which its imports are relative to, so they can only be URLs
     * @param readUrls whether URLs are downloaded; if not, an import of a URL
     * is an error ({@link #importsUrl}), e.g., in an editor, which analyzes the
     * model on every change
     */
    public record ImportOptions(List<String> roots, boolean publicOnly, Optional<String> base, boolean readUrls) {

        public ImportOptions {
            roots = List.copyOf(roots);
        }

        /**
         * No restrictions; URLs are read.
         */
        public static final ImportOptions Default = new ImportOptions(List.of(), false, Optional.empty(), true);

        /**
         * No restrictions, and only local files.
         */
        public static final ImportOptions LocalOnly = new ImportOptions(List.of(), false, Optional.empty(), false);
    }

    /**
     * The name of the standard input, as the messages show it.
     */
    public static final String Stdin = "<stdin>";

    /**
     * Reads a model and the files it imports, local files and URLs.
     *
     * @param path the model's file or URL, as the messages show it
     * @param commandLineImports the imports given on the command line,
     * relative to the working folder, which come first
     */
    public static ModelFiles read(String path, List<String> commandLineImports) {
        return read(path, commandLineImports, p -> Optional.empty(), ImportOptions.Default);
    }

    /**
     * Reads a model and the local files it imports, taking the text of a
     * file from {@code open} where it has one, e.g., a document open in an
     * editor, and from disk otherwise. An import of a URL is an error
     * ({@link #importsUrl}).
     *
     * @param path the model's file, as the messages show it
     * @param commandLineImports the imports given on the command line
     * @param open the text of a file, by its path as the model names it, if
     * it is not to be read from disk
     */
    public static ModelFiles read(String path, List<String> commandLineImports,
            Function<String, Optional<String>> open) {
        return read(path, commandLineImports, open, ImportOptions.LocalOnly);
    }

    /**
     * Reads a model and the files it imports, as the compiler does
     * ({@code Import.readModel}). The model is a file, a URL, or the
     * standard input ({@link #Stdin}, whose text {@code open} gives). The
     * imports of the command line are relative to the working folder, and
     * those of a file to its folder or URL, or, for the model, to
     * {@code --import-base}. A file read from a URL may only import URLs.
     *
     * @param path the model's file or URL, as the messages show it
     * @param commandLineImports the imports given on the command line
     * @param open the text of a file, by its path, if it is not to be read
     * @param restrictions the restrictions of the imports
     */
    public static ModelFiles read(String path, List<String> commandLineImports,
            Function<String, Optional<String>> open, ImportOptions restrictions) {
        Reader reader = new Reader(open, restrictions);
        try {
            boolean stdin = path.equals(Stdin);
            Location rootLocation = location(path);
            Location base = restrictions.base().map(ModelFiles::location).orElse(rootLocation);
            String text = withNewline(reader.fetch(false, "", rootLocation));
            ModelOptions rootOptions = ModelOptions.of(text);
            List<Location> rootImports = new ArrayList<>();
            for (String name : rootOptions.imports()) {
                rootImports.add(importLocation(path, base, name));
            }
            // the imports of the command line and of the model, imported by the model
            List<String> stackKeys = stdin ? List.of() : List.of(key(base));
            List<String> stackNames = stdin ? List.of() : List.of(path);
            for (String name : commandLineImports) {
                reader.visit(stackKeys, stackNames, location(name));
            }
            for (Location l : rootImports) {
                reader.visit(stackKeys, stackNames, l);
            }
            List<String> errors = new ArrayList<>();
            ModelOptions options = ModelOptions.combine(reader.paths, reader.options, path, rootOptions, errors);
            if (!errors.isEmpty()) {
                return new ModelFiles(List.of(), rootOptions, errors);
            }
            List<ModelFile> files = new ArrayList<>(reader.files);
            files.add(new ModelFile(stdin ? Stdin : rootLocation.show(), text));
            return new ModelFiles(List.copyOf(files), options, List.of());
        } catch (ImportError e) {
            return new ModelFiles(List.of(), ModelOptions.Defaults, List.of(e.getMessage()));
        }
    }

    /**
     * The name of the model's file for its outputs: its path, or, for a URL,
     * the last segment of its path ({@code model.cfr} if it has none), in the
     * working folder.
     */
    public static String localName(String path) {
        Location l = location(path);
        if (l.url().isEmpty()) {
            return l.path();
        }
        String p = Http.path(l.url().get());
        String name = p.substring(p.lastIndexOf('/') + 1);
        return name.isEmpty() ? "model.cfr" : name;
    }

    /**
     * Where a file of a model is: a local path, or an http or https URL.
     */
    record Location(String path, Optional<URI> url) {

        /**
         * @return the location as the messages show it
         */
        String show() {
            return url.map(URI::toString).orElse(path);
        }

        /**
         * @return the path that must name a .cfr file, unescaped for a URL
         */
        String modelPath() {
            return url.map(Http::path).orElse(path);
        }
    }

    /**
     * The location of a name ({@code Import.location}): an absolute
     * {@code http} or {@code https} URI without a fragment, otherwise a local
     * path, a {@code file://} URL being its path.
     */
    static Location location(String name) {
        try {
            URI uri = new URI(name);
            String scheme = uri.getScheme();
            if (uri.isAbsolute() && scheme != null && (scheme.equalsIgnoreCase("http")
                    || scheme.equalsIgnoreCase("https")) && uri.getRawFragment() == null && !uri.isOpaque()) {
                return new Location(name, Optional.of(uri));
            }
        } catch (URISyntaxException e) {
            // a local path
        }
        if (name.startsWith("file://")) {
            return new Location(unescape(name.substring("file://".length())), Optional.empty());
        }
        return new Location(name, Optional.empty());
    }

    private static String unescape(String s) {
        try {
            return java.net.URLDecoder.decode(s.replace("+", "%2B"), StandardCharsets.UTF_8);
        } catch (IllegalArgumentException e) {
            return s;
        }
    }

    /**
     * The location of an import, relative to the importing file
     * ({@code Import.importLocation}): to its folder, or to its URL, from
     * which only URLs can be imported.
     *
     * @param importerName the importing file, as the messages show it
     * @param importer its location
     */
    static Location importLocation(String importerName, Location importer, String name) {
        Location l = location(name);
        if (l.url().isPresent()) {
            return l;
        }
        if (importer.url().isEmpty()) {
            return new Location(importLocation(importer.path(), l.path()), Optional.empty());
        }
        if (name.startsWith("file://") || Path.of(l.path()).isAbsolute()) {
            throw new ImportError(ModelOptions.inFile(importerName,
                    "a model read from a URL cannot import the local file " + name));
        }
        try {
            return new Location(name, Optional.of(importer.url().get().resolve(new URI(Http.escape(l.path())))));
        } catch (URISyntaxException | IllegalArgumentException e) {
            throw new ImportError(ModelOptions.inFile(importerName,
                    "the import " + name + " is not a valid relative URL"));
        }
    }

    private static final class ImportError extends RuntimeException {

        ImportError(String message) {
            super(message, null, false, false);
        }
    }

    private static final class Reader {

        final List<String> done = new ArrayList<>();
        final List<ModelFile> files = new ArrayList<>();
        final List<String> paths = new ArrayList<>();
        final List<ModelOptions> options = new ArrayList<>();
        final Function<String, Optional<String>> open;
        final ImportOptions restrictions;
        final List<Path> roots = new ArrayList<>();

        Reader(Function<String, Optional<String>> open, ImportOptions restrictions) {
            this.open = open;
            this.restrictions = restrictions;
            for (String r : restrictions.roots()) {
                roots.add(canonical(r));
            }
        }

        /**
         * Reads an import and its imports, unless it was read; the stack has
         * the importing files, the last first, and is empty for the imports
         * of the command line of a model read from the standard input.
         */
        void visit(List<String> stackKeys, List<String> stackNames, Location loc) {
            String importer = stackNames.isEmpty() ? "the command line" : stackNames.get(0);
            String name = loc.show();
            if (loc.url().isPresent() && !restrictions.readUrls()) {
                throw new ImportError(ModelOptions.inFile(importer, "the import " + name + UrlImport));
            }
            // before anything is known about the file, e.g., whether it exists
            if (!isModelPath(loc.modelPath())) {
                throw new ImportError(ModelOptions.inFile(importer, "the import " + name + " is not a .cfr file"));
            }
            if (loc.url().isEmpty() && !roots.isEmpty()) {
                Path real = canonical(loc.path());
                if (roots.stream().noneMatch(real::startsWith)) {
                    throw new ImportError(ModelOptions.inFile(importer, "the import " + loc.path()
                            + " is outside the folders from which files can be imported"));
                }
            }
            String k = key(loc);
            int at = stackKeys.indexOf(k);
            if (at >= 0) {
                // the importing files from the one that imports this one again
                List<String> cycle = new ArrayList<>();
                cycle.add(name);
                cycle.addAll(stackNames.subList(0, at));
                cycle.add(name);
                java.util.Collections.reverse(cycle);
                throw new ImportError("Import cycle: " + String.join(" imports ", cycle));
            }
            if (done.contains(k)) {
                return;
            }
            String text = withNewline(fetch(true, importer, loc));
            ModelOptions o = ModelOptions.of(text);
            List<Location> imports = new ArrayList<>();
            for (String i : o.imports()) {
                imports.add(importLocation(name, loc, i));
            }
            List<String> keys = new ArrayList<>();
            keys.add(k);
            keys.addAll(stackKeys);
            List<String> names = new ArrayList<>();
            names.add(name);
            names.addAll(stackNames);
            for (Location i : imports) {
                visit(keys, names, i);
            }
            done.add(k);
            files.add(new ModelFile(name, text));
            paths.add(name);
            options.add(o);
        }

        String fetch(boolean isImport, String importer, Location loc) {
            Optional<String> text = open.apply(loc.show());
            if (text.isPresent()) {
                return text.get();
            }
            if (loc.url().isPresent()) {
                try {
                    return Http.get(loc.url().get(), isImport, restrictions.publicOnly());
                } catch (Http.Failure e) {
                    throw new ImportError(ModelOptions.inFile(importer, "cannot read " + loc.show() + ": "
                            + e.getMessage()));
                }
            }
            String path = loc.path();
            Path p = Path.of(path);
            if (!Files.isRegularFile(p)) {
                throw new ImportError(ModelOptions.inFile(importer, "the file " + path + " does not exist"));
            }
            try {
                return Files.readString(p, StandardCharsets.UTF_8);
            } catch (IOException e) {
                throw new ImportError(ModelOptions.inFile(importer, "cannot read " + path + ": " + e.getMessage()));
            }
        }
    }

    /**
     * {@code System.Directory.canonicalizePath}: the absolute path, with
     * {@code ..} and the symbolic links of the part that exists resolved.
     */
    static Path canonical(String path) {
        Path p = Path.of(path).toAbsolutePath().normalize();
        Path existing = p;
        while (existing != null && !Files.exists(existing)) {
            existing = existing.getParent();
        }
        if (existing == null) {
            return p;
        }
        try {
            return existing.toRealPath().resolve(existing.relativize(p)).normalize();
        } catch (IOException e) {
            return p;
        }
    }

    /**
     * The end of the error of an import of a URL that is not read.
     */
    private static final String UrlImport = " is a URL, which is not read here";

    /**
     * @return whether the model could not be read because it imports a URL,
     * which was not to be read ({@link ImportOptions#readUrls})
     */
    public boolean importsUrl() {
        return errors.stream().anyMatch(e -> e.endsWith(UrlImport));
    }

    /**
     * @return whether an import names a URL
     */
    public static boolean isUrl(String name) {
        return location(name).url().isPresent();
    }

    /**
     * Whether a path names a Clafer model: it ends with {@code .cfr}.
     */
    static boolean isModelPath(String path) {
        return path.endsWith(".cfr");
    }

    /**
     * The location of an import, relative to the folder of the importing
     * file: {@code normalise (takeDirectory importer </> path)}.
     */
    static String importLocation(String importer, String name) {
        String path = name;
        if (Path.of(path).isAbsolute()) {
            return normalise(path);
        }
        int slash = importer.lastIndexOf('/');
        String dir = slash < 0 ? "." : importer.substring(0, slash == 0 ? 1 : slash);
        return normalise(dir.equals("/") ? "/" + path : dir + "/" + path);
    }

    /**
     * {@code System.FilePath.normalise}: no {@code ./} segments and no
     * repeated slashes; {@code ..} is kept.
     */
    static String normalise(String path) {
        boolean absolute = path.startsWith("/");
        List<String> parts = new ArrayList<>();
        for (String part : path.split("/+")) {
            if (!part.isEmpty() && !part.equals(".")) {
                parts.add(part);
            }
        }
        String joined = String.join("/", parts);
        return absolute ? "/" + joined : joined.isEmpty() ? "." : joined;
    }

    /**
     * What identifies a file: its real path, if it exists, or its URL.
     */
    private static String key(Location l) {
        if (l.url().isPresent()) {
            return l.show();
        }
        try {
            Path p = Path.of(l.path());
            return Files.exists(p) ? p.toRealPath().toString() : l.path();
        } catch (IOException | java.nio.file.InvalidPathException e) {
            return l.path();
        }
    }

    private static String withNewline(String s) {
        return s.isEmpty() || s.endsWith("\n") ? s : s + "\n";
    }

    /**
     * @param line a line of the model, from 1
     * @return the path of its file and its line in it
     */
    public Position position(int line) {
        int start = 1;
        for (int i = 0; i < files.size(); i++) {
            ModelFile f = files.get(i);
            int size = (int) f.text().chars().filter(ch -> ch == '\n').count();
            if (i == files.size() - 1 || line < start + size) {
                return new Position(f.path(), line - start + 1);
            }
            start += size;
        }
        return new Position("", line);
    }

    /**
     * A line in a file.
     */
    public record Position(String path, int line) {
    }

    /**
     * @return the diagnostic at its position in its file, e.g.,
     * {@code model.cfr:3:5: message}, or without a path if the model has no
     * name
     */
    public String format(Diagnostic d) {
        Position p = position(d.span().line());
        String at = p.line() + ":" + d.span().column() + ": " + d.message();
        return p.path().isEmpty() ? at : p.path() + ":" + at;
    }
}
