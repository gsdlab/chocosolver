package org.clafer.frontend;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

/**
 * The files of a model: the model and the files it imports, transitively,
 * with {@code --import=FILE} in their {@code //# OPTIONS} lines, as the
 * compiler reads them ({@code Import.readModel}), for local files. An import
 * is relative to the folder of the importing file; the imported files come
 * before the files that import them, each once, and the model last. The model
 * is their concatenation, and a line of it is shown in its file
 * ({@link #position}). URLs are not read yet: an import of one is an error.
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
     * Reads a model and the files it imports.
     *
     * @param path the model's file, as the messages show it
     * @param commandLineImports the imports given on the command line,
     * relative to the working folder, which come first
     */
    public static ModelFiles read(String path, List<String> commandLineImports) {
        Reader reader = new Reader();
        try {
            String rootKey = key(path);
            String text = withNewline(reader.fetch("", path));
            ModelOptions rootOptions = ModelOptions.of(text);
            // the imports of the command line and of the model, imported by the model
            List<String> stackKeys = List.of(rootKey);
            List<String> stackNames = List.of(path);
            for (String name : commandLineImports) {
                reader.visit(stackKeys, stackNames, name);
            }
            for (String name : rootOptions.imports()) {
                reader.visit(stackKeys, stackNames, importLocation(path, name));
            }
            List<String> errors = new ArrayList<>();
            ModelOptions options = ModelOptions.combine(reader.paths, reader.options, path, rootOptions, errors);
            if (!errors.isEmpty()) {
                return new ModelFiles(List.of(), rootOptions, errors);
            }
            List<ModelFile> files = new ArrayList<>(reader.files);
            files.add(new ModelFile(path, text));
            return new ModelFiles(List.copyOf(files), options, List.of());
        } catch (ImportError e) {
            return new ModelFiles(List.of(), ModelOptions.Defaults, List.of(e.getMessage()));
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

        /**
         * Reads an import and its imports, unless it was read; the stack has
         * the importing files, the last first.
         */
        void visit(List<String> stackKeys, List<String> stackNames, String name) {
            String importer = stackNames.get(0);
            if (isUrl(name)) {
                throw new ImportError(ModelOptions.inFile(importer, "the import " + name + UrlImport));
            }
            if (!name.endsWith(".cfr")) {
                throw new ImportError(ModelOptions.inFile(importer, "the import " + name + " is not a .cfr file"));
            }
            String k = key(name);
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
            String text = withNewline(fetch(importer, name));
            ModelOptions o = ModelOptions.of(text);
            List<String> keys = new ArrayList<>();
            keys.add(k);
            keys.addAll(stackKeys);
            List<String> names = new ArrayList<>();
            names.add(name);
            names.addAll(stackNames);
            for (String i : o.imports()) {
                visit(keys, names, importLocation(name, i));
            }
            done.add(k);
            files.add(new ModelFile(name, text));
            paths.add(name);
            options.add(o);
        }

        String fetch(String importer, String path) {
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
     * The end of the error of an import of a URL.
     */
    private static final String UrlImport = " is a URL, which the Java frontend does not read yet";

    /**
     * @return whether the model could not be read because it imports a URL,
     * which the compiler reads and the Java frontend does not
     */
    public boolean importsUrl() {
        return errors.stream().anyMatch(e -> e.endsWith(UrlImport));
    }

    /**
     * @return whether an import names a URL
     */
    public static boolean isUrl(String name) {
        return name.startsWith("http://") || name.startsWith("https://");
    }

    /**
     * The location of an import, relative to the folder of the importing
     * file: {@code normalise (takeDirectory importer </> path)}.
     */
    static String importLocation(String importer, String name) {
        if (isUrl(name)) {
            return name;
        }
        String path = name.startsWith("file://") ? name.substring("file://".length()) : name;
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
     * What identifies a file: its real path, if it exists.
     */
    private static String key(String path) {
        try {
            Path p = Path.of(path);
            return Files.exists(p) ? p.toRealPath().toString() : path;
        } catch (IOException e) {
            return path;
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
