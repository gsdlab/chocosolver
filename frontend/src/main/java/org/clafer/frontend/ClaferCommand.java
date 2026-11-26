package org.clafer.frontend;

import java.io.IOException;
import java.io.InputStream;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.OptionalInt;
import java.util.OptionalLong;
import java.util.Set;
import java.util.TreeMap;
import java.util.function.Function;

/**
 * {@code claferc}: the Clafer compiler's {@code clafer -m clafer} in Java. It
 * prints the rich explicit model ({@code P.des.cfr}) or, with {@code --core},
 * the explicit core model ({@code P.core.cfr}, with {@code --record-provenance}
 * its provenance comments), and with {@code --meta-data} the explicit model's
 * scope file and the map of names ({@code P.cfr-map}), with the compiler's
 * file names, statistics and errors.
 * <p>
 * Everything else is forwarded to {@code clafer} on the path, with the same
 * arguments: the other modes ({@code -m alloy}, the default, {@code -m choco},
 * {@code json}, {@code html}, {@code graph}, {@code cvlgraph}), the options
 * that the Java side does not implement (e.g., {@code -i}, {@code -v},
 * {@code -c}, {@code --check-afm}, {@code -l}), and any argument that it
 * does not read, so that
 * the compiler gives its own message for it. The executable is
 * {@code -Dclafer.executable}, {@code clafer} by default.
 */
public final class ClaferCommand {

    private final InputStream in;
    private final PrintStream out;
    private final String executable;

    /**
     * @param in the standard input, which holds the model if no file is given
     * @param out the standard output
     * @param executable the compiler that the other modes are forwarded to
     */
    public ClaferCommand(InputStream in, PrintStream out, String executable) {
        this.in = in;
        this.out = out;
        this.executable = executable;
    }

    public static void main(String[] args) {
        PrintStream out = new PrintStream(new java.io.FileOutputStream(java.io.FileDescriptor.out), false,
                StandardCharsets.UTF_8);
        int code = new ClaferCommand(System.in, out, System.getProperty("clafer.executable", "clafer"))
                .run(List.of(args));
        out.flush();
        System.exit(code);
    }

    // ------------------------------------------------------------------
    // The arguments, as the compiler's cmdargs reads them
    // ------------------------------------------------------------------

    /**
     * The arguments of a command line or of an {@code //# OPTIONS} line, as
     * far as the Java side reads them.
     *
     * @param forward why the arguments are forwarded to the compiler, if
     * they are
     */
    record Args(List<String> modes, boolean console, boolean skipResolver, boolean explicit, boolean core,
            boolean provenance, boolean noStats, boolean metaData, boolean noLayout, boolean checks,
            OptionalInt traceLength, Optional<Boolean> simpleScopes, OptionalLong minInt, OptionalLong maxInt,
            Optional<Integer> decimalDigits, List<String> imports, List<String> files, boolean help,
            boolean version, List<String> importRoots, boolean publicImports, Optional<String> importBase,
            Optional<String> forward) {
    }

    /**
     * The flags without a value that the Java side reads, by their long and
     * short names.
     */
    private static final Map<String, String> Flags = Map.ofEntries(
            Map.entry("--console-output", "o"), Map.entry("-o", "o"),
            Map.entry("--skip-resolver", "f"), Map.entry("-f", "f"),
            Map.entry("--explicit", "explicit"), Map.entry("--core", "core"),
            Map.entry("--record-provenance", "provenance"),
            Map.entry("--keep-unused", "k"), Map.entry("-k", "k"),
            Map.entry("--no-stats", "s"), Map.entry("-s", "s"),
            Map.entry("--meta-data", "meta"), Map.entry("--public-imports", "public"),
            Map.entry("--no-layout", "l"), Map.entry("-l", "l"),
            Map.entry("--check-duplicates", "c"), Map.entry("-c", "c"),
            Map.entry("--check-afm", "afm"), Map.entry("--afm", "afm"),
            Map.entry("--help", "help"), Map.entry("-?", "help"),
            Map.entry("--version", "version"), Map.entry("-V", "version"));

    /**
     * The options with a value that the Java side reads.
     */
    private static final Map<String, String> Valued = Map.ofEntries(
            Map.entry("--mode", "m"), Map.entry("-m", "m"),
            Map.entry("--tl", "tl"), Map.entry("--trace-len", "tl"),
            Map.entry("--ss", "ss"), Map.entry("--scope-strategy", "ss"),
            Map.entry("--minint", "minint"), Map.entry("--maxint", "maxint"),
            Map.entry("--decimal-digits", "digits"), Map.entry("--import", "import"),
            Map.entry("--import-root", "root"), Map.entry("--import-base", "base"),
            Map.entry("--timeout-analysis", "timeout"));

    /**
     * Reads the arguments. An argument that the Java side does not read
     * sets {@link Args#forward}.
     *
     * @param inFile whether they are the options of a model file, where
     * {@code -c} and {@code --check-afm} are read, as the frontend checks them
     */
    static Args parse(List<String> args, boolean inFile) {
        List<String> modes = new ArrayList<>();
        Set<String> flags = new HashSet<>();
        OptionalInt tl = OptionalInt.empty();
        Optional<Boolean> simple = Optional.empty();
        OptionalLong minInt = OptionalLong.empty();
        OptionalLong maxInt = OptionalLong.empty();
        Optional<Integer> digits = Optional.empty();
        List<String> imports = new ArrayList<>();
        List<String> files = new ArrayList<>();
        List<String> roots = new ArrayList<>();
        Optional<String> base = Optional.empty();
        Optional<String> forward = Optional.empty();
        boolean onlyFiles = false;
        for (int i = 0; i < args.size() && forward.isEmpty(); i++) {
            String arg = args.get(i);
            if (onlyFiles || !arg.startsWith("-") || arg.equals("-")) {
                files.add(arg);
                continue;
            }
            if (arg.equals("--")) {
                onlyFiles = true;
                continue;
            }
            // the flags and options, with their values
            List<String[]> given = new ArrayList<>();
            if (arg.startsWith("--")) {
                int eq = arg.indexOf('=');
                String name = eq < 0 ? arg : arg.substring(0, eq);
                if (Flags.containsKey(name) && eq < 0) {
                    given.add(new String[]{Flags.get(name), null});
                } else if (Valued.containsKey(name)) {
                    String value = eq >= 0 ? arg.substring(eq + 1) : i + 1 < args.size() ? args.get(++i) : null;
                    given.add(new String[]{Valued.get(name), value});
                } else {
                    forward = Optional.of("the option " + arg);
                }
            } else {
                // short flags, which may be grouped (-so), and -mclafer or -m clafer
                for (int j = 1; j < arg.length() && forward.isEmpty(); j++) {
                    String name = "-" + arg.charAt(j);
                    if (Valued.containsKey(name)) {
                        String rest = arg.substring(j + 1);
                        if (rest.startsWith("=")) {
                            rest = rest.substring(1);
                        }
                        String value = !rest.isEmpty() ? rest : i + 1 < args.size() ? args.get(++i) : null;
                        given.add(new String[]{Valued.get(name), value});
                        break;
                    } else if (Flags.containsKey(name)) {
                        given.add(new String[]{Flags.get(name), null});
                    } else {
                        forward = Optional.of("the option " + arg);
                    }
                }
            }
            for (String[] g : given) {
                if (forward.isPresent()) {
                    break;
                }
                String key = g[0];
                String value = g[1];
                if (Valued.containsValue(key) && value == null) {
                    forward = Optional.of("the option " + arg + " without a value");
                    break;
                }
                try {
                    switch (key) {
                        case "m" -> {
                            if (!modes.contains(value)) {
                                modes.add(value);
                            }
                        }
                        case "tl" -> tl = OptionalInt.of(Integer.parseInt(value));
                        case "ss" -> {
                            if (value.equals("none") || value.equals("simple")) {
                                simple = Optional.of(value.equals("simple"));
                            } else {
                                forward = Optional.of("the scope strategy " + value);
                            }
                        }
                        case "minint" -> minInt = OptionalLong.of(Long.parseLong(value));
                        case "maxint" -> maxInt = OptionalLong.of(Long.parseLong(value));
                        case "digits" -> digits = Optional.of(Integer.parseInt(value));
                        case "import" -> {
                            if (!imports.contains(value)) {
                                imports.add(value);
                            }
                        }
                        case "timeout" -> Integer.parseInt(value);
                        case "root" -> roots.add(value);
                        case "base" -> base = Optional.of(value);
                        case "c", "afm" -> {
                            if (inFile) {
                                flags.add(key);
                            } else {
                                forward = Optional.of("the option " + arg);
                            }
                        }
                        default -> flags.add(key);
                    }
                } catch (NumberFormatException e) {
                    forward = Optional.of("the option " + arg + " with the value " + value);
                }
            }
        }
        if (forward.isEmpty() && files.size() > 1) {
            forward = Optional.of("several model files");
        }
        return new Args(List.copyOf(modes), flags.contains("o"), flags.contains("f"), flags.contains("explicit"),
                flags.contains("core"), flags.contains("provenance"), flags.contains("s"), flags.contains("meta"),
                flags.contains("l"), flags.contains("c") || flags.contains("afm"), tl, simple, minInt, maxInt,
                digits, List.copyOf(imports), List.copyOf(files), flags.contains("help"), flags.contains("version"),
                List.copyOf(roots), flags.contains("public"), base, forward);
    }

    /**
     * @return the arguments of the {@code //# OPTIONS} line of a model file
     */
    static Args fileArgs(String text) {
        String prefix = "//# OPTIONS ";
        if (!text.startsWith(prefix)) {
            return parse(List.of(), true);
        }
        int end = text.indexOf('\n');
        String line = (end < 0 ? text : text.substring(0, end)).substring(prefix.length()).replace("\r", "");
        return parse(ModelOptions.splitArgs(line), true);
    }

    // ------------------------------------------------------------------
    // The run
    // ------------------------------------------------------------------

    /**
     * Runs the command.
     *
     * @return the exit code
     */
    public int run(List<String> args) {
        Args cli = parse(args, false);
        if (cli.forward().isPresent()) {
            return forward(args, cli.forward().get());
        }
        if (cli.help()) {
            out.print(Help);
            return 0;
        }
        if (cli.version()) {
            out.println(version() + " (Java frontend; the other modes are forwarded to clafer)");
            return 0;
        }
        if (!cli.modes().isEmpty() && !cli.modes().equals(List.of("clafer"))) {
            return forward(args, "the mode " + String.join(", ", cli.modes()));
        }
        String file = cli.files().isEmpty() ? "" : cli.files().get(0);
        String name = file.isEmpty() ? ModelFiles.Stdin : file;
        Optional<String> stdin = Optional.empty();
        if (file.isEmpty()) {
            try {
                stdin = Optional.of(new String(in.readAllBytes(), StandardCharsets.UTF_8));
            } catch (IOException e) {
                return error("cannot read the standard input: " + e.getMessage());
            }
        }
        Optional<String> stdinText = stdin;
        ModelFiles files = ModelFiles.read(name, cli.imports(),
                p -> file.isEmpty() && p.equals(name) ? stdinText : Optional.empty(),
                new ModelFiles.ImportOptions(cli.importRoots(), cli.publicImports(), cli.importBase(), true));
        if (!files.errors().isEmpty()) {
            return error(files.errors().get(0));
        }
        // the options of the files, which the command line's override
        List<String> modes = new ArrayList<>(cli.modes());
        boolean console = cli.console() || file.isEmpty();
        boolean core = cli.core();
        boolean provenance = cli.provenance();
        boolean noStats = cli.noStats();
        boolean metaData = cli.metaData();
        boolean explicit = cli.explicit();
        boolean noLayout = cli.noLayout();
        ModelFiles.ModelFile root = files.files().get(files.files().size() - 1);
        List<String> fileModes = new ArrayList<>();
        for (ModelFiles.ModelFile f : files.files()) {
            Args a = fileArgs(f.text());
            if (a.forward().isPresent()) {
                return forward(args, a.forward().get() + " in " + f.path());
            }
            if (!a.files().isEmpty() || a.help() || a.version()) {
                return forward(args, "the options of " + f.path());
            }
            a.modes().stream().filter(m -> !fileModes.contains(m)).forEach(fileModes::add);
            noLayout |= a.noLayout() && !a.explicit();
            if (f == root) {
                console |= a.console();
                core |= a.core();
                provenance |= a.provenance();
                noStats |= a.noStats();
                metaData |= a.metaData();
            }
        }
        if (modes.isEmpty()) {
            modes = fileModes.isEmpty() ? List.of("alloy") : fileModes;
        }
        if (!modes.equals(List.of("clafer"))) {
            return forward(args, "the mode " + String.join(", ", modes));
        }
        if (noLayout && !explicit) {
            return forward(args, "-l");
        }
        ModelOptions o = files.options();
        Set<String> others = new LinkedHashSet<>(o.others());
        boolean simple = o.simpleScopes();
        if (cli.simpleScopes().isPresent() && !cli.simpleScopes().get()) {
            simple = false;
        }
        if (cli.skipResolver()) {
            others.add("-f");
        }
        if (explicit) {
            others.add("--explicit");
            others.add("-f");
            others.add("-l");
            simple = false;
        }
        OptionalInt tl = cli.traceLength().isPresent() && cli.traceLength().getAsInt() != 10
                ? cli.traceLength() : o.traceLength();
        ModelOptions options = new ModelOptions(core || o.core(), tl,
                cli.minInt().isPresent() ? cli.minInt() : o.minInt(),
                cli.maxInt().isPresent() ? cli.maxInt() : o.maxInt(),
                cli.decimalDigits().isPresent() ? cli.decimalDigits() : o.decimalDigits(),
                simple, o.imports(), Set.copyOf(others));
        files = new ModelFiles(files.files(), options, files.errors());
        ToIr.Result result = ToIr.toIr(files);
        if (!result.errors().isEmpty()) {
            // a ClaferErr of the compiler, after a blank line, unlike the errors of reading the files
            return error("\n" + result.errors().get(0));
        }
        if (!result.diagnostics().isEmpty()) {
            Diagnostic d = result.diagnostics().get(0);
            boolean parse = !ClaferParser.parse(files.text()).diagnostics().isEmpty();
            ModelFiles.Position p = files.position(d.span().line());
            String at = p.path() + ":" + p.line() + ":" + d.span().column();
            out.print("\n" + (parse ? "Parse failed at " : "Compile error at ") + at + "...\n");
            out.print(d.message() + "\n");
            return 1;
        }
        Ir.Model model = result.model();
        ModelFiles theFiles = files;
        String code;
        if (options.core()) {
            Function<SourceSpan, Ir.Position> position = span -> {
                ModelFiles.Position p = theFiles.position(span.line());
                return new Ir.Position(Path.of(p.path()).getFileName().toString(), p.line(), span.column());
            };
            Ir.Model lowered = ToCore.toCore(model, position, clocklessValues(files.text()));
            code = ExplicitPrinter.print(lowered, provenance);
        } else {
            code = ExplicitPrinter.print(model, false);
        }
        if (!noStats) {
            Syntax.Model source = ClaferParser.parse(files.text()).model();
            out.print(Statistics.of(result) + Warnings.of(result, source, options.skipResolver()) + "\n");
        }
        String base = dropExtension(file.isEmpty() ? "" : ModelFiles.localName(file));
        String extension = options.core() ? "core.cfr" : "des.cfr";
        try {
            if (console) {
                out.print(code + "\n");
            } else {
                write(base + "." + extension, code);
            }
            if (metaData) {
                write(base + ".cfr-map", nameMap(result.names()));
                write(base + "." + extension + "-scope", result.scopes().orElseThrow().scopeFile());
            }
        } catch (IOException e) {
            return error(e.getMessage());
        }
        out.flush();
        return 0;
    }

    private static void write(String path, String text) throws IOException {
        Files.writeString(Path.of(path), text, StandardCharsets.UTF_8);
    }

    private int error(String message) {
        out.print("\nError...\n" + message + "\n");
        out.flush();
        return 1;
    }

    /**
     * {@code System.FilePath.dropExtension}: the path without the extension
     * of its file name.
     */
    static String dropExtension(String path) {
        int slash = path.lastIndexOf('/');
        int dot = path.lastIndexOf('.');
        return dot > slash ? path.substring(0, dot) : path;
    }

    /**
     * The spans of the values of the {@code :=} written without a clock.
     */
    private static Set<SourceSpan> clocklessValues(String text) {
        Set<SourceSpan> spans = new HashSet<>();
        ClaferParser.Result parsed = ClaferParser.parse(text);
        for (Syntax.Declaration d : parsed.model().declarations()) {
            if (d instanceof Syntax.Element e) {
                clocklessValues(e, spans);
            }
        }
        return spans;
    }

    private static void clocklessValues(Syntax.Element e, Set<SourceSpan> spans) {
        Optional<Syntax.Elements> elements = Optional.empty();
        if (e instanceof Syntax.Clafer c) {
            c.init().filter(i -> i.how() == Syntax.InitHow.Default).ifPresent(i -> spans.add(i.exp().span()));
            elements = c.elements();
        } else if (e instanceof Syntax.ClaferUse u) {
            elements = u.elements();
        }
        elements.ifPresent(es -> es.elements().forEach(x -> clocklessValues(x, spans)));
    }

    /**
     * The map of names, {@code P.cfr-map}, as {@code generateJSONnameUIDMap}
     * writes it: each clafer's fully qualified name, its least-partially
     * qualified name and its UID, sorted by the first.
     */
    static String nameMap(Names names) {
        TreeMap<String, String> byFqName = new TreeMap<>();
        for (Symbols.ClaferSymbol c : names.clafers()) {
            StringBuilder fq = new StringBuilder(c.name());
            for (Optional<Symbols.ClaferSymbol> p = c.parent(); p.isPresent(); p = p.get().parent()) {
                fq.insert(0, p.get().name() + "__");
            }
            byFqName.put("root__" + fq, c.uid());
        }
        StringBuilder sb = new StringBuilder("[");
        boolean first = true;
        for (Map.Entry<String, String> e : byFqName.entrySet()) {
            sb.append(first ? "" : ",");
            first = false;
            sb.append("{\"fqName\":").append(json(e.getKey())).append(",\"lpqName\":").append(json(e.getValue()))
                    .append(",\"uid\":").append(json(e.getValue())).append('}');
        }
        sb.append(']');
        // prettyPrintJSON: a newline after [, { and , and before ] and }
        StringBuilder pretty = new StringBuilder();
        for (char ch : sb.toString().toCharArray()) {
            switch (ch) {
                case '[', '{', ',' -> pretty.append(ch).append('\n');
                case ']', '}' -> pretty.append('\n').append(ch);
                default -> pretty.append(ch);
            }
        }
        return pretty.toString();
    }

    private static String json(String s) {
        StringBuilder sb = new StringBuilder("\"");
        for (char ch : s.toCharArray()) {
            switch (ch) {
                case '"' -> sb.append("\\\"");
                case '\\' -> sb.append("\\\\");
                default -> sb.append(ch);
            }
        }
        return sb.append('"').toString();
    }

    // ------------------------------------------------------------------
    // Forwarding
    // ------------------------------------------------------------------

    /**
     * Runs the compiler with the arguments, its output on this one's.
     */
    private int forward(List<String> args, String why) {
        List<String> command = new ArrayList<>();
        command.add(executable);
        command.addAll(args);
        try {
            Process p = new ProcessBuilder(command).redirectInput(ProcessBuilder.Redirect.INHERIT)
                    .redirectError(ProcessBuilder.Redirect.INHERIT).start();
            p.getInputStream().transferTo(out);
            out.flush();
            return p.waitFor();
        } catch (IOException e) {
            out.print("\nError...\nThe Java frontend does not implement " + why + ", which needs the compiler "
                    + executable + ", but it cannot be run: " + e.getMessage() + "\n");
            out.flush();
            return 1;
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            return 1;
        }
    }

    /**
     * @return the name, the version and the build date, as the other Clafer
     * tools print them: {@code claferc v0.6.0.2026-11-26}
     */
    static String version() {
        java.util.Properties p = new java.util.Properties();
        try (java.io.InputStream in = ClaferCommand.class.getResourceAsStream("version.properties")) {
            p.load(in);
        } catch (java.io.IOException | NullPointerException e) {
            return "claferc vunknown";
        }
        return "claferc v" + p.getProperty("version") + "." + p.getProperty("releasedate");
    }

    private static final String Help = """
            claferc [OPTIONS] [FILE]

            The Clafer compiler's 'clafer -m clafer' in Java: the explicit models of a Clafer model.

              -m clafer               The explicit model, written to FILE.des.cfr (the default mode,
                                      alloy, and the other modes are forwarded to clafer).
                 --core               The explicit core model, written to FILE.core.cfr.
                 --record-provenance  With --core, the provenance comments of the lowered constructs.
                 --meta-data          The explicit model's scope file (FILE.des.cfr-scope or
                                      FILE.core.cfr-scope) and the map of names (FILE.cfr-map).
              -o --console-output     Print the output instead of writing it.
              -s --no-stats           Don't print the statistics.
              -f --skip-resolver      The names are the UIDs.
                 --explicit           The model is an explicit model: -f -l --ss=none.
                 --ss=none|simple     The scope strategy.
                 --tl=INT             The trace length.
                 --minint=INT, --maxint=INT, --decimal-digits=INT
                                      The ranges of the scope file.
                 --import=FILE        Compile the model with the model in FILE (a path or an http/https
                                      URL). Can be repeated. FILE may also be a URL.
                 --import-root=DIR    Allow only local imports inside DIR. Can be repeated.
                 --public-imports     Allow only URL imports from public addresses.
                 --import-base=URL    The URL from which the model's text was read.
              -k --keep-unused, --timeout-analysis=INT
                                      Accepted; they change nothing in the explicit models.

            Any other mode or option is forwarded to clafer on the path, with the same arguments.
            """;
}
