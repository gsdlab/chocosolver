package org.clafer.frontend;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Optional;
import java.util.OptionalInt;
import java.util.OptionalLong;
import java.util.Set;

/**
 * The options of a model file that the frontend uses, from its first line,
 * {@code //# OPTIONS ...}, as the compiler reads them
 * ({@code Import.fileOptions}): {@code --core}, {@code --tl},
 * {@code --minint}, {@code --maxint}, {@code --decimal-digits},
 * {@code --ss} and {@code --import}. An option is {@code --name=value} or
 * {@code --name value}; the other options are kept by name only, the short
 * name if it has one, e.g., {@code -c} for {@code --check-duplicates}, for
 * the checks of the model and of {@link #combine}. As in the compiler,
 * {@code --explicit} implies {@code -f}, {@code -l} and {@code --ss=none}.
 *
 * @param core {@code --core}
 * @param traceLength {@code --tl}
 * @param minInt {@code --minint}
 * @param maxInt {@code --maxint}
 * @param decimalDigits {@code --decimal-digits}
 * @param simpleScopes {@code --ss=simple}, the default, rather than
 * {@code --ss=none}
 * @param imports the files of {@code --import}, as written
 * @param others the names of the other options given, e.g., {@code -k}
 */
public record ModelOptions(boolean core, OptionalInt traceLength, OptionalLong minInt, OptionalLong maxInt,
        Optional<Integer> decimalDigits, boolean simpleScopes, List<String> imports, Set<String> others) {

    /**
     * The options of a model without a {@code //# OPTIONS} line.
     */
    public static final ModelOptions Defaults = new ModelOptions(false, OptionalInt.empty(), OptionalLong.empty(),
            OptionalLong.empty(), Optional.empty(), true, List.of(), Set.of());

    private static final String Prefix = "//# OPTIONS ";

    /**
     * @param text the text of a model file
     * @return the options of its first line
     */
    public static ModelOptions of(String text) {
        if (!text.startsWith(Prefix)) {
            return Defaults;
        }
        int end = text.indexOf('\n');
        String line = (end < 0 ? text : text.substring(0, end)).substring(Prefix.length()).replace("\r", "");
        List<String> args = splitArgs(line);
        boolean core = false;
        OptionalInt tl = OptionalInt.empty();
        OptionalLong minInt = OptionalLong.empty();
        OptionalLong maxInt = OptionalLong.empty();
        Optional<Integer> digits = Optional.empty();
        boolean simple = true;
        List<String> imports = new ArrayList<>();
        Set<String> others = new LinkedHashSet<>();
        for (int i = 0; i < args.size(); i++) {
            String arg = args.get(i);
            String name = arg;
            String value = null;
            int eq = arg.indexOf('=');
            if (arg.startsWith("--") && eq > 0) {
                name = arg.substring(0, eq);
                value = arg.substring(eq + 1);
            }
            boolean takesValue = Set.of("--tl", "--minint", "--maxint", "--decimal-digits", "--ss", "--import")
                    .contains(name);
            if (takesValue && value == null && i + 1 < args.size()) {
                value = args.get(++i);
            }
            try {
                switch (name) {
                    case "--core" -> core = true;
                    case "--tl" -> tl = OptionalInt.of(Integer.parseInt(value));
                    case "--minint" -> minInt = OptionalLong.of(Long.parseLong(value));
                    case "--maxint" -> maxInt = OptionalLong.of(Long.parseLong(value));
                    case "--decimal-digits" -> digits = Optional.of(Integer.parseInt(value));
                    case "--ss" -> simple = !"none".equalsIgnoreCase(value);
                    case "--import" -> imports.add(value);
                    default -> others.add(Aliases.getOrDefault(name, name));
                }
            } catch (NumberFormatException | NullPointerException e) {
                others.add(name);
            }
        }
        if (others.contains("--explicit")) {
            // the names are the UIDs, there is no layout and no scope analysis (ClaferArgs.withExplicit)
            others.add("-f");
            others.add("-l");
            simple = false;
        }
        return new ModelOptions(core, tl, minInt, maxInt, digits, simple, List.copyOf(imports), Set.copyOf(others));
    }

    /**
     * The options that the frontend reads by their short names.
     */
    private static final java.util.Map<String, String> Aliases = java.util.Map.of("--check-duplicates", "-c",
            "--skip-resolver", "-f", "--no-layout", "-l", "--new-layout", "--nl", "--afm", "--check-afm");

    /**
     * @return {@code --explicit}: the model is an explicit model, whose
     * names are the UIDs, without layout and without scope analysis
     */
    public boolean explicit() {
        return others.contains("--explicit");
    }

    /**
     * @return {@code -f}, also of {@code --explicit}: the names are the
     * UIDs
     */
    public boolean skipResolver() {
        return others.contains("-f");
    }

    /**
     * @return {@code --check-duplicates}: no two clafers have the same name
     */
    public boolean checkDuplicates() {
        return others.contains("-c");
    }

    /**
     * @return {@code --check-afm}: no concrete clafer has more than one
     * instance
     */
    public boolean checkAfm() {
        return others.contains("--check-afm");
    }

    /**
     * The arguments of the line, split at spaces outside double quotes
     * ({@code SplitJoin.splitArgs}).
     */
    private static List<String> splitArgs(String line) {
        List<String> out = new ArrayList<>();
        StringBuilder current = new StringBuilder();
        boolean quoted = false;
        boolean any = false;
        for (int i = 0; i < line.length(); i++) {
            char ch = line.charAt(i);
            if (ch == '"') {
                quoted = !quoted;
                any = true;
            } else if (ch == '\\' && i + 1 < line.length() && line.charAt(i + 1) == '"') {
                current.append('"');
                i++;
            } else if (Character.isWhitespace(ch) && !quoted) {
                if (any || current.length() > 0) {
                    out.add(current.toString());
                }
                current.setLength(0);
                any = false;
            } else {
                current.append(ch);
            }
        }
        if (any || current.length() > 0) {
            out.add(current.toString());
        }
        return out;
    }

    /**
     * The options that only the command line can give.
     */
    private static final List<String> CommandLineOptions = List.of("-v", "--tooldir", "--import-root",
            "--public-imports", "--import-base");

    /**
     * The options of the compilation, which only the compiled model can give.
     */
    private static final List<String> RunOptions = List.of("-o", "-s", "-a", "--self-contained", "--add-graph",
            "--sr", "--add-comments", "--meta-data", "--ecore2clafer");

    /**
     * Combines the options of the imported files and of the model as the
     * compiler does ({@code Import.combineOptions}): the largest
     * {@code --maxint}, {@code --decimal-digits} and {@code --tl} (10 for a
     * file without it), the smallest {@code --minint}, all the imports, and
     * {@code --ss} the same in all files.
     *
     * @param imported the paths and options of the imported files, in order
     * @param root the path of the model
     * @param rootOptions its options
     * @param errors the compiler's errors of the options
     * @return the options of the whole model
     */
    static ModelOptions combine(List<String> importedPaths, List<ModelOptions> imported, String root,
            ModelOptions rootOptions, List<String> errors) {
        List<String> paths = new ArrayList<>();
        List<ModelOptions> all = new ArrayList<>();
        paths.add(root);
        all.add(rootOptions);
        paths.addAll(importedPaths);
        all.addAll(imported);
        for (int i = 0; i < all.size(); i++) {
            List<String> names = CommandLineOptions.stream().filter(all.get(i).others()::contains).toList();
            if (!names.isEmpty()) {
                errors.add(inFile(paths.get(i), theOptions(names) + " can only be given on the command line, not in"
                        + " //# OPTIONS"));
                return rootOptions;
            }
        }
        for (int i = 0; i < imported.size(); i++) {
            List<String> names = RunOptions.stream().filter(imported.get(i).others()::contains).toList();
            if (!names.isEmpty()) {
                errors.add(inFile(importedPaths.get(i), theOptions(names) + " can only be given in the compiled model"
                        + " or on the command line, not in an imported file"));
                return rootOptions;
            }
        }
        for (String option : List.of("-l", "--nl")) {
            List<String> differ = new ArrayList<>();
            for (int i = 0; i < imported.size(); i++) {
                if (imported.get(i).others().contains(option) != rootOptions.others().contains(option)) {
                    differ.add(importedPaths.get(i));
                }
            }
            if (!differ.isEmpty()) {
                errors.add(differs(option, differ, root));
                return rootOptions;
            }
        }
        List<String> differ = new ArrayList<>();
        for (int i = 0; i < imported.size(); i++) {
            if (imported.get(i).simpleScopes() != rootOptions.simpleScopes()) {
                differ.add(importedPaths.get(i));
            }
        }
        if (!differ.isEmpty()) {
            errors.add(differs("--ss", differ, root));
            return rootOptions;
        }
        if (imported.isEmpty()) {
            return rootOptions;
        }
        OptionalInt tl = OptionalInt.of(all.stream().mapToInt(o -> o.traceLength().orElse(10)).max().getAsInt());
        OptionalLong maxInt = all.stream().filter(o -> o.maxInt().isPresent()).mapToLong(o -> o.maxInt().getAsLong())
                .max();
        OptionalLong minInt = all.stream().filter(o -> o.minInt().isPresent()).mapToLong(o -> o.minInt().getAsLong())
                .min();
        Optional<Integer> digits = all.stream().flatMap(o -> o.decimalDigits().stream()).max(Integer::compare);
        LinkedHashSet<String> imports = new LinkedHashSet<>();
        Set<String> others = new LinkedHashSet<>();
        for (ModelOptions o : all) {
            imports.addAll(o.imports());
            others.addAll(o.others());
        }
        // the model is explicit if its file is, and the names are the UIDs if they are in all files
        if (!rootOptions.explicit()) {
            others.remove("--explicit");
        }
        if (!all.stream().allMatch(ModelOptions::skipResolver)) {
            others.remove("-f");
        }
        return new ModelOptions(rootOptions.core(), tl, minInt, maxInt, digits, rootOptions.simpleScopes(),
                List.copyOf(imports), Set.copyOf(others));
    }

    private static String differs(String option, List<String> paths, String root) {
        return "The option " + option + " must be the same in all files of the model: it differs in "
                + String.join(", ", paths) + " from " + root;
    }

    private static String theOptions(List<String> names) {
        return names.size() == 1 ? "the option " + names.get(0) : "the options " + String.join(", ", names);
    }

    static String inFile(String path, String message) {
        return (path.isEmpty() ? "" : path + ": ") + message;
    }
}
