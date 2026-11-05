package org.clafer.scope;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstModel;
import org.clafer.ast.AstUtil;

/**
 * A {@code .cfr-scope} file: the scopes of a model, kept apart from the model.
 * It is a JSON array of entries, each an object of one kind:
 * <ul>
 * <li>{@code {"lpqName": name, "scope": n}}: the scope of the clafers named
 * {@code name}, a UID ({@code c0_Cfeatures}) or a (partially) qualified name
 * ({@code Cfeatures}, {@code Component::Cfeatures},
 * {@code ::Component::Cfeatures}) that names all the clafers whose containment
 * paths end with it. The empty name is the root, whose scope is always 1. This
 * is the entry {@code clafer --meta-data}, claferIG and the REPL write.</li>
 * <li>{@code {"defaultScope": n}}: the scope of the clafers without their own.
 * It also matters when every clafer has its own: the scope analysis of a
 * recursive structure uses it.</li>
 * <li>{@code {"int": [low, high]}}: the integers.</li>
 * <li>{@code {"mul": [low, high]}}: the products of multiplications.</li>
 * <li>{@code {"stringLength": n}}: the longest string.</li>
 * <li>{@code {"chars": [low, high]}}: the characters of strings, as code
 * points.</li>
 * <li>{@code {"decimalDigits": n}}: the fractional digits of decimals.</li>
 * <li>{@code {"decimal": [low, high]}}: the decimals, e.g.,
 * {@code [-21474.83648, 21474.83647]}.</li>
 * </ul>
 * Anything not in the file keeps the scope it had.
 */
public final class ScopeFile {

    private final String fileName;
    // name -> scope, in the order of the file
    private final List<Map.Entry<String, Integer>> claferScopes;
    private final Map<String, Object> ranges;

    private ScopeFile(String fileName, List<Map.Entry<String, Integer>> claferScopes, Map<String, Object> ranges) {
        this.fileName = fileName;
        this.claferScopes = claferScopes;
        this.ranges = ranges;
    }

    /**
     * Prints every scope of a model: the scope of every clafer by its UID,
     * sorted, whether it was given or is the default, then the default scope
     * and every range.
     *
     * @param model the model
     * @param scope its scope
     * @return the contents of the model's {@code .cfr-scope} file
     */
    public static String print(AstModel model, Scope scope) {
        List<AstClafer> clafers = AstUtil.getClafers(model).stream()
                .filter(c -> c != model.getAbstractRoot() && c != model.getRoot() && c != model.getTypeRoot())
                .sorted(Comparator.comparing(AstClafer::getName))
                .collect(Collectors.toList());
        List<String> entries = new ArrayList<>();
        for (AstClafer clafer : clafers) {
            entries.add("{\"lpqName\": " + quote(clafer.getName()) + ", \"scope\": " + scope.getScope(clafer) + "}");
        }
        entries.add("{\"defaultScope\": " + scope.getDefaultScope() + "}");
        entries.add("{\"int\": [" + scope.getIntLow() + ", " + scope.getIntHigh() + "]}");
        entries.add("{\"mul\": [" + scope.getMulLow() + ", " + scope.getMulHigh() + "]}");
        entries.add("{\"stringLength\": " + scope.getStringLength() + "}");
        entries.add("{\"chars\": [" + (int) scope.getCharLow() + ", " + (int) scope.getCharHigh() + "]}");
        entries.add("{\"decimalDigits\": " + scope.getDecimalScale() + "}");
        entries.add("{\"decimal\": [" + scope.unscaleDecimal(scope.getDecimalLow()).toPlainString()
                + ", " + scope.unscaleDecimal(scope.getDecimalHigh()).toPlainString() + "]}");
        return "[\n" + String.join(",\n", entries) + "\n]\n";
    }

    /**
     * Reads a scope file.
     *
     * @param json the contents
     * @param fileName the file's name, for errors
     * @return the scope file
     * @throws IllegalArgumentException if the file is malformed
     */
    public static ScopeFile parse(String json, String fileName) {
        Reader reader = new Reader(json, fileName);
        List<Map.Entry<String, Integer>> claferScopes = new ArrayList<>();
        Map<String, Object> ranges = new LinkedHashMap<>();
        for (Map<String, Object> entry : reader.readFile()) {
            if (entry.containsKey("lpqName")) {
                if (!(entry.get("lpqName") instanceof String name)
                        || !(entry.get("scope") instanceof BigDecimal scope) || entry.size() != 2) {
                    throw reader.error("expected {\"lpqName\": name, \"scope\": n} but found " + entry);
                }
                claferScopes.add(Map.entry(name, reader.natural(scope)));
            } else if (entry.size() == 1) {
                Map.Entry<String, Object> range = entry.entrySet().iterator().next();
                ranges.put(range.getKey(), reader.range(range.getKey(), range.getValue()));
            } else {
                throw reader.error("expected one range per entry but found " + entry);
            }
        }
        return new ScopeFile(fileName, claferScopes, ranges);
    }

    /**
     * @return whether the file gives the integers
     */
    public boolean hasIntRange() {
        return ranges.containsKey("int");
    }

    /**
     * Applies the file to a scope.
     *
     * @param scope the scope
     * @param model the model, whose clafers the file names
     * @return the scope with the file's scopes and ranges
     * @throws IllegalArgumentException if a name is not of a clafer in the
     * model
     */
    public Scope applyTo(Scope scope, AstModel model) {
        List<AstClafer> clafers = new ArrayList<>(AstUtil.getClafers(model));
        clafers.remove(model.getAbstractRoot());
        clafers.remove(model.getRoot());
        ScopeBuilder builder = scope.toBuilder();
        for (Map.Entry<String, Integer> entry : claferScopes) {
            // the REPL saves the root clafer #clafer# with an empty name
            if (entry.getKey().isEmpty()) {
                continue;
            }
            List<AstClafer> named = claferNamed(clafers, entry.getKey());
            if (named.isEmpty()) {
                throw new IllegalArgumentException(fileName + ": the model has no clafer " + entry.getKey());
            }
            for (AstClafer clafer : named) {
                builder.setScope(clafer, entry.getValue());
            }
        }
        for (Map.Entry<String, Object> range : ranges.entrySet()) {
            Object value = range.getValue();
            switch (range.getKey()) {
                case "defaultScope" -> builder.defaultScope((Integer) value);
                case "int" -> builder.intLow(((int[]) value)[0]).intHigh(((int[]) value)[1]);
                case "mul" -> builder.mulLow(((int[]) value)[0]).mulHigh(((int[]) value)[1]);
                case "stringLength" -> builder.stringLength((Integer) value);
                case "chars" -> builder.charLow((char) ((int[]) value)[0]).charHigh((char) ((int[]) value)[1]);
                case "decimalDigits" -> builder.decimalScale((Integer) value);
                case "decimal" -> builder.decimalLow(((BigDecimal[]) value)[0]).decimalHigh(((BigDecimal[]) value)[1]);
                default -> throw new IllegalStateException(range.getKey());
            }
        }
        try {
            return builder.toScope();
        } catch (IllegalArgumentException e) {
            throw new IllegalArgumentException(fileName + ": " + e.getMessage(), e);
        }
    }

    /**
     * The clafers with the given UID, otherwise with the given (partially)
     * qualified name.
     *
     * @param clafers the clafers to look in
     * @param qName a UID or a (partially) qualified name
     * @return the clafers it names
     */
    public static List<AstClafer> claferNamed(List<AstClafer> clafers, String qName) {
        List<AstClafer> result = new ArrayList<>();
        for (AstClafer clafer : clafers) {
            if (clafer.getName().equals(qName)) {
                result.add(clafer);
            }
        }
        if (!result.isEmpty()) {
            return result;
        }
        boolean full = qName.startsWith("::");
        List<String> names = new ArrayList<>();
        // an apostrophe in a name is __prime__ in the UID
        for (String name : (full ? qName.substring(2) : qName).split("::")) {
            names.add(name.replace("'", "__prime__"));
        }
        for (AstClafer clafer : clafers) {
            List<String> path = containmentPath(clafer);
            if (full ? path.equals(names)
                    : path.size() >= names.size() && path.subList(path.size() - names.size(), path.size()).equals(names)) {
                result.add(clafer);
            }
        }
        return result;
    }

    /**
     * The names of a clafer and its ancestors, from the top-level one, e.g.,
     * [Component, Cfeatures].
     */
    private static List<String> containmentPath(AstClafer clafer) {
        List<String> path = new ArrayList<>();
        AstClafer current = clafer;
        while (current != null && !current.getName().startsWith("#")) {
            // the name without the UID's prefix, e.g., Cfeatures for c0_Cfeatures
            path.add(0, current.getName().substring(current.getName().indexOf('_') + 1));
            // a top-level clafer's parent is the model's root #root#; an abstract clafer can be
            // nested, also under a concrete clafer
            AstClafer parent = current.getParent();
            current = parent == null || (parent instanceof AstConcreteClafer && !parent.hasParent()) ? null : parent;
        }
        return path;
    }

    private static String quote(String s) {
        return "\"" + s.replace("\\", "\\\\").replace("\"", "\\\"") + "\"";
    }

    /**
     * Reads the JSON of a scope file: an array of objects whose values are
     * strings, numbers or arrays of numbers.
     */
    private static final class Reader {

        private final String json;
        private final String fileName;
        private int pos = 0;

        Reader(String json, String fileName) {
            this.json = json;
            this.fileName = fileName;
        }

        IllegalArgumentException error(String message) {
            return new IllegalArgumentException(fileName + ": " + message);
        }

        List<Map<String, Object>> readFile() {
            List<Map<String, Object>> entries = new ArrayList<>();
            expect('[');
            if (!consume(']')) {
                do {
                    entries.add(readObject());
                } while (consume(','));
                expect(']');
            }
            skipSpace();
            if (pos < json.length()) {
                throw error("unexpected text after the list at offset " + pos);
            }
            return entries;
        }

        private Map<String, Object> readObject() {
            Map<String, Object> object = new LinkedHashMap<>();
            expect('{');
            if (!consume('}')) {
                do {
                    String key = readString();
                    expect(':');
                    if (object.put(key, readValue()) != null) {
                        throw error("\"" + key + "\" twice in one entry");
                    }
                } while (consume(','));
                expect('}');
            }
            return object;
        }

        private Object readValue() {
            skipSpace();
            if (pos < json.length() && json.charAt(pos) == '"') {
                return readString();
            }
            if (consume('[')) {
                List<BigDecimal> numbers = new ArrayList<>();
                if (!consume(']')) {
                    do {
                        numbers.add(readNumber());
                    } while (consume(','));
                    expect(']');
                }
                return numbers;
            }
            return readNumber();
        }

        private String readString() {
            expect('"');
            StringBuilder out = new StringBuilder();
            while (pos < json.length() && json.charAt(pos) != '"') {
                char c = json.charAt(pos++);
                if (c == '\\' && pos < json.length()) {
                    char e = json.charAt(pos++);
                    switch (e) {
                        case 'n' -> out.append('\n');
                        case 't' -> out.append('\t');
                        case 'u' -> {
                            if (pos + 4 > json.length()) {
                                throw error("a bad escape at offset " + pos);
                            }
                            out.append((char) Integer.parseInt(json.substring(pos, pos + 4), 16));
                            pos += 4;
                        }
                        default -> out.append(e);
                    }
                } else {
                    out.append(c);
                }
            }
            expect('"');
            return out.toString();
        }

        private BigDecimal readNumber() {
            skipSpace();
            int start = pos;
            while (pos < json.length() && "+-0123456789.eE".indexOf(json.charAt(pos)) >= 0) {
                pos++;
            }
            try {
                return new BigDecimal(json.substring(start, pos));
            } catch (NumberFormatException e) {
                throw error("expected a number at offset " + start);
            }
        }

        int natural(BigDecimal n) {
            try {
                int value = n.intValueExact();
                if (value >= 0) {
                    return value;
                }
            } catch (ArithmeticException e) {
                // below
            }
            throw error("expected a natural number but found " + n);
        }

        private int integer(BigDecimal n) {
            try {
                return n.intValueExact();
            } catch (ArithmeticException e) {
                throw error("expected an integer but found " + n);
            }
        }

        Object range(String key, Object value) {
            switch (key) {
                case "int", "mul", "chars", "decimal" -> {
                    if (!(value instanceof List<?> list) || list.size() != 2) {
                        throw error("expected {\"" + key + "\": [low, high]} but found " + value);
                    }
                    BigDecimal low = (BigDecimal) list.get(0);
                    BigDecimal high = (BigDecimal) list.get(1);
                    if (low.compareTo(high) > 0) {
                        throw error("the range " + key + " " + low + ".." + high + " is empty");
                    }
                    return key.equals("decimal")
                            ? new BigDecimal[]{low, high}
                            : new int[]{integer(low), integer(high)};
                }
                case "defaultScope", "stringLength", "decimalDigits" -> {
                    if (!(value instanceof BigDecimal n)) {
                        throw error("expected {\"" + key + "\": n} but found " + value);
                    }
                    return natural(n);
                }
                default -> throw error("unknown entry \"" + key + "\"");
            }
        }

        private void skipSpace() {
            while (pos < json.length() && Character.isWhitespace(json.charAt(pos))) {
                pos++;
            }
        }

        private boolean consume(char c) {
            skipSpace();
            if (pos < json.length() && json.charAt(pos) == c) {
                pos++;
                return true;
            }
            return false;
        }

        private void expect(char c) {
            if (!consume(c)) {
                throw error("expected '" + c + "' at offset " + pos
                        + (pos < json.length() ? " but found '" + json.charAt(pos) + "'" : " but found the end"));
            }
        }
    }
}
