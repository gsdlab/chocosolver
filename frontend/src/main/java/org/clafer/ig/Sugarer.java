package org.clafer.ig;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.UnaryOperator;
import org.clafer.ig.ClaferModel.AliasValue;
import org.clafer.ig.ClaferModel.Clafer;
import org.clafer.ig.ClaferModel.Id;
import org.clafer.ig.ClaferModel.IntValue;
import org.clafer.ig.ClaferModel.StringValue;
import org.clafer.ig.ClaferModel.Value;

/**
 * The names of the instances, as claferIG's {@code Sugarer.hs} gives them,
 * whatever the backend: an instance is named by its clafer's name, or its UID
 * with {@code --useuids}, followed by its number among the instances of that
 * name ({@code $n}) when there are several of them or with
 * {@code --useuids}, and by its super clafer with {@code --addtypes}.
 *
 * <p>
 * The raw solution names each instance by its clafer's key in
 * {@link Naming#clafers()} (Alloy's name of the clafer, or the UID) and a
 * number that tells the instances of that clafer apart.
 */
public final class Sugarer {

    /**
     * What the names of the instances need of a clafer, by its key.
     *
     * @param ident its name
     * @param superName the key of its super clafer, or null
     * @param reference the primitive type of its own reference, e.g.,
     * {@code string}, or null
     */
    public record ClaferInfo(String ident, String superName, String reference) {
    }

    /**
     * How the instances are named.
     *
     * @param clafers the clafers by their keys
     * @param toUid the UID of a clafer by its key
     * @param useUids the instances are named by the UIDs of their clafers
     * @param addTypes each instance has the super clafer of its clafer
     * @param strings the strings by their integers, for a backend that
     * encodes strings as integers (Alloy)
     */
    public record Naming(Map<String, ClaferInfo> clafers, UnaryOperator<String> toUid, boolean useUids,
            boolean addTypes, Map<Integer, String> strings) {
    }

    private Sugarer() {
    }

    /**
     * The simple name of each instance and its number among the instances of
     * that name, and how many instances each name has.
     */
    private static final class Census {

        final Map<Id, Object[]> sample = new HashMap<>();
        final Map<String, Integer> counts = new HashMap<>();

        void poll(Naming naming, Id id) {
            String name = claferName(naming, id.name());
            int ordinal = counts.merge(name, 1, Integer::sum);
            sample.put(id, new Object[]{ordinal, name});
        }
    }

    /**
     * @return the name of a clafer by its key, or the key if it is unknown
     */
    public static String claferName(Naming naming, String key) {
        ClaferInfo info = naming.clafers().get(key);
        return info == null ? key : info.ident();
    }

    /**
     * The named instance of a static model.
     */
    public static ClaferModel sugarInstance(Naming naming, ClaferModel raw) {
        Census census = new Census();
        raw.topLevel().forEach(c -> census(naming, c, census));
        return sugar(census, naming, raw);
    }

    /**
     * The named states of a trace: an instance that exists in several states
     * is counted once, so it has the same name in each of them.
     */
    public static List<ClaferModel> sugarStates(Naming naming, List<ClaferModel> states) {
        Census census = new Census();
        for (ClaferModel state : states) {
            state.topLevel().forEach(c -> census(naming, c, census));
        }
        return states.stream().map(s -> sugar(census, naming, s)).toList();
    }

    private static void census(Naming naming, Clafer c, Census census) {
        c.children().forEach(x -> census(naming, x, census));
        if (!census.sample.containsKey(c.id())) {
            census.poll(naming, c.id());
        }
    }

    private static ClaferModel sugar(Census census, Naming naming, ClaferModel model) {
        return new ClaferModel(model.topLevel().stream().map(c -> sugarClafer(census, naming, c)).toList());
    }

    private static Clafer sugarClafer(Census census, Naming naming, Clafer c) {
        return new Clafer(sugarId(census, naming, true, c.id()), c.uid(), sugarValue(census, naming, c),
                c.children().stream().map(x -> sugarClafer(census, naming, x)).toList());
    }

    private static Value sugarValue(Census census, Naming naming, Clafer c) {
        if (c.value() instanceof AliasValue a) {
            return new AliasValue(sugarId(census, naming, false, a.alias()));
        }
        if (c.value() == null) {
            return null;
        }
        ClaferInfo info = naming.clafers().get(c.id().name());
        if (info == null) {
            throw new IllegalStateException("IClafer not found:" + c.id().name());
        }
        if ("string".equals(info.reference()) && c.value() instanceof IntValue i) {
            String s = naming.strings().get(i.value());
            return new StringValue(s == null ? "\"<text " + i.value() + ">\"" : s);
        }
        return c.value();
    }

    private static Id sugarId(Census census, Naming naming, boolean addRefDecl, Id id) {
        Object[] sample = census.sample.get(id);
        if (sample == null) {
            // an atom that is in no state of the trace, e.g., the literal of an enumeration
            return new Id(naming.useUids() ? naming.toUid().apply(id.name()) : claferName(naming, id.name()), 0);
        }
        int ordinal = (Integer) sample[0];
        String simpleName = (String) sample[1];
        int count = census.counts.get(simpleName);
        String ordinalDisplay = naming.useUids() || count > 1 ? "$" + ordinal : "";
        String finalName = naming.useUids() ? naming.toUid().apply(id.name()) : simpleName;
        String refDecl = "";
        if (naming.addTypes() && addRefDecl) {
            ClaferInfo info = naming.clafers().get(id.name());
            if (info == null) {
                throw new IllegalStateException("IClafer not found: " + id.name());
            }
            if (info.superName() != null) {
                refDecl = " : " + (naming.useUids() ? naming.toUid().apply(info.superName())
                        : claferName(naming, info.superName()));
            }
        }
        return new Id(finalName + ordinalDisplay + refDecl, 0);
    }
}
