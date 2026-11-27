package org.clafer.ig;

import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import org.clafer.frontend.Symbols;
import org.clafer.frontend.Syntax;
import org.clafer.ig.ClaferModel.AliasValue;
import org.clafer.ig.ClaferModel.Clafer;
import org.clafer.ig.ClaferModel.IntValue;
import org.clafer.ig.ClaferModel.StringValue;

/**
 * The JSON of a solution, as claferIG's {@code JSONGenerator.hs} writes it
 * ({@code --json}): each instance as an object with the {@code ident} of its
 * clafer, its {@code uid} (the UID of its clafer and its number, as
 * {@code --useuids} names it), the {@code super} and the {@code reference}
 * of its clafer, if any, the clafer's {@code cardMin} and {@code cardMax},
 * its {@code value}, if any, and its {@code elements}; a trace as its
 * {@code states} and its {@code loop} state.
 */
public final class JsonGenerator {

    /**
     * What the JSON has of a clafer.
     *
     * @param ident its name
     * @param superUid the UID of its super clafer, or null
     * @param reference the UID or the primitive type of its reference, or
     * null
     * @param cardMin the lower bound of its cardinality
     * @param cardMax the upper bound of its cardinality, -1 for none
     */
    public record Info(String ident, String superUid, String reference, int cardMin, int cardMax) {
    }

    private JsonGenerator() {
    }

    /**
     * What the JSON has of the clafers of a model, by their UIDs, as the
     * compiler's IR has them: the super clafer and the reference by their
     * UIDs, a primitive as it is written.
     */
    public static Map<String, Info> infos(Collection<Symbols.ClaferSymbol> clafers) {
        Map<String, Info> out = new HashMap<>();
        for (Symbols.ClaferSymbol c : clafers) {
            String reference = c.reference()
                    .filter(r -> r.hasType() && refIds(r.target()) == 1)
                    .map(r -> r.clafer().map(Symbols.ClaferSymbol::uid).orElseGet(() -> r.primitive().orElseThrow()))
                    .orElse(null);
            out.put(c.uid(), new Info(c.name(), c.superClafer().map(Symbols.ClaferSymbol::uid).orElse(null),
                    reference, c.card().low(), c.card().high()));
        }
        return out;
    }

    // how many names the target of a reference has, as the compiler's getRefIds finds them: a set
    // expression, e.g., -> a ++ b, has no one reference in the JSON
    private static int refIds(Syntax.Exp target) {
        return switch (target) {
            case Syntax.ClaferId id -> 1;
            case Syntax.Binary b when b.op() == Syntax.BinaryOp.Join -> refIds(b.right());
            case Syntax.Binary b when b.op() == Syntax.BinaryOp.Union || b.op() == Syntax.BinaryOp.UnionCom ->
                refIds(b.left()) + refIds(b.right());
            default -> 0;
        };
    }

    /**
     * @param solution a solution named with {@code --useuids}
     * @param clafers the information of a clafer by its UID, or null
     */
    public static String solutionJSON(Solution solution, Function<String, Info> clafers) {
        return switch (solution) {
            case Solution.Instance i -> generateJSON(i.model(), clafers);
            case Solution.Trace t -> "{\"states\": ["
                    + String.join(", ", t.states().stream().map(s -> generateJSON(s, clafers)).toList())
                    + "], \"loop\": " + t.loopState() + "}";
        };
    }

    /**
     * The JSON of an instance, or a state of a trace.
     */
    public static String generateJSON(ClaferModel model, Function<String, Info> clafers) {
        StringBuilder sb = new StringBuilder("{");
        elements(model.topLevel(), clafers, sb);
        return sb.append('}').toString();
    }

    private static void elements(List<Clafer> clafers, Function<String, Info> infos, StringBuilder sb) {
        sb.append("\"elements\":[");
        for (int i = 0; i < clafers.size(); i++) {
            if (i > 0) {
                sb.append(',');
            }
            clafer(clafers.get(i), infos, sb);
        }
        sb.append(']');
    }

    private static void clafer(Clafer c, Function<String, Info> infos, StringBuilder sb) {
        String uid = c.name();
        String claferUid = removeOrdinal(uid);
        Info info = infos.apply(claferUid);
        if (info == null) {
            throw new IllegalStateException("The JSON expects the clafer " + claferUid);
        }
        sb.append("{\"ident\":");
        string(info.ident(), sb);
        sb.append(",\"uid\":");
        string(uid, sb);
        if (info.superUid() != null) {
            sb.append(",\"super\":");
            string(info.superUid(), sb);
        }
        if (info.reference() != null) {
            sb.append(",\"reference\":");
            string(info.reference(), sb);
        }
        sb.append(",\"cardMin\":").append(info.cardMin());
        sb.append(",\"cardMax\":").append(info.cardMax());
        switch (c.value()) {
            case null -> {
            }
            case IntValue v -> sb.append(",\"value\":").append(v.value());
            case AliasValue a -> {
                sb.append(",\"value\":");
                string(a.alias().name(), sb);
            }
            case StringValue s -> {
                sb.append(",\"value\":");
                string(s.value(), sb);
            }
        }
        sb.append(',');
        elements(c.children(), infos, sb);
        sb.append('}');
    }

    private static String removeOrdinal(String uid) {
        int i = uid.indexOf('$');
        return i < 0 ? uid : uid.substring(0, i);
    }

    // a JSON string, escaped as aeson escapes it
    static void string(String s, StringBuilder sb) {
        sb.append('"');
        for (int i = 0; i < s.length(); i++) {
            char ch = s.charAt(i);
            switch (ch) {
                case '"' -> sb.append("\\\"");
                case '\\' -> sb.append("\\\\");
                case '\n' -> sb.append("\\n");
                case '\r' -> sb.append("\\r");
                case '\t' -> sb.append("\\t");
                default -> {
                    if (ch < 0x20) {
                        sb.append(String.format("\\u%04x", (int) ch));
                    } else {
                        sb.append(ch);
                    }
                }
            }
        }
        sb.append('"');
    }
}
