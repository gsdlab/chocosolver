package org.clafer.alloy;

import java.util.ArrayList;
import java.util.List;

/**
 * Text in chunks, each chunk with the part of the model it comes from, so
 * that the positions in the Alloy text can be mapped back to the model's
 * constraints: a port of the compiler's {@code Generator/Concat.hs}, function
 * by function. The empty chunks are structural, as in the compiler: whether a
 * chunk is null ({@link #isNull}) depends on its shape, not on its text.
 */
sealed interface Concat permits Concat.CString, Concat.Cat {

    /**
     * Plain text.
     */
    record CString(String text) implements Concat {
    }

    /**
     * Chunks from one part of the model, the compiler's {@code Concat}.
     */
    record Cat(IrTrace srcPos, List<Concat> nodes) implements Concat {

        public Cat {
            nodes = List.copyOf(nodes);
        }
    }

    /**
     * Where a chunk comes from, the compiler's {@code IrTrace}.
     */
    sealed interface IrTrace permits NoTrace, IrPExp, LowerCard, UpperCard, ExactCard {
    }

    enum NoTrace implements IrTrace {
        NoTrace
    }

    /**
     * A constraint or assertion, by its expression's identity.
     */
    record IrPExp(Object pUid) implements IrTrace {
    }

    record LowerCard(String pUid, boolean isGroup) implements IrTrace {
    }

    record UpperCard(String pUid, boolean isGroup) implements IrTrace {
    }

    record ExactCard(String pUid, boolean isGroup) implements IrTrace {
    }

    static Concat cs(String text) {
        return new CString(text);
    }

    static Concat cat(IrTrace trace, List<Concat> nodes) {
        return new Cat(trace, nodes);
    }

    static String flatten(Concat c) {
        StringBuilder out = new StringBuilder();
        flatten(c, out);
        return out.toString();
    }

    private static void flatten(Concat c, StringBuilder out) {
        switch (c) {
            case CString s -> out.append(s.text());
            case Cat k -> k.nodes().forEach(n -> flatten(n, out));
        }
    }

    /**
     * The compiler's {@code (+++)}.
     */
    static Concat plus(Concat x, Concat y) {
        if (x instanceof CString a && y instanceof CString b) {
            return cs(a.text() + b.text());
        }
        if (x instanceof CString a && a.text().isEmpty() && y instanceof Cat) {
            return y;
        }
        if (y instanceof Cat k) {
            if (k.srcPos() == NoTrace.NoTrace) {
                List<Concat> nodes = new ArrayList<>();
                nodes.add(x);
                nodes.addAll(k.nodes());
                return cat(NoTrace.NoTrace, nodes);
            }
            return cat(NoTrace.NoTrace, List.of(x, y));
        }
        // x is a Cat, y is a CString
        Cat k = (Cat) x;
        if (((CString) y).text().isEmpty()) {
            return x;
        }
        if (k.srcPos() == NoTrace.NoTrace) {
            List<Concat> nodes = new ArrayList<>(k.nodes());
            nodes.add(y);
            return cat(NoTrace.NoTrace, nodes);
        }
        return cat(NoTrace.NoTrace, List.of(x, y));
    }

    /**
     * {@code cconcat = foldr (+++) (CString "")}.
     */
    static Concat cconcat(List<Concat> xs) {
        Concat acc = cs("");
        for (int i = xs.size() - 1; i >= 0; i--) {
            acc = plus(xs.get(i), acc);
        }
        return acc;
    }

    static Concat cconcat(Concat... xs) {
        return cconcat(List.of(xs));
    }

    static Concat cintercalate(Concat delim, List<Concat> xs) {
        List<Concat> out = new ArrayList<>();
        for (int i = 0; i < xs.size(); i++) {
            if (i > 0) {
                out.add(delim);
            }
            out.add(xs.get(i));
        }
        return cconcat(out);
    }

    static boolean isNull(Concat c) {
        return switch (c) {
            case CString s -> s.text().isEmpty();
            case Cat k -> k.nodes().isEmpty();
        };
    }

    static List<Concat> filterNull(List<Concat> xs) {
        return xs.stream().filter(x -> !isNull(x)).toList();
    }

    static Concat cunlines(List<Concat> xs) {
        return cconcat(xs.stream().map(x -> plus(x, cs("\n"))).toList());
    }
}
