package org.clafer.ig;

import java.util.ArrayList;
import java.util.List;

/**
 * An instance of a model, as claferIG's {@code ClaferModel.hs} has it: its
 * top-level clafers, each with its children. A state of a trace is one too.
 *
 * @param topLevel the top-level clafers
 */
public record ClaferModel(List<Clafer> topLevel) {

    public ClaferModel {
        topLevel = List.copyOf(topLevel);
    }

    /**
     * The tuple of name and ordinal is globally unique.
     */
    public record Id(String name, int ordinal) implements Comparable<Id> {

        @Override
        public int compareTo(Id o) {
            int c = name.compareTo(o.name);
            return c != 0 ? c : Integer.compare(ordinal, o.ordinal);
        }
    }

    /**
     * The value of a clafer: a clafer it refers to, an integer or a string.
     */
    public sealed interface Value {
    }

    public record AliasValue(Id alias) implements Value {
    }

    public record IntValue(int value) implements Value {
    }

    public record StringValue(String value) implements Value {
    }

    /**
     * An instance of a clafer: its name, the UID of its clafer, its value
     * (or null) and its children.
     */
    public record Clafer(Id id, String uid, Value value, List<Clafer> children) {

        public Clafer {
            children = List.copyOf(children);
        }

        public String name() {
            return id.name();
        }
    }

    /**
     * All the clafers, each before its children.
     */
    public List<Clafer> traverse() {
        List<Clafer> out = new ArrayList<>();
        topLevel.forEach(c -> traverse(c, out));
        return out;
    }

    private static void traverse(Clafer c, List<Clafer> out) {
        out.add(c);
        c.children().forEach(x -> traverse(x, out));
    }

    /**
     * The text of the instance: each clafer on a line, indented by its depth.
     */
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        topLevel.forEach(c -> show(c, "", sb));
        return sb.toString();
    }

    /**
     * The text of a clafer and its children, as {@code show} gives it.
     */
    public static String show(Clafer c) {
        StringBuilder sb = new StringBuilder();
        show(c, "", sb);
        return sb.toString();
    }

    private static void show(Clafer c, String indent, StringBuilder sb) {
        sb.append(indent).append(c.id().name());
        if (c.value() != null) {
            sb.append(showValue(c.value()));
        }
        sb.append('\n');
        c.children().forEach(x -> show(x, indent + "  ", sb));
    }

    static String showValue(Value v) {
        return switch (v) {
            case AliasValue a -> " -> " + a.alias().name();
            case IntValue i -> " -> " + i.value();
            case StringValue s -> " -> " + s.value();
        };
    }
}
