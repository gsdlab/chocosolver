package org.clafer.ig;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.function.UnaryOperator;
import org.clafer.ig.ClaferModel.Clafer;

/**
 * The views of the solutions, claferIG's {@code SolutionView.hs}: a static
 * model has instances, and a dynamic model has traces, whose states are
 * instances. Both are sequences of snapshots, which the full view shows one by
 * one, and the diff view shows as the changes from the previous snapshot.
 */
public final class SolutionView {

    private SolutionView() {
    }

    /**
     * The full view of the n-th solution: the instance, or each state of the
     * trace.
     */
    public static String showSolution(int n, Solution solution) {
        return switch (solution) {
            case Solution.Instance i -> "=== Instance " + n + " Begin ===\n\n" + i.model()
                    + "\n--- Instance " + n + " End ---\n\n";
            case Solution.Trace t -> "=== Trace " + n + " Begin ===\n\n" + solutionText(t)
                    + "--- Trace " + n + " End ---\n\n";
        };
    }

    /**
     * The text of a solution, as it is saved: the instance, or the states of
     * the trace.
     */
    public static String solutionText(Solution solution) {
        return switch (solution) {
            case Solution.Instance i -> i.text() != null ? i.text() : i.model().toString();
            case Solution.Trace t -> {
                StringBuilder sb = new StringBuilder();
                for (int s = 0; s < t.states().size(); s++) {
                    sb.append(stateHeader(s)).append(t.states().get(s)).append('\n');
                }
                yield sb.append(loopBack(t.loopState())).toString();
            }
        };
    }

    /**
     * The diff view of the n-th solution. The first state of a trace, whose
     * clafers all appear (+), is shown in full, and each next state as its
     * changes: the clafers that appear (+), with their children, and that
     * disappear (-), and the values that change (~), each at its depth in the
     * tree and with its least-partially-qualified name, given by the UID of its
     * clafer. The states are compared by instance, which has the same name in
     * every state. An instance is compared with the previous instance, if any,
     * by its structure, since the names of the instances of separate solutions
     * are unrelated.
     *
     * @param previous the previous solution, or null
     */
    public static String showSolutionDiff(UnaryOperator<String> names, int n, Solution previous, Solution solution) {
        return switch (solution) {
            case Solution.Instance i -> "=== Instance " + n + " Begin ===\n\n"
                    + (previous instanceof Solution.Instance p
                            ? showChanges(structureDiff(names, p.model(), i.model()))
                            : appears(i.model()))
                    + "\n--- Instance " + n + " End ---\n\n";
            case Solution.Trace t -> {
                StringBuilder sb = new StringBuilder("=== Trace " + n + " Begin ===\n\n");
                List<ClaferModel> states = t.states();
                for (int s = 0; s < states.size(); s++) {
                    sb.append(stateHeader(s)).append(s == 0 ? appears(states.get(0))
                            : showChanges(instanceDiff(names, states.get(s - 1), states.get(s)))).append('\n');
                }
                yield sb.append(loopBack(t.loopState())).append("--- Trace " + n + " End ---\n\n").toString();
            }
        };
    }

    /**
     * A snapshot without a previous one, whose clafers all appear (+),
     * aligned with the changes.
     */
    static String appears(ClaferModel model) {
        StringBuilder sb = new StringBuilder();
        for (String line : lines(model.toString())) {
            sb.append("+ ").append(line).append('\n');
        }
        return sb.toString();
    }

    /**
     * The Graphviz graph of a solution: an instance is a single node, and a
     * trace is a node per state with an edge to the next state, and a dashed
     * edge from the last state back to the loop state.
     */
    public static String solutionDot(Solution solution) {
        List<String> labels = new ArrayList<>();
        solution.models().forEach(m -> labels.add(m.toString()));
        return graphWith(labels, solution);
    }

    /**
     * The Graphviz graph of a solution in the diff view, which keeps large
     * traces readable: the first state in full, and each next state as its
     * changes from the previous one.
     */
    public static String solutionDiffDot(UnaryOperator<String> names, Solution solution) {
        if (solution instanceof Solution.Trace t) {
            List<String> labels = new ArrayList<>();
            labels.add(appears(t.states().get(0)));
            for (int s = 1; s < t.states().size(); s++) {
                labels.add(showChanges(instanceDiff(names, t.states().get(s - 1), t.states().get(s))));
            }
            return graphWith(labels, solution);
        }
        return solutionDot(solution);
    }

    private static String graphWith(List<String> labels, Solution solution) {
        // no minimum size, so that a box fits its text
        StringBuilder sb = new StringBuilder("digraph solution {\n"
                + "    node [shape=box, fontname=\"monospace\", width=0, height=0, margin=\"0.08,0.04\"];\n");
        switch (solution) {
            case Solution.Instance i -> sb.append(dotNode("instance", "", String.join("", labels)));
            case Solution.Trace t -> {
                sb.append("    start [shape=point, width=0.05];\n");
                for (int s = 0; s < labels.size(); s++) {
                    // the name of a state is outside its box, and it is also the tooltip of the box
                    sb.append(dotNode(stateNode(s), ", xlabel=\"" + stateNode(s) + "\"", labels.get(s)));
                }
                int last = t.states().size() - 1;
                sb.append("    start -> ").append(stateNode(0)).append(";\n");
                for (int s = 1; s <= last; s++) {
                    sb.append("    ").append(stateNode(s - 1)).append(" -> ").append(stateNode(s)).append(";\n");
                }
                sb.append("    ").append(stateNode(last)).append(" -> ").append(stateNode(t.loopState()))
                        .append(" [style=dashed, label=\"loop\"];\n");
            }
        }
        return sb.append("}\n").toString();
    }

    private static String stateNode(int i) {
        return "S" + i;
    }

    /**
     * A node with left-aligned lines, which keep the indentation of the tree,
     * and further attributes.
     */
    private static String dotNode(String name, String attributes, String text) {
        StringBuilder sb = new StringBuilder("    " + name + " [label=\"");
        for (String l : lines(text)) {
            if (l.isEmpty()) {
                continue;
            }
            for (char ch : l.toCharArray()) {
                switch (ch) {
                    case '"' -> sb.append("\\\"");
                    case '\\' -> sb.append("\\\\");
                    default -> sb.append(ch);
                }
            }
            sb.append("\\l");
        }
        return sb.append('"').append(attributes).append("];\n").toString();
    }

    static String stateHeader(int i) {
        return "--- State " + i + " ---\n";
    }

    static String loopBack(int loopState) {
        return "--- Loop back to state " + loopState + " ---\n\n";
    }

    /**
     * Haskell's {@code lines}.
     */
    static List<String> lines(String s) {
        List<String> out = new ArrayList<>();
        int start = 0;
        while (start < s.length()) {
            int end = s.indexOf('\n', start);
            if (end < 0) {
                out.add(s.substring(start));
                break;
            }
            out.add(s.substring(start, end));
            start = end + 1;
        }
        return out;
    }

    /**
     * A clafer of an instance: the names on the path from a top-level clafer
     * to it, which identify it, its name to show and its value.
     */
    record Entry(List<String> path, String name, String value) {
    }

    private static final Comparator<Entry> EntryOrder = (a, b) -> {
        for (int i = 0; i < Math.min(a.path().size(), b.path().size()); i++) {
            int c = a.path().get(i).compareTo(b.path().get(i));
            if (c != 0) {
                return c;
            }
        }
        int c = Integer.compare(a.path().size(), b.path().size());
        if (c != 0) {
            return c;
        }
        c = a.name().compareTo(b.name());
        return c != 0 ? c : a.value().compareTo(b.value());
    };

    private static List<Entry> entries(UnaryOperator<String> names, ClaferModel model) {
        List<Entry> out = new ArrayList<>();
        model.topLevel().forEach(c -> entries(names, List.of(), c, out));
        return out;
    }

    private static void entries(UnaryOperator<String> names, List<String> path, Clafer c, List<Entry> out) {
        List<String> path2 = new ArrayList<>(path);
        path2.add(c.id().name());
        out.add(new Entry(List.copyOf(path2), displayName(names, c),
                c.value() == null ? "" : ClaferModel.showValue(c.value())));
        c.children().forEach(x -> entries(names, path2, x, out));
    }

    // the instance's name with the least-partially-qualified name of its clafer: green$2 : Light
    // -> normal.green$2 : Light
    private static String displayName(UnaryOperator<String> names, Clafer c) {
        String lpq = names.apply(c.uid());
        String last = lpq.substring(lpq.lastIndexOf('.') + 1);
        return c.id().name().startsWith(last) ? lpq + c.id().name().substring(last.length()) : c.id().name();
    }

    private sealed interface Change {

        Entry entry();
    }

    private record Added(Entry entry) implements Change {
    }

    private record Removed(Entry entry) implements Change {
    }

    private record Changed(Entry entry) implements Change {
    }

    /**
     * The changes, each as in the tree of the instance, after its marker.
     */
    private static String showChanges(List<Change> changes) {
        if (changes.isEmpty()) {
            return "(no changes)\n";
        }
        StringBuilder sb = new StringBuilder();
        for (Change ch : changes) {
            char marker = switch (ch) {
                case Added a -> '+';
                case Removed r -> '-';
                case Changed c -> '~';
            };
            Entry e = ch.entry();
            sb.append(marker).append(' ').append(" ".repeat(2 * (e.path().size() - 1))).append(e.name())
                    .append(e.value()).append('\n');
        }
        return sb.toString();
    }

    /**
     * The changes from one state to the next: an instance is the same in both
     * if it has the same path. A removed clafer is shown without its children,
     * which disappear with it.
     */
    private static List<Change> instanceDiff(UnaryOperator<String> names, ClaferModel previous, ClaferModel current) {
        List<Entry> before = entries(names, previous);
        List<Entry> after = entries(names, current);
        List<List<String>> previousPaths = before.stream().map(Entry::path).toList();
        List<List<String>> currentPaths = after.stream().map(Entry::path).toList();
        List<List<String>> removedPaths = previousPaths.stream().filter(p -> !currentPaths.contains(p)).toList();
        List<Change> out = new ArrayList<>();
        for (Entry e : before) {
            if (!currentPaths.contains(e.path())
                    && properPrefixes(e.path()).stream().noneMatch(removedPaths::contains)) {
                out.add(new Removed(e));
            }
        }
        for (Entry e : after) {
            for (Entry p : before) {
                if (p.path().equals(e.path())) {
                    if (!p.value().equals(e.value())) {
                        out.add(new Changed(e));
                    }
                    break;
                }
            }
        }
        for (Entry e : after) {
            if (!previousPaths.contains(e.path())) {
                out.add(new Added(e));
            }
        }
        return out;
    }

    /**
     * The changes from one instance to another of a static model, by their
     * structure: the clafers without the numbers of their instances, with their
     * values, as multisets.
     */
    private static List<Change> structureDiff(UnaryOperator<String> names, ClaferModel previous,
            ClaferModel current) {
        List<Entry> before = entries(names, previous).stream().map(SolutionView::anonymous).toList();
        List<Entry> after = entries(names, current).stream().map(SolutionView::anonymous).toList();
        List<Change> out = new ArrayList<>();
        minus(before, after).stream().sorted(EntryOrder).forEach(e -> out.add(new Removed(e)));
        minus(after, before).stream().sorted(EntryOrder).forEach(e -> out.add(new Added(e)));
        return out;
    }

    // Haskell's \\: the first occurrence of each element of ys is removed from xs
    private static List<Entry> minus(List<Entry> xs, List<Entry> ys) {
        List<Entry> out = new ArrayList<>(xs);
        for (Entry y : ys) {
            out.remove(y);
        }
        return out;
    }

    private static Entry anonymous(Entry e) {
        return new Entry(e.path().stream().map(SolutionView::dropOrdinal).toList(), dropOrdinal(e.name()),
                dropOrdinal(e.value()));
    }

    // name$2 : Super -> name : Super
    private static String dropOrdinal(String name) {
        int i = name.indexOf('$');
        if (i < 0) {
            return name;
        }
        int j = i + 1;
        while (j < name.length() && Character.isDigit(name.charAt(j))) {
            j++;
        }
        return name.substring(0, i) + name.substring(j);
    }

    /**
     * The paths of the ancestors, the outermost first.
     */
    private static List<List<String>> properPrefixes(List<String> xs) {
        List<List<String>> out = new ArrayList<>();
        for (int i = 1; i < xs.size(); i++) {
            out.add(xs.subList(0, i));
        }
        return out;
    }
}
