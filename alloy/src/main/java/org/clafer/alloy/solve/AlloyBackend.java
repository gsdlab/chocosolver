package org.clafer.alloy.solve;

import edu.mit.csail.sdg.alloy4.Err;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.TreeMap;
import java.util.function.Supplier;
import java.util.function.UnaryOperator;
import org.clafer.alloy.AlloyGenerator;
import org.clafer.alloy.solve.Constraints.ClaferInfo;
import org.clafer.alloy.solve.Constraints.Constraint;
import org.clafer.alloy.solve.Constraints.ConstraintInfo;
import org.clafer.alloy.solve.Constraints.ExactCardinalityConstraint;
import org.clafer.alloy.solve.Constraints.LowerCardinalityConstraint;
import org.clafer.alloy.solve.Constraints.Range;
import org.clafer.alloy.solve.Constraints.UpperCardinalityConstraint;
import org.clafer.alloy.solve.Constraints.UserConstraint;
import org.clafer.frontend.Symbols.ClaferSymbol;
import org.clafer.frontend.ToIr;
import org.clafer.ig.Backend;
import org.clafer.ig.Repl;
import org.clafer.ig.Solution;
import org.clafer.ig.SolutionView;

/**
 * The Alloy backend of the solution loop, for a temporal model: claferIG's
 * {@code ClaferIG.hs}, and its commands of {@code CommandLine.hs}, on an
 * {@link AlloySession} in this JVM. The model is translated by the
 * frontend's port of the compiler's AlloyLtl generator, with {@code -k}, as
 * claferIG has the compiler translate it; the code is given to Alloy in
 * memory.
 */
public final class AlloyBackend implements Backend {

    /**
     * What claferIG takes from its command line.
     *
     * @param bitwidth {@code --bitwidth}, 4 by default
     * @param useUids {@code --useuids}
     * @param addTypes {@code --addtypes}
     */
    public record Options(int bitwidth, boolean useUids, boolean addTypes) {

        public static final Options Default = new Options(4, false, false);
    }

    /**
     * Reads the model again ({@code r}), with the errors of the model as the
     * messages of an exception.
     */
    @FunctionalInterface
    public interface Loader {

        ToIr.Result load() throws Exception;
    }

    private final Loader loader;
    private final Options options;
    private final String modelFileName;
    private final AlloySession session = new AlloySession();
    // the environment of the model, ClaferIGEnv
    private ToIr.Result ir;
    private AlloyGenerator.Translation translation;
    private List<Constraint> constraints;
    private AlloyXml.Naming naming;
    private Map<String, String> alloyNames;
    private Map<String, String> uids;
    private Map<String, ClaferInfo> claferInfos;
    private Set<String> emptySigs;
    private Map<String, List<String>> paths;
    private int bitwidth;
    private boolean dynamic;
    // AlloyIGInterface's AlloyIGEnv
    private final Map<String, AlloySession.SigInfo> sigMap = new TreeMap<>();
    private final Map<String, Integer> scopes = new TreeMap<>();
    private int globalScope;

    /**
     * Loads the model, as claferIG's {@code load} does.
     *
     * @param modelFileName the name of the model's file, which names the
     * saved files
     */
    public AlloyBackend(Loader loader, String modelFileName, Options options) throws Exception {
        this.loader = loader;
        this.modelFileName = modelFileName;
        this.options = options;
        this.bitwidth = options.bitwidth();
        load();
    }

    private void load() throws Exception {
        ir = loader.load();
        translation = AlloyGenerator.translate(ir.model(), ir.scopes().orElseThrow().clafers(),
                ir.model().options().traceLength().orElse(10));
        // the integer range of the options (--maxint, --minint) raises the bitwidth
        int bw = bitwidth;
        var o = ir.files().options();
        if (o.maxInt().isPresent()) {
            bw = Math.max(bw, requiredBitwidth(o.maxInt().getAsLong()));
        }
        if (o.minInt().isPresent()) {
            bw = Math.max(bw, requiredBitwidth(-o.minInt().getAsLong() - 1));
        }
        alloyNames = new HashMap<>(translation.alloyNames());
        uids = new HashMap<>();
        alloyNames.forEach((u, a) -> uids.put(a, u));
        claferInfos = new HashMap<>();
        Map<String, AlloyXml.ClaferInfo> namingInfos = new HashMap<>();
        paths = new LinkedHashMap<>();
        for (ClaferSymbol c : ir.names().clafers()) {
            String a = alloyName(c.uid());
            var card = c.card();
            claferInfos.put(a, new ClaferInfo(c.name(), new Constraints.Cardinality(card.low(), card.high())));
            String reference = c.reference().flatMap(r -> r.primitive()).orElse(null);
            namingInfos.put(a, new AlloyXml.ClaferInfo(c.name(),
                    c.superClafer().map(s -> alloyName(s.uid())).orElse(null), reference));
            List<String> path = new ArrayList<>();
            for (Optional<ClaferSymbol> p = Optional.of(c); p.isPresent(); p = p.get().parent()) {
                path.add(0, p.get().name());
            }
            paths.put(c.uid(), path);
        }
        naming = new AlloyXml.Naming(namingInfos, this::toUid, options.useUids(), options.addTypes(),
                translation.strings());
        constraints = Constraints.parseConstraints(ir.files(), claferInfos, translation.mapping());
        emptySigs = new HashSet<>();
        ir.scopes().orElseThrow().clafers().forEach((u, s) -> {
            if (s == 0) {
                emptySigs.add("this/" + alloyName(u));
            }
        });
        AlloySession.Loaded loaded = session.load(translation.code());
        dynamic = session.isDynamic();
        sigMap.clear();
        scopes.clear();
        loaded.sigs().forEach(s -> sigMap.put(s.label(), s));
        globalScope = loaded.globalScope();
        for (AlloySession.SigInfo s : sigMap.values()) {
            setScopeCommand(s.label(), s.startingScope().orElse(1));
        }
        setBitwidth(bw);
    }

    private String alloyName(String uid) {
        return alloyNames.getOrDefault(uid, uid);
    }

    private String toUid(String alloyName) {
        return uids.getOrDefault(alloyName, alloyName);
    }

    // ----------------------------------------------------------------------
    // AlloyIGInterface
    // ----------------------------------------------------------------------
    private static boolean withinRange(int scope, String multiplicity) {
        return switch (multiplicity) {
            case "One" -> scope == 1;
            case "Lone" -> scope == 0 || scope == 1;
            case "Some" -> scope >= 1;
            default -> true;
        };
    }

    // sendSetScopeCommand: the subset sig, if it is one, whose scope is not set
    private Optional<String> setScopeCommand(String sig, int scope) throws Err {
        AlloySession.SigInfo info = sigMap.get(sig);
        if (!info.subset().isEmpty()) {
            return Optional.of(info.subset());
        }
        if (withinRange(scope, info.multiplicity())) {
            session.setScope(sig, scope);
        }
        scopes.put(sig, scope);
        return Optional.empty();
    }

    private int valueOfScope(String sig) {
        return scopes.getOrDefault(sig, globalScope);
    }

    private void setGlobalScope(int scope) {
        session.setGlobalScope(scope);
        globalScope = scope;
    }

    /**
     * Sets the global scope before the first solution, as {@code S} does
     * without solving or printing, e.g., for clafersg's {@code --scope}.
     */
    public void setInitialGlobalScope(int value) {
        int scope = Math.max(1, value);
        setGlobalScope(scope);
        if (scope > allowedMaxInt(bitwidth)) {
            setBitwidth(requiredBitwidth(scope));
        }
    }

    private void setBitwidth(int bw) {
        bitwidth = bw;
        session.setBitwidth(bw);
    }

    // ----------------------------------------------------------------------
    // ClaferIG
    // ----------------------------------------------------------------------
    private boolean hasNoInstances(String sig) {
        return emptySigs.contains(sig);
    }

    // setAlloyScope: the clafer's name of the subset sig, if it is one
    private Optional<String> setAlloyScope(int value, String sig) throws Err {
        return setScopeCommand(sig, hasNoInstances(sig) ? 0 : value).map(this::claferNameOfSig);
    }

    private String claferNameOfSig(String sig) {
        String name = sig.startsWith("this/") ? sig.substring(5) : sig;
        AlloyXml.ClaferInfo info = naming.clafers().get(name);
        return info == null ? name : info.ident();
    }

    private String sigOfUID(String uid) {
        return "this/" + alloyName(uid);
    }

    private String uidOfSig(String sig) {
        return toUid(sig.startsWith("this/") ? sig.substring(5) : sig);
    }

    /**
     * {@code getUIDs}: the clafers that a name names.
     */
    private List<String> getUIDs(String qName) {
        if (paths.containsKey(qName)) {
            return List.of(qName);
        }
        boolean fromRoot;
        List<String> names;
        if (qName.startsWith("::")) {
            fromRoot = true;
            names = List.of(qName.substring(2).split("::", -1));
        } else if (qName.contains("::")) {
            fromRoot = false;
            names = List.of(qName.split("::", -1));
        } else {
            List<String> parts = List.of(qName.split("__", -1));
            if (parts.size() > 1 && parts.get(0).equals("root")) {
                fromRoot = true;
                names = parts.subList(1, parts.size());
            } else {
                fromRoot = false;
                names = parts;
            }
        }
        if (names.isEmpty() || qName.isEmpty()) {
            return List.of();
        }
        List<String> out = new ArrayList<>();
        paths.forEach((uid, path) -> {
            if (fromRoot ? path.equals(names)
                    : path.size() >= names.size() && path.subList(path.size() - names.size(), path.size()).equals(names)) {
                out.add(uid);
            }
        });
        return out;
    }

    private Solution toSolution(String xml) {
        return AlloyXml.toSolution(xml, dynamic, naming);
    }

    private Shown shown(Optional<String> xml) {
        return xml.map(x -> new Shown(toSolution(x), x)).orElse(null);
    }

    private List<Optional<Constraint>> findRemovable(List<int[]> core) {
        Set<String> absIDs = new HashSet<>();
        for (ClaferSymbol c : ir.names().clafers()) {
            if (c.isAbstract()) {
                absIDs.add(c.name());
            }
        }
        List<Optional<Constraint>> out = new ArrayList<>();
        for (int[] c : core) {
            Range r = new Range(c[0], c[1], c[2], c[3]);
            Optional<Constraint> found = constraints.stream().filter(k -> k.range().equals(r)).findFirst();
            if (found.isPresent() && found.get() instanceof UpperCardinalityConstraint u
                    && u.claferInfo().cardinality().lower() == 0 && u.claferInfo().cardinality().upper() == 0
                    && absIDs.contains(u.claferInfo().uniqueId())) {
                continue;
            }
            out.add(found);
        }
        return out;
    }

    private static Optional<Constraint> first(List<Optional<Constraint>> cs) {
        return cs.stream().filter(Optional::isPresent).map(Optional::get).findFirst();
    }

    private static List<Constraint> present(List<Optional<Constraint>> cs) {
        return cs.stream().filter(Optional::isPresent).map(Optional::get).toList();
    }

    /**
     * {@code next}: the next solution, or the unsat core of the model with a
     * near-miss, or none.
     */
    @Override
    public Shown next(Repl.Io io) throws Exception {
        Optional<String> xml = session.next();
        if (xml.isPresent()) {
            return shown(xml);
        }
        session.saveState();
        // Generating counterexample modifies the state. If we ever want to
        // rerun the original model, we need to restore the state.
        List<int[]> core = session.unsatCore();
        List<Constraint> removed = new ArrayList<>();
        List<Constraint> unsatCore = null;
        Shown nearMiss = null;
        while (true) {
            Optional<Constraint> remove = first(findRemovable(core));
            if (remove.isEmpty()) {
                // It is possible that none of the constraints are removable
                break;
            }
            Range r = remove.get().range();
            session.removeConstraint((int) r.line(), (int) r.column(), (int) r.endLine(), (int) r.endColumn());
            session.resolve();
            Optional<String> near = session.next();
            removed.add(remove.get());
            if (near.isPresent()) {
                unsatCore = present(findRemovable(core));
                nearMiss = shown(near);
                break;
            }
            core = session.unsatCore();
        }
        session.restoreState();
        if (unsatCore == null) {
            io.err().println(dynamic
                    ? "No more traces found. Try increasing the scopes or the maximum trace length ('tl') to get more traces (type 'h' for help)."
                    : "No more instances found. Try increasing the scopes to get more instances (type 'h' for help).");
            return null;
        }
        io.err().println("No instances found.");
        io.err().println("The following set of constraints cannot be satisfied in the current scope.");
        io.err().println("Consider removing one of the constraints from the model.");
        io.err().println("Use the setUnsatCoreMinimization command to minimize the set of constraints below.");
        io.err().println("Try increasing the scopes to get an instance (type 'h' for help).");
        int i = 1;
        for (Constraint c : unsatCore) {
            io.err().println("  " + i++ + ") " + printConstraint(c));
        }
        io.err().println("Altering the following constraints produced the following near-miss example:");
        printTransformations(io, removed);
        io.err().flush();
        io.out().println(SolutionView.solutionText(nearMiss.solution()));
        return null;
    }

    private static String printConstraint(Constraint c) {
        return switch (c) {
            case UserConstraint u -> u.constraintInfo().toString();
            case ExactCardinalityConstraint e -> e.claferInfo().toString();
            case LowerCardinalityConstraint l -> l.claferInfo().toString();
            case UpperCardinalityConstraint u -> u.claferInfo().toString();
        };
    }

    private static ClaferInfo withCard(ClaferInfo info, long lower, long upper) {
        return new ClaferInfo(info.uniqueId(), new Constraints.Cardinality(lower, upper));
    }

    private void printTransformations(Repl.Io io, List<Constraint> cs) {
        List<Constraint> rest = new ArrayList<>(cs);
        int i = 1;
        while (!rest.isEmpty()) {
            Constraint c = rest.remove(0);
            String text = switch (c) {
                case UserConstraint u -> "removed " + u.constraintInfo().syntax();
                case ExactCardinalityConstraint e -> e.claferInfo() + " changed to " + withCard(e.claferInfo(), 0, -1);
                case LowerCardinalityConstraint l -> {
                    ClaferInfo info = l.claferInfo();
                    // Deleted the lower and upper constraint, or only the lower one
                    yield deleteOther(rest, info, UpperCardinalityConstraint.class)
                            ? info + " changed to " + withCard(info, 0, -1)
                            : info + " changed to " + withCard(info, 0, info.cardinality().upper());
                }
                case UpperCardinalityConstraint u -> {
                    ClaferInfo info = u.claferInfo();
                    yield deleteOther(rest, info, LowerCardinalityConstraint.class)
                            ? info + " changed to " + withCard(info, 0, -1)
                            : info + " changed to " + withCard(info, info.cardinality().lower(), -1);
                }
            };
            io.err().println("  " + i++ + ") " + text);
        }
    }

    // findAndDelete in the cardinality constraints of the other kind: the rest without the first one of the
    // same clafer, if any (the rest keeps only that kind, as the compiler's filter gives it)
    private static boolean deleteOther(List<Constraint> rest, ClaferInfo info, Class<?> kind) {
        List<Constraint> ofKind = rest.stream().filter(kind::isInstance).toList();
        for (int i = 0; i < ofKind.size(); i++) {
            ClaferInfo other = ofKind.get(i) instanceof LowerCardinalityConstraint l ? l.claferInfo()
                    : ((UpperCardinalityConstraint) ofKind.get(i)).claferInfo();
            if (other.uniqueId().equals(info.uniqueId())) {
                List<Constraint> left = new ArrayList<>(ofKind);
                left.remove(i);
                rest.clear();
                rest.addAll(left);
                return true;
            }
        }
        return false;
    }

    @Override
    public Shown fork(Repl.Io io, int state) throws Exception {
        Shown s = shown(session.fork(state));
        if (s == null) {
            io.out().println("No other trace has the states of the last trace before state " + state + ".");
        }
        return s;
    }

    @Override
    public Shown nextConfiguration(Repl.Io io) throws Exception {
        Shown s = shown(session.nextConfiguration());
        if (s == null) {
            io.out().println("No solution has another configuration.");
        }
        return s;
    }

    @Override
    public void check(Repl.Io io, Integer line, Repl.Displayer display) throws Exception {
        List<String[]> assertions = new ArrayList<>();
        List<ConstraintInfo> infos = new ArrayList<>();
        List<String> text = Constraints.text(ir.files());
        for (AlloyGenerator.Assertion a : translation.assertions()) {
            ConstraintInfo info = Constraints.constraintInfoOf(ir.files(), text, a.span());
            if (line == null || info.isOnLine(line)) {
                assertions.add(new String[]{a.name()});
                infos.add(info);
            }
        }
        if (assertions.isEmpty()) {
            io.out().println(line == null ? "The model has no assertions."
                    : "The model has no assertion on line " + line + ".");
            return;
        }
        int failed = 0;
        for (int i = 0; i < assertions.size(); i++) {
            Optional<String> xml = session.check(assertions.get(i)[0]);
            if (xml.isEmpty()) {
                io.out().println("No counterexample: " + infos.get(i));
            } else {
                io.out().println("Counterexample of: " + infos.get(i));
                display.display(new Shown(toSolution(xml.get()), xml.get()));
                failed++;
            }
        }
        String bounds = dynamic ? "the scopes and the maximum trace length" : "the scopes";
        io.out().println(assertions.size() + " assertion(s) checked within " + bounds + ": " + failed
                + " with a counterexample.");
    }

    // ----------------------------------------------------------------------
    // the commands of CommandLine.hs
    // ----------------------------------------------------------------------
    /**
     * The bitwidth required to store the given integer.
     */
    public static int requiredBitwidth(long n) {
        return (int) Math.ceil(Math.log(2.0 * n + 1) / Math.log(2));
    }

    /**
     * The largest integer allowed by the given bitwidth.
     */
    static long allowedMaxInt(int bw) {
        return (1L << (bw - 1)) - 1;
    }

    private static long allowedMinInt(int bw) {
        return -(allowedMaxInt(bw) + 1);
    }

    private void printBitwidthIntRange(Repl.Io io, int bw) {
        io.out().println("Integer range = " + allowedMinInt(bw) + ".." + allowedMaxInt(bw) + " (bitwidth = " + bw
                + ")");
    }

    private void printBitwidthWarning(Repl.Io io, int bw) {
        if (bw > 9) {
            io.out().println("Warning! This is a very large bitwidth for Alloy, which may cause slow down.");
        }
    }

    /**
     * {@code findNecessaryBitwidth}.
     */
    private int findNecessaryBitwidth(int oldBw, List<Integer> scopeValues) {
        long max = Math.max(scopeValues.stream().mapToLong(Integer::longValue).max().orElse(0), translation.maxInt());
        int newBw = (int) Math.ceil(Math.log(2.0 * max + 1) / Math.log(2));
        return Math.max(newBw, oldBw);
    }

    private void solve() throws Err {
        session.resolve();
    }

    @Override
    public void start(Repl.Io io) throws Exception {
        io.out().println("Compiling the Clafer model...");
        prepare();
        io.out().println("Type 'h' for the list of available REPL commands\n");
    }

    /**
     * Sets the bitwidth that the scopes need and solves the model, as
     * {@link #start} does without its messages: before the first
     * {@link #next}.
     */
    public void prepare() throws Exception {
        setBitwidth(findNecessaryBitwidth(bitwidth, new ArrayList<>(scopes.values())));
        solve();
    }

    @Override
    public boolean isDynamic() {
        return dynamic;
    }

    @Override
    public String prompt() {
        return "claferIG> ";
    }

    @Override
    public int number(Repl.Context context) {
        return 1 + context.unsavedCount();
    }

    @Override
    public String show(int n, Shown shown) {
        return SolutionView.showSolution(n, shown.solution()) + "\n";
    }

    @Override
    public UnaryOperator<String> names() {
        // the least-partially-qualified name is the UID, with dots for the compiler's ::
        return uid -> uid;
    }

    @Override
    public String modelFileName() {
        return modelFileName;
    }

    @Override
    public void quit() {
    }

    @Override
    public void unknown(Repl.Io io, String input) {
        io.out().println("Unhandled command: " + input.trim());
    }

    private static Integer integer(String s) {
        try {
            return Integer.valueOf(s);
        } catch (NumberFormatException e) {
            return null;
        }
    }

    private static Integer natural(String s) {
        Integer n = integer(s);
        return n != null && n >= 0 && !s.startsWith("+") && !s.startsWith("-") ? n : null;
    }

    @Override
    public boolean command(Repl.Io io, String input, List<String> words, Repl.Context context) throws Exception {
        String name = words.isEmpty() ? "" : words.get(0);
        int arity = words.size() - 1;
        switch (name) {
            case "S", "SetGlobalScope" -> {
                Integer n = arity == 1 ? natural(words.get(1)) : null;
                if (n == null) {
                    io.out().println("The format of the command is: 'S <integer>'");
                    return true;
                }
                setGlobalScopeCommand(io, n);
            }
            case "I", "IncGlobalScope" -> {
                Integer n = arity == 0 ? Integer.valueOf(1) : arity == 1 ? integer(words.get(1)) : null;
                if (n == null) {
                    io.out().println("The format of the command is: 'I <integer?>'");
                    return true;
                }
                increaseGlobalScope(io, n);
            }
            case "s", "setScope" -> {
                Integer n = arity == 2 ? natural(words.get(2)) : null;
                if (n == null) {
                    io.out().println("The format of the command is: 's <claferUID> <integer>'");
                    return true;
                }
                setScopes(io, words.get(1), sig -> n);
            }
            case "i", "incScope" -> {
                Integer n = arity == 1 ? Integer.valueOf(1) : arity == 2 ? integer(words.get(2)) : null;
                if (n == null) {
                    io.out().println("The format of the command is: 'i <claferUID> <integer?>'");
                    return true;
                }
                setScopes(io, words.get(1), sig -> Math.max(1, valueOfScope(sig) + n));
            }
            case "b", "bitwidth" -> {
                Integer n = arity == 1 ? natural(words.get(1)) : null;
                if (n == null) {
                    io.out().println("The format of the command is: 'b <integer>'");
                    return true;
                }
                if (n >= 4) {
                    setBitwidth(n);
                    printBitwidthWarning(io, n);
                    solve();
                    printBitwidthIntRange(io, n);
                }
            }
            case "m", "maxInt", "maxint" -> {
                Integer n = arity == 1 ? natural(words.get(1)) : null;
                if (n == null) {
                    io.out().println("The format of the command is: 'm <integer>'");
                    return true;
                }
                if (n >= 7) {
                    int bw = requiredBitwidth(n);
                    setBitwidth(bw);
                    printBitwidthWarning(io, bw);
                    solve();
                    printBitwidthIntRange(io, bw);
                }
            }
            case "tl" -> {
                Integer n = arity == 1 ? natural(words.get(1)) : null;
                if (n == null) {
                    io.out().println("The format of the command is: 'tl <integer>'");
                    return true;
                }
                if (!dynamic) {
                    io.out().println("The model is static: its instances are not traces.");
                } else if (n < 1) {
                    io.out().println("The trace length must be at least 1.");
                } else {
                    session.setMaxTraceLength(n);
                    solve();
                    io.out().println("The maximum trace length is " + n + ".");
                }
            }
            case "r", "reload" -> reload(io);
            case "scope" -> showScopes(io);
            case "v", "saveScopes" -> {
                saveScopes();
                io.out().println("Scopes saved to the `.cfr-scope` file.");
            }
            case "loadScopes" -> loadScopes(io);
            case "a", "alloyModel" -> io.out().println(editAlloyModel());
            case "c", "claferModel" -> showClaferModel(io);
            case "setUnsatCoreMinimization" -> {
                Integer level = arity != 1 ? null : switch (words.get(1)) {
                    case "fastest" -> 2;
                    case "medium" -> 1;
                    case "best" -> 0;
                    default -> null;
                };
                if (level == null) {
                    io.out().println("The format of the command is: 'setUnsatCoreMinimization <fastest|medium|best>'");
                    return true;
                }
                session.setUnsatCoreMinimization(level);
                solve();
            }
            case "u", "unsatCore" -> {
                // clafersg's header, with the constraints of claferIG's unsat core, as 'n' lists them
                io.out().println("UNSAT Core command:");
                List<Constraint> core = present(findRemovable(session.unsatCore()));
                if (core.isEmpty()) {
                    io.out().println("No unsat core: the model has a solution within " + (dynamic
                            ? "the scopes and the maximum trace length." : "the scopes."));
                }
                int i = 1;
                for (Constraint c : core) {
                    io.out().println("  " + i++ + ") " + printConstraint(c));
                }
            }
            case "U", "minUnsat", "p", "prettify", "x", "maximize", "z", "minimize", "t", "stats",
                    "O", "Options", "g", "strategy", "o", "optimizations", "B", "symmetryBreaking" ->
                io.out().println("The command '" + name + "' is for static models, which Choco solves.");
            default -> {
                return false;
            }
        }
        return true;
    }

    private void setGlobalScopeCommand(Repl.Io io, int value) throws Err {
        int newGlobalScope = Math.max(1, value);
        setGlobalScope(newGlobalScope);
        if (newGlobalScope > allowedMaxInt(bitwidth)) {
            io.out().println("Warning! Requested global scope is larger than maximum allowed by bitwidth ... increasing bitwidth");
            setBitwidth(requiredBitwidth(newGlobalScope));
            printBitwidthWarning(io, bitwidth);
        }
        solve();
        io.out().println("Global scope set to " + newGlobalScope);
    }

    private void increaseGlobalScope(Repl.Io io, int inc) throws Err {
        int bw = bitwidth;
        int newGlobalScope = Math.max(1, globalScope + inc);
        setGlobalScope(newGlobalScope);
        if (newGlobalScope > allowedMaxInt(bw)) {
            io.out().println("Warning! Requested global scope is larger than maximum allowed by bitwidth ... increasing bitwidth");
            setBitwidth(requiredBitwidth(newGlobalScope));
            printBitwidthWarning(io, bitwidth);
        }
        // the clafers without instances keep their scopes, 0
        for (Map.Entry<String, Integer> s : new ArrayList<>(scopes.entrySet())) {
            if (!hasNoInstances(s.getKey())) {
                setAlloyScopeAndBitwidth(io, bw, Math.max(1, s.getValue() + inc), claferNameOfSig(s.getKey()),
                        s.getKey());
            }
        }
        solve();
        io.out().println("Global scope changed to " + newGlobalScope);
    }

    @FunctionalInterface
    private interface ScopeOf {

        int of(String sig);
    }

    private void setScopes(Repl.Io io, String qName, ScopeOf value) throws Err {
        int bw = bitwidth;
        for (String uid : getUIDs(qName)) {
            String sig = sigOfUID(uid);
            if (!sigMap.containsKey(sig)) {
                continue;
            }
            // the compiler's setAlloyScopeAndBitwidth ignores a subset sig, whose scope is not set
            setAlloyScopeAndBitwidth(io, bw, value.of(sig), qName, sig);
        }
        solve();
    }

    private Optional<String> setAlloyScopeAndBitwidth(Repl.Io io, int bw, int newValue, String fqName, String sig)
            throws Err {
        if (hasNoInstances(sig)) {
            io.out().println("Scope of " + fqName + " (" + sig.substring(5) + ") stays 0: it has no instances, since no"
                    + " concrete clafer inherits from it or from the abstract clafer it is in");
            return Optional.empty();
        }
        if (newValue > allowedMaxInt(bw)) {
            setBitwidth(requiredBitwidth(newValue));
            io.out().println("Warning! Requested scope for " + fqName + " is larger than maximum allowed by bitwidth"
                    + " ... increasing bitwidth");
            printBitwidthWarning(io, bitwidth);
        }
        Optional<String> subset = setAlloyScope(newValue, sig);
        io.out().println("Scope of " + fqName + " (" + sig.substring(5) + ") changed to " + newValue);
        return subset;
    }

    private void showScopes(Repl.Io io) {
        io.out().println("Global scope = " + globalScope);
        scopes.forEach((sig, value) -> io.out().println("  " + uidOfSig(sig) + " scope = " + value));
    }

    private Path scopeFile() {
        return Path.of(modelFileName + "-scope");
    }

    private void saveScopes() throws IOException {
        StringBuilder sb = new StringBuilder("[\n");
        List<String> entries = new ArrayList<>();
        entries.add("{\"lpqName\": \"\", \"scope\": " + globalScope + "}");
        scopes.forEach((sig, value) -> entries.add("{\"lpqName\": \"" + uidOfSig(sig) + "\", \"scope\": " + value
                + "}"));
        sb.append(String.join(",\n", entries)).append("\n]\n");
        Files.writeString(scopeFile(), sb.toString(), StandardCharsets.UTF_8);
    }

    private void loadScopes(Repl.Io io) throws Exception {
        if (!Files.isRegularFile(scopeFile())) {
            io.out().println("The `.cfr-scope` file does not exist. Use the command `saveScopes` to create one.");
            return;
        }
        Map<String, Integer> read = readScopeFile(Files.readString(scopeFile(), StandardCharsets.UTF_8));
        Integer global = null;
        for (Map.Entry<String, Integer> e : read.entrySet()) {
            if (e.getKey().isEmpty()) {
                global = global == null ? e.getValue() : Math.max(global, e.getValue());
            }
        }
        // a file may list only some clafers, without the global scope; then it stays
        if (global != null) {
            setGlobalScope(global);
        }
        for (Map.Entry<String, Integer> e : read.entrySet()) {
            if (!e.getKey().isEmpty()) {
                for (String uid : getUIDs(e.getKey())) {
                    String sig = sigOfUID(uid);
                    if (sigMap.containsKey(sig)) {
                        setAlloyScope(e.getValue(), sig);
                    }
                }
            }
        }
        io.out().println("Scopes loaded from the `.cfr-scope` file.");
        showScopes(io);
    }

    // the {"lpqName": ..., "scope": ...} entries of a .cfr-scope file
    private static Map<String, Integer> readScopeFile(String text) {
        Map<String, Integer> out = new LinkedHashMap<>();
        java.util.regex.Matcher m = java.util.regex.Pattern
                .compile("\\{\\s*\"lpqName\"\\s*:\\s*\"([^\"]*)\"\\s*,\\s*\"scope\"\\s*:\\s*(-?\\d+)\\s*\\}")
                .matcher(text);
        while (m.find()) {
            out.put(m.group(1), Integer.parseInt(m.group(2)));
        }
        return out;
    }

    private void reload(Repl.Io io) throws Exception {
        Map<String, Integer> oldScopes = new TreeMap<>(scopes);
        int oldBw = bitwidth;
        int oldGlobal = globalScope;
        try {
            load();
            setGlobalScope(oldGlobal);
        } catch (Exception e) {
            io.err().println(e.getMessage());
        }
        // mergeScopes: the larger of the old and the new scope of each clafer
        for (Map.Entry<String, Integer> s : new ArrayList<>(scopes.entrySet())) {
            int old = oldScopes.getOrDefault(s.getKey(), s.getValue());
            setAlloyScope(Math.max(s.getValue(), old), s.getKey());
        }
        setBitwidth(findNecessaryBitwidth(oldBw, new ArrayList<>(scopes.values())));
        printBitwidthWarning(io, bitwidth);
        solve();
    }

    /**
     * {@code c}: the Clafer model, each line with its number in its file,
     * the lines of the unsat core marked, and the scope of the clafer
     * declared on the line, as claferIG's {@code ShowClaferModel}.
     */
    private void showClaferModel(Repl.Io io) {
        printBitwidthIntRange(io, bitwidth);
        io.out().println("Global scope  = " + globalScope + "\n");
        // the constraints of the unsat core: a constraint on the line where it starts, a cardinality by its text
        List<Integer> unsatLines = new ArrayList<>();
        List<String> unsatTexts = new ArrayList<>();
        for (Constraint c : present(findRemovable(session.unsatCore()))) {
            switch (c) {
                case UserConstraint u -> unsatLines.add(u.constraintInfo().pos().line());
                case ExactCardinalityConstraint e -> unsatTexts.add(e.claferInfo().toString());
                case LowerCardinalityConstraint l -> unsatTexts.add(l.claferInfo().toString());
                case UpperCardinalityConstraint u -> unsatTexts.add(u.claferInfo().toString());
            }
        }
        String text = ir.files().text();
        // the lines of the comments
        Set<Integer> commentLines = new HashSet<>();
        for (org.clafer.frontend.Token t : org.clafer.frontend.ClaferLexer.lex(text).others()) {
            if (t.kind() == org.clafer.frontend.TokenKind.COMMENT) {
                for (int l = t.span().line(); l <= t.span().endLine(); l++) {
                    commentLines.add(l);
                }
            }
        }
        // the clafer declared on each line, by its Alloy name: of two on a line, the smaller UID
        Map<Integer, String> lineNumMap = new HashMap<>();
        Map<Integer, String> lineUids = new HashMap<>();
        for (ClaferSymbol c : ir.names().clafers()) {
            int line = c.declaration().span().line();
            String other = lineUids.get(line);
            if (other == null || c.uid().compareTo(other) < 0) {
                lineUids.put(line, c.uid());
                lineNumMap.put(line, alloyName(c.uid()));
            }
        }
        List<String> claferLines = lines(removeCommentsAndUnify(text));
        int maxLineLength = claferLines.stream().mapToInt(String::length).max().orElse(0);
        int digits = Integer.toString(claferLines.size()).length();
        StringBuilder sb = new StringBuilder();
        String previousPath = null;
        for (int num = 1; num <= claferLines.size(); num++) {
            String l = claferLines.get(num - 1);
            if (commentLines.contains(num) && isEmptyLine(l)) {
                continue;
            }
            var position = ir.files().position(num);
            if (!position.path().equals(previousPath)) {
                sb.append("--- ").append(position.path()).append('\n');
                previousPath = position.path();
            }
            boolean unsat = isUnsat(unsatLines, unsatTexts, l, num);
            String fileNum = Integer.toString(position.line());
            sb.append(fileNum).append('.').append(" ".repeat(Math.max(0, 1 + digits - fileNum.length())))
                    .append(unsat ? "> " : "| ").append(l).append(" ".repeat(3 + maxLineLength - l.length()))
                    .append(unsat ? "<UnSAT " : "|      ");
            String clafer = lineNumMap.get(num);
            if (clafer != null) {
                Integer scope = scopes.get("this/" + clafer);
                sb.append("scope = ").append(scope == null ? "Nothing" : scope);
            }
            sb.append('\n');
        }
        io.out().println(sb);
    }

    // a constraint is on the line where it starts; a cardinality is on the line with the clafer's name and
    // cardinality
    private static boolean isUnsat(List<Integer> unsatLines, List<String> unsatTexts, String l, int num) {
        if (unsatLines.contains(num)) {
            return true;
        }
        List<String> lw = words(l);
        for (String u : unsatTexts) {
            List<String> uw = words(u);
            String uFirst = uw.isEmpty() ? "" : uw.get(0);
            String lFirst = lw.isEmpty() ? "" : lw.get(0);
            String uLast = uw.isEmpty() ? "" : uw.get(uw.size() - 1);
            String lLast = lw.isEmpty() ? "" : lw.get(lw.size() - 1);
            if ((uFirst.equals(lFirst) && uLast.equals(lLast)) || l.contains(u)) {
                return true;
            }
        }
        return false;
    }

    private static List<String> words(String s) {
        String t = s.strip();
        return t.isEmpty() ? List.of() : List.of(t.split("\\s+"));
    }

    private String editAlloyModel() {
        String model = translation.code();
        List<String> alloyLines = lines(removeCommentsAndUnify(model));
        int splitNum = 1;
        while (splitNum - 1 < alloyLines.size() && !isEmptyLine(alloyLines.get(splitNum - 1))) {
            splitNum++;
        }
        List<String> info = alloyLines.subList(0, Math.min(splitNum, alloyLines.size()));
        List<String> rest = alloyLines.subList(Math.min(splitNum, alloyLines.size()), alloyLines.size());
        int m = rest.stream().mapToInt(String::length).max().orElse(0);
        int digits = Integer.toString(alloyLines.size()).length();
        StringBuilder sb = new StringBuilder("global scope = " + globalScope + "\n\n");
        info.forEach(l -> sb.append(l).append('\n'));
        int num = 1 + splitNum;
        for (String l : rest) {
            String withScope;
            String pad = " ".repeat(3 + m - l.length());
            if (!List.of(l.trim().split("\\s+")).contains("sig")) {
                withScope = l + pad + " |";
            } else {
                int us = l.indexOf('_');
                String key = us < 0 ? "" : l.substring(us + 1).split("[ \t\n]", 2)[0];
                Integer val = scopes.get(key);
                withScope = l + pad + " | scope = " + (val == null ? "Nothing" : val);
            }
            sb.append(num).append('.').append(" ".repeat(1 + digits - Integer.toString(num).length())).append("| ")
                    .append(withScope).append('\n');
            num++;
        }
        return sb.toString();
    }

    private static List<String> lines(String s) {
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

    private static boolean isEmptyLine(String l) {
        return l.chars().allMatch(c -> c == ' ' || c == '\n' || c == '\t' || c == '\r');
    }

    // only remove the contents while preserving the empty lines
    private static String removeCommentsAndUnify(String model) {
        StringBuilder sb = new StringBuilder();
        int i = 0;
        int n = model.length();
        while (i < n) {
            if (i == n - 1) {
                sb.append(model.charAt(i));
                break;
            }
            char c = model.charAt(i);
            if (c == '\t') {
                sb.append("   ");
                i++;
            } else if (c == '/' && model.charAt(i + 1) == '/') {
                while (i < n && model.charAt(i) != '\n') {
                    i++;
                }
            } else if (c == '/' && model.charAt(i + 1) == '*') {
                i += 2;
                while (i < n) {
                    if (i == n - 1) {
                        i = n;
                        break;
                    }
                    if (model.charAt(i) == '\n') {
                        sb.append('\n');
                        i++;
                    } else if (model.charAt(i) == '*' && model.charAt(i + 1) == '/') {
                        i += 2;
                        break;
                    } else {
                        i++;
                    }
                }
            } else {
                sb.append(c);
                i++;
            }
        }
        return sb.toString();
    }

    @Override
    public String help() {
        return """
                You can invoke the following commands as indicated by single quotes:
                'n'ext, [enter]          - to produce the next solution if available, an instance of a static model or a trace
                                           of a dynamic model, or to output a message that no more solutions exist within
                                           the given scope
                'S'etGlobalScope <n>     - to set the global scope to <n>
                'I'ncGlobalScope <n?>    - to increase the global scope by <n>, 1 by default
                's'etScope <name> <n>    - to set the scope of the clafer <name> to <n>
                'i'ncScope <name> <n?>   - to increase the scope of the clafer <name> by <n>, 1 by default
                'm'axInt, 'maxint' <n>   - to set the bitwidth by providing the largest integer
                'b'itwidth <n>           - to set the bitwidth
                'tl' <n>                 - to set the maximum length of the traces of a dynamic model
                'd'iff                   - to show each next instance or state as its changes from the previous one, or in full
                                           (+ appears, - disappears, ~ changes its value)
                'fork' <n>               - to produce another trace with the states of the last trace before state <n>
                'config'                 - to produce a trace of a dynamic model whose clafers that do not change over time
                                           differ from those of the traces so far
                'check' <n?>             - to check the assertions of the model within the scopes and the maximum trace
                                           length, and to give a counterexample of each one that fails; with <n>, only
                                           the assertions on line <n>
                sav'e'                   - to save all solutions displayed so far to files named
                                           <model file name>.<solution number>.data, one solution per file
                'dot'                    - to save the most recent solution as a Graphviz graph, for a trace its states
                                           and transitions, to <model file name>.<solution number>.dot
                'q'uit                   - to quit the interactive session
                'r'eload                 - to reload your clafer model
                'h'elp                   - to display this menu options summary
                'scope'                  - to print out the values of the global scope and individual Clafer scopes
                sa'v'eScopes             - to generate a '<model>.cfr-scope' file with the current scopes
                'loadScopes'             - to load scopes from a '<model>.cfr-scope' file
                'setUnsatCoreMinimization' <fastest|medium|best> - to choose the UnSAT core minimization strategy. Default: fastest
                'u'nsatCore              - to print out the constraints of the unsat core of the model, if it has no solution
                'c', 'claferModel'       - to print out the Clafer model verbatim, with the scopes and the unsat core
                'a', 'alloyModel'        - to print out the Alloy model
                'alloyInstance'          - to print out the Alloy xml document of the most recent solution
                'f'ind <name>            - to print a Clafer with given name found in the most recent solution
                """;
    }
}
