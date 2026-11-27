package org.clafer.alloy.solve;

import edu.mit.csail.sdg.alloy4.A4Reporter;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.ErrorSyntax;
import edu.mit.csail.sdg.alloy4.ErrorWarning;
import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.alloy4.SafeList;
import edu.mit.csail.sdg.ast.Command;
import edu.mit.csail.sdg.ast.CommandScope;
import edu.mit.csail.sdg.ast.Decl;
import edu.mit.csail.sdg.ast.Expr;
import edu.mit.csail.sdg.ast.ExprConstant;
import edu.mit.csail.sdg.ast.ExprQt;
import edu.mit.csail.sdg.ast.Func;
import edu.mit.csail.sdg.ast.Sig;
import edu.mit.csail.sdg.parser.CompModule;
import edu.mit.csail.sdg.parser.CompUtil;
import edu.mit.csail.sdg.translator.A4Options;
import edu.mit.csail.sdg.translator.A4Solution;
import edu.mit.csail.sdg.translator.A4Tuple;
import edu.mit.csail.sdg.translator.A4TupleSet;
import edu.mit.csail.sdg.translator.TranslateAlloyToKodkod;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.OptionalInt;
import java.util.Set;
import kodkod.engine.satlab.SATFactory;
import org.alloytools.solvers.natv.minisatprover.FixedMiniSatProver;

/**
 * An Alloy model in this JVM and its solutions: claferIG's {@code AlloyIG.java},
 * whose operations are the methods here rather than messages on a pipe. A
 * solution is Alloy's XML of it, as {@code AlloyIG} sends it.
 * <p>
 * The model is parsed from its text, in memory. The solutions of a dynamic
 * model, a trace each, are solved for one by one, without the traces given so
 * far ({@link #next}); {@link #fork} and {@link #nextConfiguration} give
 * other traces, and {@link #check} the counterexample of an assertion. The
 * unsat core of the model comes from the minisat prover, claferIG's fixed
 * one ({@link FixedMiniSatProver}).
 */
public final class AlloySession {

    // The only way to detect unsatisfiability is to wait for the minimized callback to be called.
    // If it is called, then unsatisfiable. Otherwise not. See alloy4whole/SimpleReporter.java.
    private static final class Reporter extends A4Reporter {

        private int minimizedBefore;
        private int minimizedAfter;

        @Override
        public void warning(ErrorWarning msg) {
            System.err.print("Relevance Warning:\n" + (msg.toString().trim()) + "\n\n");
            System.err.flush();
        }

        @Override
        public void minimized(Object command, int before, int after) {
            minimizedBefore = before;
            minimizedAfter = after;
            super.minimized(command, before, after);
        }
    }

    /**
     * A sig of the model, as {@code load} sends it.
     *
     * @param label its label, e.g., {@code this/c0_A}
     * @param multiplicity {@code One}, {@code Lone}, {@code Some} or
     * {@code Any}
     * @param subset the type of a subset sig, without its braces, or empty
     * @param startingScope its scope in the command, if any
     */
    public record SigInfo(String label, String multiplicity, String subset, OptionalInt startingScope) {
    }

    /**
     * The sigs of the model and the global scope of its command.
     */
    public record Loaded(List<SigInfo> sigs, int globalScope) {
    }

    static {
        // Alloy logs its progress at the info level and warns whenever a native solver is garbage collected.
        if (System.getProperty("org.slf4j.simpleLogger.defaultLogLevel") == null) {
            System.setProperty("org.slf4j.simpleLogger.defaultLogLevel", "error");
        }
    }

    private final Reporter rep = new Reporter();
    private CompModule world;
    private SafeList<Sig> sigs;
    private Command command;
    // Alloy parses the model from a temporary file; positions only match with its name.
    private String modelFile = "";
    private A4Solution ans;
    // The solution of the model, as solved by the last resolve: if it is unsatisfiable, the
    // model has no instances, and its unsat core explains why.
    private A4Solution resolved;
    private boolean dynamic;
    // In a dynamic model, the traces given so far are excluded from the next solutions, and the
    // configurations given so far from the next configuration. The next trace is solved for
    // when it is asked for, after the trace shown last.
    private Expr givenTraces = ExprConstant.TRUE;
    private Expr givenConfigurations = ExprConstant.TRUE;
    private A4Solution shown;
    private boolean stale;
    // the static model gave its last instance
    private boolean exhausted;
    private final A4Options options = new A4Options();
    private Util.State<StateExtra> state;

    public AlloySession() {
        // Use fastest
        options.coreMinimization = 2;
        // The minisat prover computes unsat cores. Alloy has none for Windows, so instances come from SAT4J there.
        // The optimized build of the prover is given in the system property alloy.native.lib.minisatprover.
        SATFactory prover = new FixedMiniSatProver.Factory();
        options.solver = prover.isPresent() ? prover : SATFactory.get("sat4j");
    }

    /**
     * {@code load}: parses the model.
     *
     * @throws Err the errors of the model
     */
    public Loaded load(String model) throws Err {
        world = CompUtil.parseEverything_fromString(rep, model);
        sigs = world.getAllSigs();
        Util.splitFacts(world.getAllReachableSigs());
        dynamic = isDynamic(sigs);
        command = world.getAllCommands().get(0);
        modelFile = command.pos.filename;
        List<SigInfo> infos = new ArrayList<>();
        for (Sig sig : sigs) {
            CommandScope scope = command.getScope(sig);
            infos.add(new SigInfo(sig.label, multiplicity(sig),
                    sig instanceof Sig.PrimSig ? "" : removeCurly(sig.type().toString()),
                    scope == null ? OptionalInt.empty() : OptionalInt.of(scope.startingScope)));
        }
        return new Loaded(infos, command.overall);
    }

    /**
     * @return whether the model has mutable sigs or fields: its solutions are
     * traces
     */
    public boolean isDynamic() {
        return dynamic;
    }

    /**
     * {@code resolve}: solves the command again, from its first solution.
     */
    public void resolve() throws Err {
        rep.minimizedBefore = 0;
        rep.minimizedAfter = 0;
        givenTraces = ExprConstant.TRUE;
        givenConfigurations = ExprConstant.TRUE;
        shown = null;
        stale = false;
        exhausted = false;
        ans = TranslateAlloyToKodkod.execute_command(rep, world.getAllReachableSigs(), command, options);
        resolved = ans;
    }

    /**
     * {@code next}: the next solution, if any.
     */
    public Optional<String> next() throws Err {
        if (exhausted) {
            return Optional.empty();
        }
        if (dynamic && stale) {
            // Alloy does not enumerate the traces of a temporal model: it gives few of them,
            // of the length it found first, or none at all, depending on the model. Instead,
            // the next trace is solved for, without the traces given so far. The reporter of
            // the model stays as it is, since no more traces is not an unsat core of the model.
            ans = TranslateAlloyToKodkod.execute_command(new Reporter(), world.getAllReachableSigs(),
                    command.change(command.formula.and(givenTraces)), withoutCore(options));
            stale = false;
        }
        if (!ans.satisfiable()) {
            return Optional.empty();
        }
        String xml = toXml(ans);
        if (dynamic) {
            List<Sig.Field> fields = stateFields(world.getAllReachableSigs());
            givenTraces = givenTraces.and(sameStates(ans, visible(fields, fields), ans.getTraceLength(), true).not());
            givenConfigurations = givenConfigurations.and(
                    sameStates(ans, visible(staticFields(fields), fields), 1, false).not());
            shown = ans;
            stale = true;
        } else {
            givenConfigurations = givenConfigurations.and(
                    sameStates(ans, structure(world.getAllReachableSigs()), 1, false).not());
            shown = ans;
            A4Solution nextAns = ans.next();
            // AlloyIG quits here: the static model has no more instances
            if (nextAns == ans) {
                exhausted = true;
            }
            ans = nextAns;
        }
        return Optional.of(xml);
    }

    /**
     * {@code fork}: a trace with the same states as the trace shown last
     * before the given state, which differs from the traces given so far.
     */
    public Optional<String> fork(int forkState) throws Err {
        return other(true, forkState);
    }

    /**
     * {@code nextConfiguration}: a trace whose relations that do not change
     * over time differ from those of the traces given so far, or an instance
     * of a static model whose structure differs from those of the instances
     * given so far.
     */
    public Optional<String> nextConfiguration() throws Err {
        return other(false, 0);
    }

    private Optional<String> other(boolean fork, int forkState) throws Err {
        // Both differ from the traces given so far.
        A4Solution other = null;
        if ((dynamic || !fork) && shown != null) {
            List<Sig.Field> fields = stateFields(world.getAllReachableSigs());
            Expr formula = fork
                    ? sameStates(shown, visible(fields, fields), forkState, false)
                    : givenConfigurations;
            other = TranslateAlloyToKodkod.execute_command(new Reporter(), world.getAllReachableSigs(),
                    command.change(command.formula.and(givenTraces).and(formula)), withoutCore(options));
        }
        if (other == null || !other.satisfiable()) {
            return Optional.empty();
        }
        String xml = toXml(other);
        List<Sig.Field> fields = stateFields(world.getAllReachableSigs());
        shown = other;
        if (dynamic) {
            givenTraces = givenTraces.and(sameStates(other, visible(fields, fields), other.getTraceLength(), true).not());
            givenConfigurations = givenConfigurations.and(
                    sameStates(other, visible(staticFields(fields), fields), 1, false).not());
            ans = other;
            stale = true;
        } else {
            // the next instances continue from this one, without the configurations so far
            givenConfigurations = givenConfigurations.and(
                    sameStates(other, structure(world.getAllReachableSigs()), 1, false).not());
            ans = other.next();
            exhausted = ans == other;
        }
        return Optional.of(xml);
    }

    /**
     * {@code check}: the counterexample of the assertion with the given label,
     * at the scopes and the trace length of the model, if any. It has no unsat
     * core. The solutions of the model stay as they are.
     */
    public Optional<String> check(String assertion) throws Err {
        A4Options checkOptions = withoutCore(options);
        A4Solution counterexample = null;
        for (Command a : world.getAllCommands()) {
            if (a.check && a.label.equals(assertion)) {
                Command c = command;
                counterexample = TranslateAlloyToKodkod.execute_command(new Reporter(), world.getAllReachableSigs(),
                        new Command(a.pos, a.nameExpr, a.label, true, c.overall, c.bitwidth, c.maxseq, c.minprefix,
                                c.maxprefix, a.expects, c.scope, c.additionalExactScopes, a.commandKeyword,
                                a.formula, a.parent),
                        checkOptions);
            }
        }
        return counterexample != null && counterexample.satisfiable()
                ? Optional.of(toXml(counterexample)) : Optional.empty();
    }

    /**
     * {@code setGlobalScope}.
     */
    public void setGlobalScope(int scopeSize) {
        Command c = command;
        command = new Command(c.pos, c.nameExpr, c.label, c.check, scopeSize, c.bitwidth, c.maxseq, c.minprefix,
                c.maxprefix, c.expects, c.scope, c.additionalExactScopes, c.commandKeyword, c.formula, c.parent);
    }

    /**
     * {@code setScope}: the scope of the sig with the given label.
     */
    public void setScope(String sigName, int scopeSize) throws ErrorSyntax {
        Sig sig = Util.findSig(sigName, sigs);
        List<CommandScope> scope = setScopeSize(sig, scopeSize, command.scope);
        Command c = command;
        command = new Command(c.pos, c.nameExpr, c.label, c.check, c.overall, c.bitwidth, c.maxseq, c.minprefix,
                c.maxprefix, c.expects, scope, c.additionalExactScopes, c.commandKeyword, c.formula, c.parent);
    }

    /**
     * {@code setBitwidth}.
     */
    public void setBitwidth(int bitwidth) {
        if (bitwidth < 1) {
            throw new IllegalArgumentException(bitwidth + " is an invalid bitwidth.");
        }
        Command c = command;
        command = new Command(c.pos, c.nameExpr, c.label, c.check, c.overall, bitwidth, c.maxseq, c.minprefix,
                c.maxprefix, c.expects, c.scope, c.additionalExactScopes, c.commandKeyword, c.formula, c.parent);
    }

    /**
     * {@code setMaxTraceLength}.
     */
    public void setMaxTraceLength(int maxTraceLength) {
        if (maxTraceLength < 1) {
            throw new IllegalArgumentException(maxTraceLength + " is an invalid trace length.");
        }
        command = withTraceLength(command, Math.min(command.minprefix, maxTraceLength), maxTraceLength);
    }

    /**
     * {@code unsatCoreMinimization}: 0 (best), 1 or 2 (fastest).
     */
    public void setUnsatCoreMinimization(int level) {
        if (level < 0 || level > 2) {
            throw new IllegalArgumentException(level + " is in invalid optimization level.");
        }
        options.coreMinimization = level;
    }

    /**
     * {@code saveState}: to generate counterexamples, the model is mutated;
     * its state is saved first and restored once completed.
     */
    public void saveState() {
        state = Util.saveState(sigs, new StateExtra(rep.minimizedBefore, rep.minimizedAfter, ans, resolved, command,
                givenTraces, exhausted));
    }

    /**
     * {@code restoreState}.
     */
    public void restoreState() {
        StateExtra extra = Util.restoreState(state, sigs);
        rep.minimizedBefore = extra.minimizedBefore();
        rep.minimizedAfter = extra.minimizedAfter();
        ans = extra.answer();
        resolved = extra.resolved();
        command = extra.command();
        givenTraces = extra.givenTraces();
        exhausted = extra.exhausted();
    }

    /**
     * {@code removeConstraint}: the constraint at the span, as the unsat core
     * gives it (the second column one more than Alloy's).
     *
     * @throws IllegalStateException if there is no constraint at the span
     */
    public void removeConstraint(int y, int x, int y2, int x2) {
        // The second column is one less than it should be. Not sure if Alloy bug or intended.
        Pos constraint = new Pos(modelFile, x, y, x2 - 1, y2);
        Command newCommand = Util.removeGlobalConstraint(constraint, command);
        if (newCommand == null) {
            if (!Util.removeLocalConstraint(constraint, sigs)) {
                throw new IllegalStateException(String.format(
                        "Cannot remove constraint (line=%d, column=%d)-(line=%d, column=%d)",
                        constraint.y, constraint.x, constraint.y2, constraint.x2));
            }
        } else {
            command = newCommand;
        }
    }

    /**
     * {@code unsatCore}: the spans of the constraints of the unsat core of the
     * model, each as {line, column, line, column + 1}, or none. Only the model
     * has an unsat core: not the next solution, when the solutions of the
     * model run out. Alloy reports no minimized core when it finds the model
     * unsatisfiable without solving, e.g., {@code [ some b ]} and
     * {@code [ no b ]}, so the core is not checked by its size.
     */
    public List<int[]> unsatCore() {
        List<int[]> core = new ArrayList<>();
        if (ans == resolved && !ans.satisfiable()) {
            Set<Pos> unsatCore = ans.highLevelCore().a;
            for (Pos p : unsatCore) {
                // The second column is one less than it should be.
                core.add(new int[]{p.y, p.x, p.y2, p.x2 + 1});
            }
        }
        return core;
    }

    // ----------------------------------------------------------------------
    // AlloyIG's helpers
    // ----------------------------------------------------------------------
    private record StateExtra(int minimizedBefore, int minimizedAfter, A4Solution answer, A4Solution resolved,
            Command command, Expr givenTraces, boolean exhausted) {
    }

    private static CommandScope setCommandScopeSize(int scopeSize, CommandScope cs) throws ErrorSyntax {
        return new CommandScope(cs.pos, cs.sigPos, cs.sig, cs.isExact, scopeSize, scopeSize, cs.increment);
    }

    private static List<CommandScope> setScopeSize(Sig sig, int scopeSize, List<CommandScope> scope)
            throws ErrorSyntax {
        List<CommandScope> newScope = new ArrayList<>();
        boolean found = false;
        for (CommandScope cs : scope) {
            if (sig.equals(cs.sig)) {
                found = true;
                newScope.add(setCommandScopeSize(scopeSize, cs));
            } else {
                newScope.add(cs);
            }
        }
        if (!found) {
            newScope.add(new CommandScope(sig, false, scopeSize));
        }
        return newScope;
    }

    private static String toXml(A4Solution ans) throws Err {
        StringWriter xml = new StringWriter();
        ans.writeXML(new PrintWriter(xml), Collections.<Func>emptyList(), null);
        return xml.toString();
    }

    // A dynamic model has mutable sigs or fields; its solutions are traces.
    private static boolean isDynamic(SafeList<Sig> sigs) {
        for (Sig sig : sigs) {
            if (sig.isVariable != null) {
                return true;
            }
            for (Sig.Field field : sig.getFields()) {
                if (field.isVariable != null) {
                    return true;
                }
            }
        }
        return false;
    }

    // The relations of the instance in a state of a trace: the containment relations of the
    // clafers (r_c0_name) and their references (c0_name_ref).
    private static List<Sig.Field> stateFields(Iterable<Sig> sigs) {
        List<Sig.Field> fields = new ArrayList<>();
        for (Sig sig : sigs) {
            for (Sig.Field field : sig.getFields()) {
                if (field.label.startsWith("r_") || field.label.endsWith("_ref")) {
                    fields.add(field);
                }
            }
        }
        return fields;
    }

    // The given relations as they are seen in a state: the references of the clafers in it. The
    // model constrains a reference only while its clafer is alive, so a clafer that is not in the
    // state has any value, and a trace that differs only in such values is the same trace. A
    // clafer is alive when it is the child of a clafer, which is alive itself (the parent of
    // each child is alive in the model).
    private static List<Expr> visible(List<Sig.Field> fields, List<Sig.Field> all) {
        Expr alive = null;
        for (Sig.Field field : all) {
            if (field.label.startsWith("r_")) {
                Expr children = Sig.UNIV.join(field);
                alive = alive == null ? children : alive.plus(children);
            }
        }
        List<Expr> visible = new ArrayList<>();
        for (Sig.Field field : fields) {
            visible.add(alive != null && field.label.endsWith("_ref") ? alive.domain(field) : field);
        }
        return visible;
    }

    // The expression applied n times.
    private static Expr repeat(Expr e, int n, boolean prime) {
        for (int i = 0; i < n; i++) {
            e = prime ? e.prime() : e.after();
        }
        return e;
    }

    // The expression of an atom: the sig of a one sig, a number, or a variable for the atom.
    private static Expr atom(String label, Sig.PrimSig sig, Map<String, Decl> vars) throws Err {
        if (sig.isOne != null) {
            return sig;
        }
        if (sig.builtin) {
            return ExprConstant.makeNUMBER(Integer.parseInt(label));
        }
        Decl decl = vars.get(label);
        if (decl == null) {
            decl = sig.oneOf("atom" + vars.size());
            vars.put(label, decl);
        }
        return decl.get();
    }

    // The relations of a configuration of a dynamic model, which do not change over time.
    private static List<Sig.Field> staticFields(List<Sig.Field> fields) {
        List<Sig.Field> statics = new ArrayList<>();
        for (Sig.Field field : fields) {
            if (field.isVariable == null) {
                statics.add(field);
            }
        }
        return statics;
    }

    // The relations of a configuration of a static model: its structure, which clafers there
    // are, without the values of its references. A top-level clafer has no parent relation,
    // so the sigs say which clafers there are.
    private static List<Expr> structure(Iterable<Sig> sigs) {
        List<Expr> structure = new ArrayList<>();
        for (Sig sig : sigs) {
            if (!sig.builtin && sig.isAbstract == null) {
                structure.add(sig);
            }
            for (Sig.Field field : sig.getFields()) {
                if (field.label.startsWith("r_")) {
                    structure.add(field);
                }
            }
        }
        return structure;
    }

    // Some distinct atoms make the given relations in the first states of the trace equal to those
    // of the given trace, up to a renaming of its atoms; with the loop, from the loop state, the
    // trace repeats the loop, so that it is the given trace.
    private static Expr sameStates(A4Solution ans, List<? extends Expr> fields, int count, boolean withLoop)
            throws Err {
        Map<String, Decl> vars = new LinkedHashMap<>();
        // an instance of a static model has no trace, but it is one state
        int length = Math.max(ans.getTraceLength(), 1);
        int loop = ans.getLoopState();
        Expr trace = ExprConstant.TRUE;
        for (int s = Math.min(count, length) - 1; s >= 0; s--) {
            Expr st = ExprConstant.TRUE;
            for (Expr field : fields) {
                Expr value = null;
                for (A4Tuple tuple : (A4TupleSet) ans.eval(field, s)) {
                    Expr t = null;
                    for (int i = 0; i < tuple.arity(); i++) {
                        Expr a = atom(tuple.atom(i), tuple.sig(i), vars);
                        t = t == null ? a : t.product(a);
                    }
                    value = value == null ? t : value.plus(t);
                }
                st = st.and(value == null ? field.no() : field.equal(value));
            }
            trace = s == Math.min(count, length) - 1 ? st : st.and(trace.after());
        }
        if (withLoop) {
            Expr periodic = ExprConstant.TRUE;
            for (Expr field : fields) {
                periodic = periodic.and(field.equal(repeat(field, length - loop, true)));
            }
            trace = trace.and(repeat(periodic.always(), loop, false));
        }
        List<Decl> decls = new ArrayList<>(vars.values());
        for (int i = 0; i < decls.size(); i++) {
            for (int j = i + 1; j < decls.size(); j++) {
                trace = decls.get(i).get().equal(decls.get(j).get()).not().and(trace);
            }
        }
        return decls.isEmpty() ? trace : ExprQt.Op.SOME.make(Pos.UNKNOWN, Pos.UNKNOWN, decls, trace);
    }

    // The command with the given bounds of the trace length.
    private static Command withTraceLength(Command c, int minTraceLength, int maxTraceLength) {
        return new Command(c.pos, c.nameExpr, c.label, c.check, c.overall, c.bitwidth, c.maxseq, minTraceLength,
                maxTraceLength, c.expects, c.scope, c.additionalExactScopes, c.commandKeyword, c.formula, c.parent);
    }

    private static String multiplicity(Sig sig) {
        if (sig.isOne != null) {
            return "One";
        } else if (sig.isLone != null) {
            return "Lone";
        } else if (sig.isSome != null) {
            return "Some";
        }
        return "Any";
    }

    // The options for a solve whose unsatisfiability is not shown as an unsat core: a check, and the next
    // trace, fork or configuration. The solver is not the prover, which logs the translation to a file for
    // an unsat core; that log of a large model can take gigabytes, more than a temporary folder holds.
    private static A4Options withoutCore(A4Options options) {
        A4Options without = options.dup();
        SATFactory minisat = SATFactory.get("minisat");
        without.solver = minisat.isPresent() ? minisat : SATFactory.get("sat4j");
        return without;
    }

    private static String removeCurly(String string) {
        int length = string.length();
        if (length > 0 && string.charAt(0) == '{' && string.charAt(length - 1) == '}') {
            return string.substring(1, length - 1);
        }
        throw new IllegalArgumentException("\"" + string + "\" is not surrounded by curly brackets.");
    }
}
