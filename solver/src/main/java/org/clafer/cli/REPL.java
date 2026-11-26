package org.clafer.cli;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.function.UnaryOperator;
import joptsimple.OptionSet;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstModel;
import static org.clafer.ast.Asts.global;
import static org.clafer.ast.Asts.sum;
import org.clafer.compiler.ClaferCompiler;
import org.clafer.compiler.ClaferOption;
import org.clafer.compiler.ClaferSearch;
import org.clafer.compiler.ClaferSearchStrategy;
import org.clafer.compiler.ClaferUnsat;
import org.clafer.ig.Backend;
import org.clafer.ig.ClaferModel;
import org.clafer.ig.Repl;
import org.clafer.ig.Solution;
import org.clafer.instance.InstanceClafer;
import org.clafer.instance.InstanceModel;
import org.clafer.javascript.JavascriptFile;
import org.clafer.objective.Objective;
import org.clafer.scope.Scope;

/**
 * clafersg's REPL for a static model: the Choco backend of the one solution
 * loop ({@link Repl}), with clafersg's commands and messages, and the
 * instances as chocosolver prints them.
 */
public class REPL {

    public static void runREPL(File inputFile, JavascriptFile javascriptFile, OptionSet options) throws Exception {
        ChocoBackend backend = new ChocoBackend(inputFile, javascriptFile, options);
        new Repl(backend, new Repl.Io(new BufferedReader(new InputStreamReader(System.in)), System.out, System.err),
                false).run();
    }

    public static void printHelp() {
        System.out.print(ChocoBackend.Help);
    }

    /**
     * The Choco backend.
     */
    static final class ChocoBackend implements Backend {

        static final String Help = """
                'h'elp                              Print the REPL commands.
                'n'ext                              Generate the next instance.
                <enter>                             Generate the next instance.
                'p'rettify                          Toggle prettify on/off.
                'r'eload                            Reload the model from its file: a .cfr file is compiled again.
                'u'nsatCore                         Compute the set of contradicting constraints if any.
                min'U'nsat                          Compute the minimal UnSAT core and a near-miss example.
                'S'etGlobalScope <value>            Set the global scope to the <value> .
                's'etScope <claferUID> <value>      Set the scope of the given clafer to the <value>.
                'I'ncGlobalScope <value?>           Increase the global scope by <value> or 1 by default.
                'i'ncScope <claferUID> <value?>     Increase the scope of the given clafer by the <value>  or 1 by default.
                sa'v'eScopes                        Save the currect scopes to a .cfr-scope file.
                'scope'                             Print the global scope and the scope of each clafer.
                'm'axInt <value>                    Set the largest allowed integer to <value>.
                ma`x`imize <claferUID>              Find a solution where <claferUID>.dref is maximal.
                minimi'z'e <claferUID>              Find a solution where <claferUID>.dref is minimal.
                sta't's                             Display statistics about the current search.
                'O'ptions                           Display the current solver options.
                strate'g'y <smaller|larger|random>  Set search strategy to prefer smaller, prefer larger, or random.
                'o'ptimizations                     Toggle optimizations basic/full.
                symmetry'B'reaking                  Toggle symmetry breaking basic/full.
                'd'iff                              Show each next instance as its changes from the previous one, or in full.
                sav'e'                              Save the instances shown so far to <model file name>.<number>.data.
                'dot'                               Save the most recent instance as a Graphviz graph.
                'f'ind <name>                       Print the clafer with the given name of the most recent instance.
                'q'uit                              Exit the REPL sesssion.
                """;

        private final File inputFile;
        private final OptionSet options;
        private final String scopesFile;
        private JavascriptFile javascriptFile;
        private Scope scope;
        private ClaferOption compilerOption;
        private AstModel model;
        private Objective[] objectives;
        private ClaferSearch solver;
        private boolean prettify;
        private final boolean addTypes;
        private int instanceID = 0; // id of an instance generated previously

        ChocoBackend(File inputFile, JavascriptFile javascriptFile, OptionSet options) throws IOException {
            this.inputFile = inputFile;
            this.javascriptFile = javascriptFile;
            this.options = options;
            // the model's scope file, e.g., model.cfr-scope for model.js, model.des.cfr-scope for model.des.cfr
            String path = inputFile.getAbsolutePath();
            this.scopesFile = path.substring(0, path.lastIndexOf('.')) + ".cfr-scope";
            this.scope = Utils.resolveScopes(inputFile, javascriptFile, options);
            ClaferOption option = javascriptFile.getOption();
            if (options.has("search")) {
                option = option.setStrategy((ClaferSearchStrategy) options.valueOf("search"));
            }
            this.compilerOption = option;
            this.model = javascriptFile.getModel();
            this.objectives = javascriptFile.getObjectives();
            this.prettify = options.has("prettify");
            this.addTypes = options.has("addtypes");
        }

        @Override
        public boolean isDynamic() {
            return false;
        }

        @Override
        public String prompt() {
            return "\nClaferChocoIG> ";
        }

        @Override
        public String help() {
            return Help;
        }

        @Override
        public void start(Repl.Io io) {
            solver = objectives.length == 0
                    ? ClaferCompiler.compile(model, scope, compilerOption)
                    : ClaferCompiler.compile(model, scope, objectives, compilerOption);
            io.out().println("Type 'help' for the list of available REPL commands");
        }

        @Override
        public Shown next(Repl.Io io) throws IOException {
            if (solver == null) {
                solver = compileModel(io);
            }
            return nextInstance(io);
        }

        private Shown nextInstance(Repl.Io io) throws IOException {
            if (solver == null) {
                io.out().println("Could not start the instantiation");
                return null;
            }
            if (!solver.find()) {
                io.out().println("No more instances found. Consider increasing scopes.");
                return null;
            }
            InstanceModel instance = solver.instance();
            StringBuilder text = new StringBuilder();
            if (prettify) {
                instance.print(text, addTypes);
            } else {
                for (InstanceClafer c : instance.getTopClafers()) {
                    Utils.printClafer(c, text);
                }
            }
            return new Shown(new Solution.Instance(tree(instance), text.toString()), null);
        }

        @Override
        public String show(int n, Shown shown) {
            return "\n=== Instance " + n + " Begin ===\n\n" + ((Solution.Instance) shown.solution()).text()
                    + "\n--- Instance " + n + " End ---\n\n";
        }

        @Override
        public int number(Repl.Context context) {
            return ++instanceID;
        }

        @Override
        public Shown fork(Repl.Io io, int state) {
            return null;
        }

        @Override
        public Shown nextConfiguration(Repl.Io io) {
            io.out().println("The command 'config' is not available for static models yet.");
            return null;
        }

        @Override
        public void check(Repl.Io io, Integer line, Repl.Displayer display) {
            io.out().println("The command 'check' is not available for static models yet: run clafersg with -v"
                    + " to check the assertions.");
        }

        @Override
        public UnaryOperator<String> names() {
            return uid -> uid;
        }

        @Override
        public String modelFileName() {
            return inputFile.getName();
        }

        @Override
        public void quit() {
        }

        @Override
        public void unknown(Repl.Io io, String input) {
            io.out().println("Unhandled command: " + input.trim());
        }

        private ClaferSearch compileModel(Repl.Io io) {
            ClaferSearch s;
            instanceID = 0; // reset instance ID
            try {
                s = objectives.length == 0
                        ? ClaferCompiler.compile(model, scope, compilerOption)
                        : ClaferCompiler.compile(model, scope, objectives, compilerOption);
            } catch (Exception e) {
                s = null;
                io.out().println("Message: " + e.getMessage());
                e.printStackTrace(io.err());
            }
            return s;
        }

        // The result of an unsat command is null when removing constraints cannot make the model satisfiable
        private static void printUnsat(Repl.Io io, Object unsat) {
            io.out().println(unsat == null
                    ? "No constraints to report: the model is unsatisfiable without them. Check the cardinalities or increase the scopes."
                    : unsat);
        }

        private void recompiled(Repl.Io io) {
            solver = compileModel(io);
            if (solver != null) {
                io.out().println("Model is ready after the scope change");
            }
        }

        private Integer integer(Repl.Io io, String value) {
            try {
                return Integer.parseInt(value);
            } catch (Exception e) {
                io.out().println("The scope has to be an integer number. Given '" + value + "'");
                return null;
            }
        }

        private AstClafer clafer(Repl.Io io, String claferName) {
            AstClafer clafer = Utils.getModelChildByName(model, claferName);
            if (clafer == null) {
                io.out().println("The clafer is not found: '" + claferName + "'");
            }
            return clafer;
        }

        @Override
        public boolean command(Repl.Io io, String s, List<String> words, Repl.Context context) throws Exception {
            s = s.trim();
            String[] commandParts = s.split(" ");
            String command = commandParts[0];
            switch (command) {
                case "n", "next" -> {
                    // clafersg's next ignores the arguments
                    Shown shown = next(io);
                    if (shown != null) {
                        context.display(shown);
                    }
                }
                case "p", "prettify" -> {
                    prettify = !prettify;
                    io.out().println("Prettify: " + (prettify ? "On" : "Off"));
                }
                case "t", "stats" -> io.out().println(solver.getInternalSolver().getMeasures().toString());
                case "O", "Options" -> {
                    String strategy = (compilerOption.getStrategy() == ClaferSearchStrategy.PreferSmallerInstances)
                            ? "smaller"
                            : ((compilerOption.getStrategy() == ClaferSearchStrategy.PreferLargerInstances)
                            ? "larger" : "random");
                    io.out().println("Options:"
                            + "\n strate'g'y:         " + strategy
                            + "\n symmetry'B'reaking: "
                            + (compilerOption.isBasicSymmetryBreaking() ? "basic" : "")
                            + (compilerOption.isFullSymmetryBreaking() ? "full" : "")
                            + "\n 'o'ptimizations:    "
                            + (compilerOption.isBasicOptimizations() ? "basic" : "")
                            + (compilerOption.isFullOptimizations() ? "full" : "")
                            + "\n 'm'axInt            " + (scope.getIntHigh())
                            + "\n global'S'cope       " + (scope.getDefaultScope()));
                }
                case "o", "optimizations" -> {
                    if (compilerOption.isBasicOptimizations()) {
                        compilerOption = compilerOption.fullOptimizations();
                        io.out().println("Optimizations: full");
                    } else {
                        compilerOption = compilerOption.basicOptimizations();
                        io.out().println("Optimizations: basic");
                    }
                }
                case "B", "symmetryBreaking" -> {
                    if (compilerOption.isBasicSymmetryBreaking()) {
                        compilerOption = compilerOption.fullSymmetryBreaking();
                        io.out().println("Symmetry breaking: full");
                    } else {
                        compilerOption = compilerOption.basicSymmetryBreaking();
                        io.out().println("Symmetry breaking: basic");
                    }
                }
                case "U", "minUnsat" -> {
                    io.out().println("Min UNSAT command:");
                    ClaferUnsat unsat = ClaferCompiler.compileUnsat(model, scope);
                    // Print the Min-Unsat and near-miss example.
                    printUnsat(io, unsat.minUnsat());
                }
                case "u", "unsatCore" -> {
                    io.out().println("UNSAT Core command:");
                    ClaferUnsat unsat = ClaferCompiler.compileUnsat(model, scope);
                    printUnsat(io, unsat.unsatCore());
                }
                case "r", "reload" -> reload(io);
                case "S", "SetGlobalScope" -> {
                    io.out().println("Set global scope: " + s);
                    if (commandParts.length != 2) {
                        io.out().println("The format of the command is: 'S <integer>'");
                        io.out().println("Given: '" + s + "'");
                        return true;
                    }
                    Integer scopeValue = integer(io, commandParts[1]);
                    if (scopeValue != null) {
                        scope = scope.toBuilder().defaultScope(scopeValue).toScope();
                        recompiled(io);
                    }
                }
                case "I", "IncGlobalScope" -> {
                    io.out().println("Increase global scope: " + s);
                    if (commandParts.length > 2) {
                        io.out().println("The format of the command is: 'I <integer?>'");
                        io.out().println("Given: '" + s + "'");
                        return true;
                    }
                    Integer scopeValue = commandParts.length == 2 ? integer(io, commandParts[1]) : Integer.valueOf(1);
                    if (scopeValue != null) {
                        scope = scope.toBuilder().adjustDefaultScope(scopeValue).toScope();
                        recompiled(io);
                    }
                }
                case "m", "maxInt", "maxint" -> {
                    io.out().println("Max Integer: " + s);
                    if (commandParts.length != 2) {
                        io.out().println("The format of the command is: 'm <integer>'");
                        io.out().println("Given: '" + s + "'");
                        return true;
                    }
                    int scopeHigh;
                    try {
                        scopeHigh = Integer.parseInt(commandParts[1]);
                    } catch (Exception e) {
                        io.out().println("Expected an integer number. Given '" + commandParts[1] + "'");
                        return true;
                    }
                    int scopeLow = -(scopeHigh + 1);
                    scope = scope.toBuilder().intLow(scopeLow).intHigh(scopeHigh).toScope();
                    recompiled(io);
                }
                case "x", "maximize", "z", "minimize" -> {
                    boolean maximize = command.equals("x") || command.equals("maximize");
                    io.out().println((maximize ? "Maximize: " : "Minimize: ") + s);
                    if (commandParts.length != 2) {
                        io.out().println("The format of the command is: '" + (maximize ? "x" : "z") + " <claferUID>'");
                        io.out().println("Given: '" + s + "'");
                        return true;
                    }
                    AstClafer clafer = clafer(io, commandParts[1]);
                    if (clafer == null) {
                        return true;
                    }
                    if (!clafer.hasRef()) {
                        io.out().println("Cannot " + (maximize ? "maximize " : "minimize ") + clafer
                                + " because it is not a reference.");
                        return true;
                    }
                    solver = ClaferCompiler.compile(model, scope, maximize
                            ? Objective.maximize(sum(global(clafer))) : Objective.minimize(sum(global(clafer))));
                    // shown by the loop as the next instance
                    Shown shown = nextInstance(io);
                    if (shown != null) {
                        context.display(shown);
                    }
                }
                case "v", "saveScopes" -> {
                    List<ClaferNameScopePair> claferScopePairs = new ArrayList<>();
                    for (AstClafer curClafer : Utils.getAllModelClafers(model)) {
                        int curScope;
                        try {
                            curScope = scope.getScope(curClafer);
                        } catch (Exception e) {
                            curScope = 0;
                        }
                        claferScopePairs.add(new ClaferNameScopePair(curClafer.getName(), curScope));
                    }
                    Collections.sort(claferScopePairs);
                    Utils.produceScopeFile(claferScopePairs, scopesFile);
                }
                case "scope" -> {
                    io.out().println("Global scope = " + scope.getDefaultScope());
                    for (AstClafer c : Utils.getAllModelClafers(model)) {
                        int curScope;
                        try {
                            curScope = scope.getScope(c);
                        } catch (Exception e) {
                            curScope = 0;
                        }
                        io.out().println("  " + c.getName() + " scope = " + curScope);
                    }
                }
                case "s", "setScope" -> {
                    io.out().println("Set individual scope: " + s);
                    if (commandParts.length != 3) {
                        io.out().println("The format of the command is: 's <claferUID> <integer>'");
                        io.out().println("Given: '" + s + "'");
                        return true;
                    }
                    Integer claferScopeValue = integer(io, commandParts[2]);
                    if (claferScopeValue == null) {
                        return true;
                    }
                    AstClafer clafer = clafer(io, commandParts[1]);
                    if (clafer != null) {
                        scope = scope.toBuilder().setScope(clafer, claferScopeValue).toScope();
                        recompiled(io);
                    }
                }
                case "i", "incScope" -> {
                    io.out().println("Increase individual scope: " + s);
                    if (commandParts.length < 2 || commandParts.length > 3) {
                        io.out().println("The format of the command is: 'i <claferUID> <integer?>'");
                        io.out().println("Given: '" + s + "'");
                        return true;
                    }
                    Integer claferScopeValue = commandParts.length == 3 ? integer(io, commandParts[2])
                            : Integer.valueOf(1);
                    if (claferScopeValue == null) {
                        return true;
                    }
                    AstClafer clafer = clafer(io, commandParts[1]);
                    if (clafer != null) {
                        scope = scope.toBuilder().adjustScope(clafer, claferScopeValue).toScope();
                        recompiled(io);
                    }
                }
                case "g", "strategy" -> {
                    if (commandParts.length != 2 || (!"smaller".equals(commandParts[1])
                            && !"larger".equals(commandParts[1]) && !"random".equals(commandParts[1]))) {
                        io.out().println("The format of the command is: 'g <smaller|larger|random>'");
                        io.out().println("Given: '" + s + "'");
                        return true;
                    }
                    switch (commandParts[1]) {
                        case "smaller" -> {
                            compilerOption = compilerOption.setStrategy(ClaferSearchStrategy.PreferSmallerInstances);
                            io.out().println("Search strategy: smaller");
                        }
                        case "larger" -> {
                            compilerOption = compilerOption.setStrategy(ClaferSearchStrategy.PreferLargerInstances);
                            io.out().println("Search strategy: larger");
                        }
                        default -> {
                            io.out().println("Search strategy: random");
                            compilerOption = compilerOption.setStrategy(ClaferSearchStrategy.Random);
                        }
                    }
                }
                case "b", "bitwidth", "tl", "a", "alloyModel", "c", "claferModel", "loadScopes", "setUnsatCoreMinimization" ->
                    io.out().println("The command '" + command + "' is for temporal models, which Alloy solves.");
                default -> {
                    return false;
                }
            }
            return true;
        }

        private void reload(Repl.Io io) throws IOException {
            try {
                javascriptFile = Utils.readModel(inputFile, options);
            } catch (Frontend.ModelException | IllegalArgumentException e) {
                // the errors of the model, with their positions
                io.out().println(e.getMessage());
                javascriptFile = null;
            } catch (Exception e) {
                io.out().println("Unhandled compilation error occured. Please report this problem.");
                io.out().println(e.getMessage());
                javascriptFile = null;
            }
            if (javascriptFile != null) {
                model = javascriptFile.getModel();
                scope = Utils.resolveScopes(inputFile, javascriptFile, options);
                objectives = javascriptFile.getObjectives();
                solver = compileModel(io);
                io.out().println(solver == null ? "Could not reload" : "Reloaded");
            } else {
                io.out().println("Could not reload");
            }
        }

        /**
         * The instance as the loop's tree: each clafer named as chocosolver
         * prints it.
         */
        private static ClaferModel tree(InstanceModel instance) {
            List<ClaferModel.Clafer> top = new ArrayList<>();
            for (InstanceClafer c : instance.getTopClafers()) {
                top.add(tree(c));
            }
            return new ClaferModel(top);
        }

        private static String name(InstanceClafer c) {
            return c.getType().getName() + Utils.countSuffix(c.getId());
        }

        private static ClaferModel.Clafer tree(InstanceClafer c) {
            ClaferModel.Value value = null;
            if (c.hasRef()) {
                Object ref = c.getRef();
                value = switch (ref) {
                    case InstanceClafer r -> new ClaferModel.AliasValue(new ClaferModel.Id(name(r), 0));
                    case Integer i -> new ClaferModel.IntValue(i);
                    case BigDecimal d -> new ClaferModel.StringValue(d.toPlainString());
                    default -> new ClaferModel.StringValue(ref.toString());
                };
            }
            List<ClaferModel.Clafer> children = new ArrayList<>();
            for (InstanceClafer child : c.getChildren()) {
                children.add(tree(child));
            }
            return new ClaferModel.Clafer(new ClaferModel.Id(name(c), 0), c.getType().getName(), value, children);
        }
    }
}
