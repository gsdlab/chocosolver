package org.clafer.cli;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import joptsimple.OptionSet;

import static org.clafer.ast.Asts.*;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstModel;
import org.clafer.ast.AstGlobal;
import org.clafer.compiler.ClaferCompiler;
import org.clafer.compiler.ClaferOption;
import org.clafer.compiler.ClaferSearch;
import org.clafer.compiler.ClaferSearchStrategy;
import org.clafer.compiler.ClaferUnsat;
import org.clafer.instance.InstanceClafer;
import org.clafer.instance.InstanceModel;
import org.clafer.javascript.Javascript;
import org.clafer.javascript.JavascriptFile;
import org.clafer.objective.*;
import org.clafer.scope.Scope;

public class REPL {
    private static int instanceID = 0; // id of an instance generated previously

    private static String prompt(BufferedReader br) throws IOException {
        System.out.print("\nClaferChocoIG> ");
        return br.readLine().trim();
    }
    public static void printHelp() {
      System.out.println("'h'elp                              Print the REPL commands.");
      System.out.println("'n'ext                              Generate the next instance.");
      System.out.println("<enter>                             Generate the next instance.");
      System.out.println("'p'rettify                          Toggle prettify on/off.");
      System.out.println("'r'eload                            Reload the model from the same <file-name.js> file.");
      System.out.println("'u'nsatCore                         Compute the set of contradicting constraints if any.");
      System.out.println("min'U'nsat                          Compute the minimal UnSAT core and a near-miss example.");
      System.out.println("'S'etGlobalScope <value>            Set the global scope to the <value> .");
      System.out.println("'s'etScope <claferUID> <value>      Set the scope of the given clafer to the <value>.");
      System.out.println("'I'ncGlobalScope <value?>           Increase the global scope by <value> or 1 by default.");
      System.out.println("'i'ncScope <claferUID> <value?>     Increase the scope of the given clafer by the <value>  or 1 by default.");
      System.out.println("sa'v'eScopes                        Save the currect scopes to a .cfr-scope file.");
      System.out.println("'m'axInt <value>                    Set the largest allowed integer to <value>.");
      System.out.println("ma`x`imize <claferUID>              Find a solution where <claferUID>.dref is maximal.");
      System.out.println("minimiz'e' <claferUID>              Find a solution where <claferUID>.dref is minimal.");
      System.out.println("sta't's                             Display statistics about the current search.");
      System.out.println("'O'ptions                           Display the current solver options.");
      System.out.println("strate'g'y <smaller|larger|random>  Set search strategy to prefer smaller, prefer larger, or random.");
      System.out.println("'o'ptimizations                     Toggle optimizations basic/full.");
      System.out.println("symmetry'B'reaking                  Toggle symmetry breaking basic/full.");
      System.out.println("'q'uit                              Exit the REPL sesssion.");
    }
    public static String commandHelpL             = "help";
    public static String commandHelpS             = "h";
    public static String commandNextL             = "next";
    public static String commandNextS             = "n";
    public static String commandNext              = "";
    public static String commandPrettifyL         = "prettify";
    public static String commandPrettifyS         = "p";
    public static String commandReloadL           = "reload";
    public static String commandReloadS           = "r";
    public static String commandUnsatCoreL        = "unsatCore";
    public static String commandUnsatCoreS        = "u";
    public static String commandMinUnsatL         = "minUnsat";
    public static String commandMinUnsatS         = "U";
    public static String commandSetGlobalScopeL   = "SetGlobalScope";
    public static String commandSetGlobalScopeS   = "S";
    public static String commandSetScopeL         = "setScope";
    public static String commandSetScopeS         = "s";
    public static String commandIncGlobalScopeL   = "IncGlobalScope";
    public static String commandIncGlobalScopeS   = "I";
    public static String commandIncScopeL         = "incScope";
    public static String commandIncScopeS         = "i";
    public static String commandSaveScopesL       = "saveScopes";
    public static String commandSaveScopesS       = "v";
    public static String commandMaxIntL           = "maxInt";
    public static String commandMaxIntS           = "m";
    public static String commandMaximizeL         = "maximize";
    public static String commandMaximizeS         = "x";
    public static String commandMinimizeL         = "maximize";
    public static String commandMinimizeS         = "z";
    public static String commandStatsL            = "stats";
    public static String commandStatsS            = "t";
    public static String commandOptionsL          = "Options";
    public static String commandOptionsS          = "O";
    public static String commandOptimizationsL    = "optimizations";
    public static String commandOptimizationsS    = "o";
    public static String commandSymmetryBreakingL = "symmetryBreaking";
    public static String commandSymmetryBreakingS = "B";
    public static String commandStrategyL         = "strategy";
    public static String commandStrategyS         = "g";
    public static String commandQuitL             = "quit";
    public static String commandQuitS             = "q";

    public static void runREPL(File inputFile, JavascriptFile javascriptFile, OptionSet options) throws Exception {
        String scopesFile = inputFile.getAbsolutePath().substring(0, inputFile.getAbsolutePath().length() - 3) + ".cfr-scope";

        Scope scope = Utils.resolveScopes(javascriptFile, options);

        ClaferOption compilerOption = ClaferOption.Default;
        if (options.has("search"))
            compilerOption = compilerOption.setStrategy((ClaferSearchStrategy) options.valueOf("search"));

        AstModel model = javascriptFile.getModel();
        Objective[] objectives = javascriptFile.getObjectives();
        ClaferSearch solver = null;
        if (objectives.length == 0)
              solver = ClaferCompiler.compile(model, scope, compilerOption);
            else
              solver = ClaferCompiler.compile(model, scope, objectives, compilerOption);

        boolean prettify = options.has("prettify");

        System.out.println("Type 'help' for the list of available REPL commands");
        if (solver != null)
            nextInstance(solver, prettify);

        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        String s = "";

        /* ****************************************
        |            Main REPL loop               |
        **************************************** */
        while(true) {
            s = prompt(br);

            if (s.equals(commandQuitS) || s.equals(commandQuitL))
               break;

            if (s.equals(commandPrettifyS) || s.equals(commandPrettifyL)) {
                prettify = ! prettify;
                System.out.println("Prettify: " + (prettify ? "On" : "Off"));
                continue;
            }

            if (s.equals(commandStatsL)) {
                System.out.println(solver.getInternalSolver().getMeasures().toString());
                continue;
            }

            if (s.equals(commandOptionsS) || s.equals(commandOptionsL)) {
                String strategy = (compilerOption.getStrategy() == ClaferSearchStrategy.PreferSmallerInstances)
                                  ? "smaller"
                                  : ((compilerOption.getStrategy() == ClaferSearchStrategy.PreferLargerInstances)
                                      ? "larger" : "random"
                                    );

                System.out.println
                ( "Options:"
                + "\n strate'g'y:         "
                + strategy
                + "\n symmetry'B'reaking: "
                + (compilerOption.isBasicSymmetryBreaking() ? "basic" : "")
                + (compilerOption.isFullSymmetryBreaking() ? "full" : "")
                + "\n 'o'ptimizations:    "
                + (compilerOption.isBasicOptimizations() ? "basic" : "")
                + (compilerOption.isFullOptimizations() ? "full" : "")
                + "\n"
                );
                continue;
            }

            if (s.equals(commandOptimizationsS) || s.equals(commandOptimizationsL)) {
                if (compilerOption.isBasicOptimizations()) {
                  compilerOption = compilerOption.fullOptimizations();
                  System.out.println("Optimizations: full");
                }
                else  {
                  compilerOption = compilerOption.basicOptimizations();
                  System.out.println("Optimizations: basic");
                }
                continue;
            }

            if (s.equals(commandSymmetryBreakingS) || s.equals(commandSymmetryBreakingL)) {
                if (compilerOption.isBasicSymmetryBreaking()) {
                  compilerOption = compilerOption.fullSymmetryBreaking();
                  System.out.println("Symmetry breaking: full");
                }
                else  {
                  compilerOption = compilerOption.basicSymmetryBreaking();
                  System.out.println("Symmetry breaking: basic");
                }
                continue;
            }


            String commandParts[] = s.split(" ");
            if (s.equals(commandNext) || commandParts.length == 0) { // next instance
                if (solver == null)
                    solver = compileModel(model, scope, objectives, compilerOption);
                nextInstance(solver, prettify);
                continue;
            }

            String command = commandParts[0];

            if (command.equals(commandNextS) || command.equals(commandNextL)) // next instance
            {
                if (solver == null)
                    solver = compileModel(model, scope, objectives, compilerOption);

                nextInstance(solver, prettify);
            }
            else if (command.equals(commandMinUnsatS) || command.equals(commandMinUnsatL)) { // unsat
                System.out.println("Min UNSAT command:");
                ClaferUnsat unsat = ClaferCompiler.compileUnsat(model, scope);
                // Print the Min-Unsat and near-miss example.
                System.out.println(unsat.minUnsat());
            }
            else if (command.equals(commandUnsatCoreS) || command.equals(commandUnsatCoreL)) {// min unsat
                System.out.println("UNSAT Core command:");
                ClaferUnsat unsat = ClaferCompiler.compileUnsat(model, scope);
                // Print the Min-Unsat and near-miss example.
                System.out.println(unsat.unsatCore());
            }
            else if (command.equals(commandReloadS) || command.equals(commandReloadL)) {// reloading
                try {
                    javascriptFile = Javascript.readModel(inputFile);
                }
                catch(Exception e) {
                    System.out.println("Unhandled compilation error occured. Please report this problem.");
                    System.out.println(e.getMessage());
                    javascriptFile = null;
                }

                if (javascriptFile != null) {
                    model = javascriptFile.getModel();
                    scope = Utils.resolveScopes(javascriptFile, options);
                    objectives = javascriptFile.getObjectives();

                    solver = compileModel(model, scope, objectives, compilerOption);
                    if (solver == null)
                        System.out.println("Could not reload");
                    else
                        System.out.println("Reloaded");
                }
                else
                    System.out.println("Could not reload");

            }
            else if (command.equals(commandSetGlobalScopeS) || command.equals(commandSetGlobalScopeL)) {
                System.out.println("Set global scope: " + s);

                if (commandParts.length != 2) {
                    System.out.println("The format of the command is: '" + commandSetGlobalScopeS + " <integer>'");
                    System.out.println("Given: '" + s + "'");
                    continue;
                }

                int scopeValue;

                try {
                    scopeValue = Integer.parseInt(commandParts[1]);
                }
                catch(Exception e) {
                    System.out.println("The scope has to be an integer number. Given '" + commandParts[1] + "'");
                    continue;
                }

                scope = scope.toBuilder().defaultScope(scopeValue).toScope();
                solver = compileModel(model, scope, objectives, compilerOption);

                if (solver != null)
                    System.out.println("Model is ready after the scope change");
            }
            else if (command.equals(commandIncGlobalScopeS) || command.equals(commandIncGlobalScopeL)) {
                System.out.println("Increase global scope: " + s);

                if (commandParts.length > 2) {
                    System.out.println("The format of the command is: '" + commandIncGlobalScopeS + " <integer?>'");
                    System.out.println("Given: '" + s + "'");
                    continue;
                }

                int scopeValue = 1; // default increase by 1

                if (commandParts.length == 2) {
                  try {
                      scopeValue = Integer.parseInt(commandParts[1]);
                  }
                  catch(Exception e) {
                      System.out.println("The scope has to be an integer number. Given '" + commandParts[1] + "'");
                      continue;
                  }
                }

                scope = scope.toBuilder().adjustDefaultScope(scopeValue).toScope();
                solver = compileModel(model, scope, objectives, compilerOption);

                if (solver != null)
                    System.out.println("Model is ready after the scope change");
            }
            else if (command.equals(commandMaxIntS) || command.equals(commandMaxIntL)) {
                System.out.println("Max Integer: " + s);

                if (commandParts.length != 2) {
                    System.out.println("The format of the command is: '" + commandMaxIntS + " <integer>'");
                    System.out.println("Given: '" + s + "'");
                    continue;
                }

                int scopeHigh;

                try {
                    scopeHigh = Integer.parseInt(commandParts[1]);
                }
                catch(Exception e) {
                    System.out.println("Expected integer numbers. Given '" + commandParts[1] + "' '" + commandParts[2] + "'");
                    continue;
                }

                int scopeLow = -(scopeHigh + 1);
                scope = scope.toBuilder().intLow(scopeLow).intHigh(scopeHigh).toScope();
                solver = compileModel(model, scope, objectives, compilerOption);

                if (solver != null)
                    System.out.println("Model is ready after the scope change");

            }
            else if (command.equals(commandMaximizeS) || command.equals(commandMaximizeL)) {
                System.out.println("Maximize: " + s);

                if (commandParts.length != 2) {
                    System.out.println("The format of the command is: '" + commandMaximizeS + " <claferUID>'");
                    System.out.println("Given: '" + s + "'");
                    continue;
                }

                String claferName = commandParts[1];
                AstClafer clafer = Utils.getModelChildByName(model, claferName);
                if (clafer == null) {
                    System.out.println("The clafer is not found: '" + claferName + "'");
                    continue;
                }

                if (!clafer.hasRef()) {
                    System.out.println("Cannot maximize " + clafer + " because it is not a reference.");
                    continue;
                }
                solver = ClaferCompiler.compile(model, scope, Objective.maximize(sum(global(clafer))));

                nextInstance(solver, prettify);
            }
            else if (command.equals(commandMinimizeS) || command.equals(commandMinimizeL)) {
                System.out.println("Minimize: " + s);

                if (commandParts.length != 2) {
                    System.out.println("The format of the command is: '" + commandMinimizeS + " <claferUID>'");
                    System.out.println("Given: '" + s + "'");
                    continue;
                }

                String claferName = commandParts[1];
                AstClafer clafer = Utils.getModelChildByName(model, claferName);
                if (clafer == null) {
                    System.out.println("The clafer is not found: '" + claferName + "'");
                    continue;
                }

                if (!clafer.hasRef()) {
                    System.out.println("Cannot maximize " + clafer + " because it is not a reference.");
                    continue;
                }
                solver = ClaferCompiler.compile(model, scope, Objective.minimize(sum(global(clafer))));

                nextInstance(solver, prettify);
            }
            else if (command.equals(commandSaveScopesS) || command.equals(commandSaveScopesL)) { // getting list of scopes
                List<ClaferNameScopePair> claferScopePairs = new ArrayList<ClaferNameScopePair>();

                List<AstClafer> allClafers = Utils.getAllModelClafers(model);

                for (AstClafer curClafer: allClafers) {
                    int curScope;

                    try {
                        curScope = scope.getScope(curClafer);
                    }
                    catch(Exception e) {
                        curScope = 0;
                    }

                    claferScopePairs.add(new ClaferNameScopePair(curClafer.getName(), curScope));
                }

                Collections.sort(claferScopePairs);

                Utils.produceScopeFile(claferScopePairs, scopesFile);
            }
            else if (command.equals(commandSetScopeS) || command.equals(commandSetScopeL)) {
                System.out.println("Set individual scope: " + s);

                if (commandParts.length != 3) {
                    System.out.println("The format of the command is: '" + commandSetScopeS + " <claferUID> <integer>'");
                    System.out.println("Given: '" + s + "'");
                    continue;
                }

                int claferScopeValue;

                try {
                    claferScopeValue = Integer.parseInt(commandParts[2]);
                }
                catch(Exception e) {
                    System.out.println("The scope has to be an integer number. Given '" + commandParts[2] + "'");
                    continue;
                }

                String claferName = commandParts[1];
                AstClafer clafer = Utils.getModelChildByName(model, claferName);
                if (clafer == null) {
                    System.out.println("The clafer is not found: '" + claferName + "'");
                    continue;
                }

                scope = scope.toBuilder().setScope(clafer, claferScopeValue).toScope();
                solver = compileModel(model, scope, objectives, compilerOption);

                if (solver != null)
                    System.out.println("Model is ready after the scope change");

            }
            else if (command.equals(commandIncScopeS) || command.equals(commandIncScopeL)) {
                System.out.println("Increase individual scope: " + s);

                if (commandParts.length < 2 || commandParts.length > 3) {
                    System.out.println("The format of the command is: '" + commandIncScopeS + " <claferUID> <integer?>'");
                    System.out.println("Given: '" + s + "'");
                    continue;
                }

                int claferScopeValue = 1;  // Default increase by 1
                if (commandParts.length == 3) {
                  try {
                      claferScopeValue = Integer.parseInt(commandParts[2]);
                  }
                  catch(Exception e) {
                      System.out.println("The scope has to be an integer number. Given '" + commandParts[2] + "'");
                      continue;
                  }
                }

                String claferName = commandParts[1];
                AstClafer clafer = Utils.getModelChildByName(model, claferName);
                if (clafer == null) {
                    System.out.println("The clafer is not found: '" + claferName + "'");
                    continue;
                }

                scope = scope.toBuilder().adjustScope(clafer, claferScopeValue).toScope();
                solver = compileModel(model, scope, objectives, compilerOption);

                if (solver != null)
                    System.out.println("Model is ready after the scope change");
            }
            else if (command.equals(commandStrategyS) || command.equals(commandStrategyL)) {
                if (commandParts.length != 2 ||
                    (!"smaller".equals(commandParts[1]) &&
                     !"larger".equals(commandParts[1]) &&
                     !"random".equals(commandParts[1]))) {
                    System.out.println("The format of the command is: '" + commandStrategyS + " <smaller|larger|random>'");
                    System.out.println("Given: '" + s + "'");
                    continue;
                }
                switch (commandParts[1]) {
                  case "smaller":
                    compilerOption = compilerOption.setStrategy(ClaferSearchStrategy.PreferSmallerInstances);
                    System.out.println("Search strategy: smaller");
                    break;
                  case "larger":
                    compilerOption = compilerOption.setStrategy(ClaferSearchStrategy.PreferLargerInstances);
                    System.out.println("Search strategy: larger");
                    break;
                  case "random":
                    System.out.println("Search strategy: random");
                    compilerOption = compilerOption.setStrategy(ClaferSearchStrategy.Random);
                    break;
                }
                continue;
            }


            else if (command.equals(commandHelpS) || command.equals(commandHelpL)) {
                printHelp();
            }
            else
                System.out.println("Unhandled command: " + s);
        }
    }

    private static void nextInstance(ClaferSearch solver, boolean prettify) throws IOException {
        if (solver == null) {
            System.out.println("Could not start the instantiation");
            return;
        }

        if (solver.find()) {
            instanceID++;
            System.out.println("\n=== Instance " + instanceID + " Begin ===\n");

            InstanceModel instance = solver.instance();

            if (prettify)
                instance.print(System.out);
            else
                for (InstanceClafer c : instance.getTopClafers())
                    Utils.printClafer(c, System.out);
            System.out.println("\n--- Instance " + (instanceID) + " End ---\n");
        }
        else
            System.out.println("No more instances found. Consider increasing scopes.");
    }

    private static ClaferSearch compileModel(AstModel model, Scope scope, Objective[] objectives, ClaferOption compilerOption) {
        ClaferSearch solver;
        instanceID = 0; // reset instance ID
        try {
            if (objectives.length == 0)
              solver = ClaferCompiler.compile(model, scope, compilerOption);
            else
              solver = ClaferCompiler.compile(model, scope, objectives, compilerOption);
        }
        catch (Exception e) {
            solver = null;
            System.out.println("Message: " + e.getMessage());
            e.printStackTrace();
        }
        return solver;
    }
}
