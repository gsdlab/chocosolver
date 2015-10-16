package org.clafer.cli;

import java.io.File;
import java.io.IOException;
import java.io.PrintStream;

import joptsimple.OptionSet;

import org.clafer.ast.AstClafer;
import org.clafer.ast.AstModel;
import org.clafer.compiler.ClaferCompiler;
import org.clafer.compiler.ClaferOption;
import org.clafer.compiler.ClaferSearch;
import org.clafer.compiler.ClaferOptimizer;
import org.clafer.compiler.ClaferSearchStrategy;
import org.clafer.instance.InstanceClafer;
import org.clafer.instance.InstanceModel;
import org.clafer.javascript.Javascript;
import org.clafer.javascript.JavascriptFile;
import org.clafer.objective.Objective;
import org.clafer.scope.Scope;


public class Normal {
    // Running the model itself(instantiating or optimizing)
    public static void runNormal(JavascriptFile  javascriptFile, OptionSet options, PrintStream outStream) throws Exception {

        Objective[] objectives = javascriptFile.getObjectives();
        if (objectives.length == 0)
            System.out.println("Instantiating...");
        else
            System.out.println("Optimizing...");

        // handle scopes
        Scope scope = Utils.resolveScopes(javascriptFile, options);

        // handle search strategy
        ClaferOption compilerOption = ClaferOption.Default;
        if (options.has("search"))
            compilerOption = compilerOption.setStrategy((ClaferSearchStrategy) options.valueOf("search"));

        // pick the right solver
        ClaferSearch solver = objectives.length == 0
            ? ClaferCompiler.compile(javascriptFile.getModel(), scope,             compilerOption)
            : ClaferCompiler.compile(javascriptFile.getModel(), scope, objectives, compilerOption);

        int index = 0; // instance id
        boolean prettify = options.has("prettify");

        int n = 0;
        if (options.has("n"))
            n = (int)options.valueOf("n");
        else
            n = -1;

        while (solver.find()) {
            if (n >= 0 && index == n)
                break;

            outStream.println("=== Instance " + (++index) + " Begin ===\n");
            InstanceModel instance = solver.instance();
            if (prettify)
                instance.print(outStream);
            else
                for (InstanceClafer c : instance.getTopClafers())
                      Utils.printClafer(c, outStream);
            outStream.println("\n--- Instance " + (index) + " End ---\n");
        }
        if (objectives.length == 0)
          System.out.println("Generated " +                           index + " instance(s) within the scope\n");
        else
          System.out.println("Generated " + (n == -1 ? "all " : "") + index + " optimal instance(s) within the scope\n");
    }
}
