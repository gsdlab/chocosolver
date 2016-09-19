package org.clafer.cli;

import java.io.File;
import java.io.PrintStream;
import joptsimple.OptionSet;
import org.clafer.assertion.Assertion;
import org.clafer.compiler.ClaferAsserter;
import org.clafer.compiler.ClaferCompiler;
import org.clafer.compiler.ClaferOption;
import org.clafer.compiler.ClaferSearchStrategy;
import org.clafer.instance.InstanceClafer;
import org.clafer.instance.InstanceModel;
import org.clafer.javascript.JavascriptFile;
import org.clafer.scope.Scope;


public class Validate {
    public static void runValidate(File inputFile, JavascriptFile javascriptFile, OptionSet options, PrintStream outStream) throws Exception {
        //----------------------------------------
        // Running the model itself(instantiating)
        //----------------------------------------
        Assertion[] assertions = javascriptFile.getAssertions();
            // no assertions means the model is valid
        if (assertions.length == 0)
            return;

        // handle scopes
        Scope scope = Utils.resolveScopes(javascriptFile, options);

        // handle search strategy
        ClaferOption compilerOption = ClaferOption.Default;
        if (options.has("search")) {
            compilerOption = compilerOption.setStrategy((ClaferSearchStrategy) options.valueOf("search"));
        }

        ClaferAsserter solver = ClaferCompiler.compile(javascriptFile.getModel(), scope, assertions, javascriptFile.getOption());

        int index = 0; // optimal instance id
        boolean prettify = options.has("prettify");

        while (solver.find()) {
            System.out.println("Failed assertion(s):");
            for (Assertion failedAssertion : solver.failedAssertions())
              System.out.println("  " + failedAssertion);
            outStream.println("\n=== Counterexample " + (++index) + " Begin ===\n");
            InstanceModel instance = solver.instance();

            if (prettify)
                instance.print(outStream);
            else {
                for (InstanceClafer c : instance.getTopClafers())
                    Utils.printClafer(c, outStream);
            }
            outStream.println("\n--- Counterexample " + (index) + " End ---\n");
        }

        if (index == 0)
          System.out.println("No counterexamples found within the scope. All assertions may hold.");
        else
          System.out.println("Generated all " + index + " counterexample(s) within the scope\n");
    }
}
