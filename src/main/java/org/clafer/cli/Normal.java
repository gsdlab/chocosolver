package org.clafer.cli;

import java.io.File;
import java.io.PrintStream;
import joptsimple.OptionSet;
import org.clafer.compiler.ClaferCompiler;
import org.clafer.compiler.ClaferOption;
import org.clafer.compiler.ClaferSearch;
import org.clafer.compiler.ClaferSearchStrategy;
import org.clafer.instance.InstanceClafer;
import org.clafer.instance.InstanceModel;
import org.clafer.javascript.JavascriptFile;
import org.clafer.objective.Objective;
import org.clafer.scope.Scope;
import org.clafer.ast.AstModel;
import org.plantuml.ast.PlantumlProgram;
import org.plantuml.compiler.AstPlantumlCompiler;
import org.plantuml.pprinter.PlantumlPrinter;
import org.sysml.ast.SysmlProperty;
import org.sysml.ast.SysmlPropertyDef;
import org.sysml.compiler.AstSysmlCompiler;
import org.sysml.pprinter.SysmlPrinter;


public class Normal {
    // Running the model itself(instantiating or optimizing)
    public static void runNormal(JavascriptFile  javascriptFile, OptionSet options, PrintStream outStream) throws Exception {
        //do this first to cut irrelevant optimizing message
        boolean plantuml = options.has("plantuml");

        Objective[] objectives = javascriptFile.getObjectives();
        if (!plantuml){
            if (objectives.length == 0)
                System.out.println("Instantiating...");
            else
                System.out.println("Optimizing...");
        }

        // handle scopes
        Scope scope = Utils.resolveScopes(javascriptFile, options);

        // handle search strategy
        ClaferOption compilerOption = javascriptFile.getOption();
        if (options.has("search"))
            compilerOption = compilerOption.setStrategy((ClaferSearchStrategy) options.valueOf("search"));

        // pick the right solver
        ClaferSearch solver = objectives.length == 0
            ? ClaferCompiler.compile(javascriptFile.getModel(), scope,             compilerOption)
            : ClaferCompiler.compile(javascriptFile.getModel(), scope, objectives, compilerOption);

        int index = 0; // instance id
        boolean prettify = options.has("prettify");
        boolean sysml = options.has("sysml");
        boolean printOff = options.has("noprint");
        boolean dataTackingOn = options.has("dataFile");
        boolean timeOn = options.has("time");

        // check for conflicting options
        if (plantuml && sysml) {
            System.err.println("Bad CLI config: both plantuml and sysml are selected");
            return;
        }

        File dataFile;
        PrintStream dataStream = null;
        if (dataTackingOn) {
            dataFile = (File) options.valueOf("dataFile");
            dataStream = new PrintStream(dataFile);
        }

        if (plantuml) {
            File plantumlConfigFile = (File) options.valueOf("plantuml-config");
            AstModel top = javascriptFile.getModel();
            AstPlantumlCompiler compiler = AstPlantumlCompiler
                    .AstPlantumlCompilerBuilder
                    .buildFromToml(plantumlConfigFile);
            PlantumlProgram prog = compiler.compile(top);
            PlantumlPrinter pprinter = new PlantumlPrinter(outStream, plantumlConfigFile);
            pprinter.visit(prog, "");
            if (dataStream != null){
                dataStream.close();
            }
            return;
        }

        double elapsedTime;

        long startTime = System.nanoTime();

        int n = 0;
        if (options.has("n"))
            n = (int)options.valueOf("n");
        else
            n = -1;

        while (solver.find()) {
            if (dataTackingOn) {
                elapsedTime = (double) (System.nanoTime() - startTime) / 1000000000;
                dataStream.println(elapsedTime + ", " + (index + 1));
            }

            if (n >= 0 && index == n)
                break;

            if (printOff) {
                ++index;
            } else {
                if (sysml) {
                    outStream.append("package Instance" + (++index) + "{\n");
                    outStream.append("    import ScalarValues::*;\n");
                    AstModel top = javascriptFile.getModel();
                    SysmlPrinter pprinter = new SysmlPrinter(outStream);
                    AstSysmlCompiler compiler = new AstSysmlCompiler();
                    SysmlPropertyDef[] models = compiler.compile(top, top);
                    for (SysmlPropertyDef model: models){
                        pprinter.visit(model, "    ");
                    }

                    InstanceModel instance = solver.instance();
                    instance.printSysml(outStream, "    ");
                    outStream.append("}\n");
                } else {
                    outStream.println("=== Instance " + (++index) + " Begin ===\n");
                    InstanceModel instance = solver.instance();
                    if (prettify)
                        instance.print(outStream);
                    else
                        for (InstanceClafer c : instance.getTopClafers())
                            Utils.printClafer(c, outStream);
                    outStream.println("\n--- Instance " + (index) + " End ---\n");
                }
            }
        }
        if (!sysml) {
            if (timeOn) {
                elapsedTime = (double) (System.nanoTime() - startTime) / 1000000000;
                if (objectives.length == 0)
                    System.out.println("Generated " +                           index + " instance(s) within the scope in " + elapsedTime + " seconds\n");
                else
                    System.out.println("Generated " + (n == -1 ? "all " : "") + index + " optimal instance(s) within the scope in " + elapsedTime + " secondse\n");
            } else {
                if (objectives.length == 0)
                    System.out.println("Generated " +                           index + " instance(s) within the scope\n");
                else
                    System.out.println("Generated " + (n == -1 ? "all " : "") + index + " optimal instance(s) within the scope\n");
            }
        }

        // make sure to close this resource
        if (dataStream != null){
            dataStream.close();
        }
    }
}
