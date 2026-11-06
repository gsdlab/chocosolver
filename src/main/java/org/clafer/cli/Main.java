package org.clafer.cli;

import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import joptsimple.OptionException;
import joptsimple.OptionParser;
import joptsimple.OptionSet;
import joptsimple.OptionSpec;
import org.clafer.compiler.ClaferSearchStrategy;
import org.clafer.javascript.JavascriptFile;

public class Main
{
    /**
     * @return the parser of the command line options
     */
    static OptionParser optionParser() {
        return new OptionParser() {
            {
                accepts( "addtypes", "Add the super clafer of each clafer to the prettified output (the default output always has them)." );
                accepts( "dataFile", "Text file to store time (in seconds) to find each instance.").withRequiredArg()
                        .ofType( File.class ).describedAs("text file");
                accepts( "decimal-digits", "Specify the number of fractional digits of decimals (overrides the model's)." )
                    .withRequiredArg().ofType( Integer.class );
                accepts( "explicit", "The .cfr model is an explicit model (as 'clafer -m clafer' prints it): read it directly, without the Clafer compiler. A model whose first line is '//# OPTIONS --explicit' is one without this option." );
                accepts( "file", "Input file in .cfr or .js format" ).withRequiredArg().ofType( File.class )
                    .describedAs( "Clafer model file (.cfr), explicit or not, or Clafer Javascript file (.js)." );
                accepts( "help", "Show help.").forHelp();
                accepts( "import", "Compile the .cfr model together with the given Clafer file or URL, relative to the current directory (repeatable); a model also imports files with --import in its //# OPTIONS directive." )
                    .withRequiredArg().describedAs( "Clafer file or URL" );
                accepts( "maxint", "Specify maximum integer value." ).withRequiredArg().ofType( Integer.class );
                accepts( "minint", "Specify minimum integer value." ).withRequiredArg().ofType( Integer.class );
                accepts( "moo", "Run in multi-objective optimization mode." );
                accepts( "n", "Specify the maximum number of instances." ).withRequiredArg().ofType( Integer.class );
                accepts( "noprint", "Don't print the instances to the console or a file");
                accepts( "output", "Output instances to the given file." ).withRequiredArg().ofType( File.class ).describedAs( "text file" );
                accepts( "prettify", "Use simple and pretty output format (not formal)." );
                accepts( "repl", "Run in REPL (interactive) mode." );
                accepts( "scope", "Override the default global scope value." ).withRequiredArg().ofType( Integer.class );
                accepts( "scope-file", "Read the scopes of clafers from the given .cfr-scope file (by default, from the model's .cfr-scope file, e.g., model.cfr-scope, if it exists)." ).withRequiredArg().ofType( File.class ).describedAs( "scope file" );
                accepts( "search", "PreferSmallerInstances/PreferLargerInstances/Random" ).withRequiredArg().ofType( ClaferSearchStrategy.class );
                accepts( "time", "Time how long it takes to find all instances (and print if it is turned on");
                accepts( "v", "Run in validation mode; checks all assertions." );
                accepts( "version", "Display the tool version" );
            }
        };
    }

    /**
     * Reports a wrong command line, e.g., an unknown option, without a stack trace, and exits with 1.
     */
    private static void usageError(String message) {
        System.err.println("chocosolver: " + message);
        System.err.println("Run chocosolver --help to see the options.");
        System.exit(1);
    }

    public static void main(String[] args) throws Exception {
        OptionParser parser = optionParser();
        OptionSet options;
        try {
            options = parser.parse(args);
            // jopt-simple converts the values only when they are read, e.g., "-n abc"
            for (OptionSpec<?> spec : options.specs())
                options.valuesOf(spec);
        } catch (OptionException e) { // unknown option, missing or wrong argument
            usageError(e.getMessage());
            return;
        }

        if (options.has("version")) {
            Properties configFile = new Properties();
            try {
                configFile.load(Main.class.getClassLoader().getResourceAsStream("version.properties"));
                String name = configFile.getProperty("name");
                String releaseDate = configFile.getProperty("releasedate");
                String version = configFile.getProperty("version");
                System.out.println(name + " v" + version + "." + releaseDate);
            } catch (IOException e) {
                e.printStackTrace();
            }
            return;
        }
        if (options.has("help")) {
            parser.printHelpOn(System.out);
            return;
        }
        if (!options.has( "file" )) {
            usageError("use --file to provide the input file");
            return;
        }

        File inputFile = (File) options.valueOf("file");

        if (!inputFile.exists()) {
            usageError("the input file does not exist: " + inputFile.getPath());
            return;
        }

        String fileName = inputFile.toString();
        // If a .cfr file is given, compile it first and change the input file to the resulting .js file,
        // unless it is an explicit model, which is read directly
        boolean explicit = Utils.isExplicitModel(inputFile, options);
        if (fileName.endsWith(".cfr") && !explicit) {
            System.out.println("Compiling the Clafer model...");
            String jsFileName = fileName.substring(0, fileName.lastIndexOf(".")) + ".js";
            new File(jsFileName).delete();
            // compile the file; clafer's output (its errors) is shown only if there is no .js file
            String compilerOutput;
            int exitValue;
            try {
                List<String> command = new ArrayList<>(List.of("clafer", "-k", "-m", "choco"));
                for (Object importedFile : options.valuesOf("import"))
                    command.add("--import=" + importedFile);
                command.add(fileName);
                Process compilerProcess = new ProcessBuilder(command).redirectErrorStream(true).start();
                compilerOutput = new String(compilerProcess.getInputStream().readAllBytes());
                exitValue = compilerProcess.waitFor();
            } catch (Exception e) {
                System.out.println("Could not run the Clafer compiler (clafer): " + e.getMessage());
                System.exit(1);
                return;
            }
            // clafer exits with 0 also when the model has no Choco output (e.g., temporal operators)
            if (exitValue != 0 || !new File(jsFileName).exists()) {
                System.out.println(compilerOutput.strip());
                System.out.println("Clafer compilation failed. Aborting...");
                System.exit(1);
            }

            // change the inputFile to the resulting .js file
            fileName = jsFileName;
            inputFile = new File(fileName);
        }


        PrintStream outStream = System.out;

        if (options.has("output"))
        {
            File outputFile = (File) options.valueOf("output");
            outStream = new PrintStream(outputFile);
        }

        if (!inputFile.exists())
        {
            throw new Exception("File does not exist: " + inputFile.getPath());
        }

        // run the different modes
        JavascriptFile javascriptFile = null;
        try {
            if (options.has( "v"))
                System.out.println("=========== Parsing+Typechecking " + fileName + "  =============");
            javascriptFile = Utils.readModel(inputFile, options);
        }
        catch(IllegalArgumentException e) {
            if (!explicit)
                throw e;
            // the errors of an explicit model, with their positions
            System.out.println(e.getMessage());
            System.exit(1);
            return;
        }
        catch(Exception e) {
            System.out.println("Unhandled compilation error occured. Please report this problem.");
            System.out.println(e.getMessage());
            return;
        }

        if (options.has("v"))
            Validate.runValidate(inputFile, javascriptFile, options, outStream);
        else if (options.has("repl"))
            REPL.runREPL(inputFile, javascriptFile, options);
        else
            Normal.runNormal(inputFile, javascriptFile, options, outStream);
    }
}
