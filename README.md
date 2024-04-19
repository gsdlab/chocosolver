[![Build Status](https://github.com/EthanJamesLew/chocosolver-sysml/actions/workflows/maven.yml/badge.svg)](https://github.com/EthanJamesLew/chocosolver-sysml/actions/workflows/maven.yml)

# chocosolver

##### v0.4.4

An instance generator and multi-objective optimizer backend for [Clafer](http://clafer.org) using the Choco 4.0.0b constraint programming library. There are two ways to use the project:
programmatically via the Java API or via the command-line interface (CLI).

The CLI is used by
[ClaferIDE](https://github.com/gsdlab/ClaferIDE),
[ClaferMooVisualizer](https://github.com/gsdlab/ClaferMooVisalizer), and [ClaferConfigurator](https://github.com/gsdlab/ClaferConfigurator).

Note: the name "choco-solver" refers to the [Java library for Constraint Programming](http://choco-solver.org/), whereas "chocosolver" (without the "-") is the name of this project. We might change the name in the future to avoid confusion.

## Contributors

* [Jimmy Liang](http://gsd.uwaterloo.ca/jliang), Ph.D. Candidate. Main developer.
* [Michal Antkiewicz](http://gsd.uwaterloo.ca/mantkiew), Research Engineer. Release Management, development, testing.
* [Alexandr Murashkin](http://gsd.uwaterloo.ca/amurashk). CLI, stress testing.
* [Jordan Ross](http://gsd.uwaterloo.ca/j25ross). Stress testing.

## Getting Clafer Tools

### Installation from binaries

Binary distributions of the release 0.4.4 of Clafer Tools for Windows, Mac, and Linux, can be downloaded from [Clafer Tools - Binary Distributions](http://http://gsd.uwaterloo.ca/clafer-tools-binary-distributions).

1. download the binaries and unpack `<target directory>` of your choice
2. add the `<target directory>` to your system path so that the executables can be found

### Integration with Sublime Text 2/3

See [ClaferToolsST](https://github.com/gsdlab/ClaferToolsST).

### Installation from the source code

## Prerequisites

* [Choco 4.0](https://github.com/chocoteam/choco-solver), v4.0.0.b.
* [Java 8+](http://www.oracle.com/technetwork/java/javase/downloads/index.html), 64bit.
* [Maven 3.2+](http://maven.apache.org/). Required for building the project.

## Recommended but optional

* [Clafer compiler](https://github.com/gsdlab/clafer), v0.4.4.
  - This backend provides an API for solving Clafer models. The Clafer compiler can compile a Clafer model down to the proper API calls.
  - The API calls can also be written manually quite easily with a bit of extra typing (examples down below).

Follow the installation instructions in the [README.md](https://github.com/gsdlab/clafer#clafer).

## Installation

Install the API and the CLI.

```bash
git clone https://github.com/gsdlab/chocosolver.git
cd chocosolver
mvn install -DskipTests
```

Include the following XML snippet in your POM to use the API in your Maven project.

```xml
<dependency>
    <groupId>org.clafer</groupId>
    <artifactId>chocosolver</artifactId>
    <version>0.4.4</version>
</dependency>
```

The CLI is installed to `target/chocosolver-0.4.4-jar-with-dependencies.jar`;
however, in the Clafer Tools binary distribution it is called `chocosolver.jar`.

You can deploy the files the same way as in Clafer Tools binary distribution by executing

```bash
make install to=<target path>
```

### Important: branches must correspond

All related projects are following the *simultaneous release model*.
The branch `master` contains releases, whereas the branch `develop` contains code under development.
When building the tools, the branches should match.
Releases from branches `master` are guaranteed to work well together.
Development versions from branches `develop` should work well together but this might not always be the case.


# Getting started with the CLI

For usage instructions, start the CLI using the command

```sh
java -jar chocosolver.jar --help`

Option                                  Description
------                                  -----------
--file <File: Clafer model file (.cfr)  Input file in .cfr or .js format.
  or Clafer Javascript file (.js)>
--help                                  Show help.
--maxint <Integer>                      Specify maximum integer value.
--minint <Integer>                      Specify minimum integer value.
--moo                                   Run in multi-objective optimization mode.
-n <Integer>                            Specify the maximum number of instances.
--output <File: text file>              Output instances to the given file.
--prettify                              Use simple and pretty output format (not formal).
--repl                                  Run in REPL (interactive) mode.
--scope <Integer>                       Override the default global scope value.
--search <ClaferSearchStrategy>         PreferSmallerInstances/PreferLargerInstances/Random.
-v                                      Run in validation mode; checks all assertions.
--version                               Display the tool version.
```

The CLI supports four modes of operation:

* `-v` - validation mode,
* `--repl` - interactive mode,
* `-moo` - batch multi-objective optimization mode, and
* (default) batch instance generation mode.

The last mode is selected when no `-v` and `--repl` are given and the model does not contain optimization objectives.

If the `.cfr` file is given, the CLI will first call the Clafer compiler using `clafer -k -m choco model.cfr`,
which will produce `model.js`.
Certain features, such as transitive closure, are not yet supported by Clafer,
so they can only be used directly via API call in the `.js` file.
We recommend using the so called Choco escapes, as follows:


```
[choco|
<your code to be inserted verbatim at the end of the generated output>
|]
```

### CLI interactive mode

When running the interactive mode, the CLI automatically produces an instance and then is ready to accept commands.
Here is an example session:

```
Type 'help' for the list of available REPL commands

=== Instance 1 Begin ===

Bob
  owns -> car
car
  transmission
    manual
  drivenBy -> Bob

--- Instance 1 End ---


ClaferChocoIG> h
'h'elp                              Print the REPL commands.
'n'ext                              Generate the next instance.
<enter>                             Generate the next instance.
'p'rettify                          Toggle prettify on/off.
'r'eload                            Reload the model from the same <file-name.js> file.
'u'nsatCore                         Compute the set of contradicting constraints if any.
min'U'nsat                          Compute the minimal UnSAT core and a near-miss example.
'S'etGlobalScope <value>            Set the global scope to the <value> .
's'etScope <claferUID> <value>      Set the scope of the given clafer to the <value>.
'I'ncGlobalScope <value?>           Increase the global scope by <value> or 1 by default.
'i'ncScope <claferUID> <value?>     Increase the scope of the given clafer by the <value>  or 1 by default.
sa'v'eScopes                        Save the currect scopes to a .cfr-scope file.
'm'axInt <value>                    Set the largest allowed integer to <value>.
ma`x`imize <claferUID>              Find a solution where <claferUID>.dref is maximal.
minimiz'e' <claferUID>              Find a solution where <claferUID>.dref is minimal.
sta't's                             Display statistics about the current search.
'O'ptions                           Display the current solver options.
strate'g'y <smaller|larger|random>  Set search strategy to prefer smaller, prefer larger, or random.
'o'ptimizations                     Toggle optimizations basic/full.
symmetry'B'reaking                  Toggle symmetry breaking basic/full.
'q'uit                              Exit the REPL sesssion.

ClaferChocoIG>
```

Each command can be invoked using a full name, e.g., `IncGlobalScope`, or using a shortcut, e.g., `I`, as indicated by `'I'`.
The CLI is case sensitive, that is, `i` is for `incScope` command, whereas, `I` is for `IncGlobalScope`.

## Getting Started with the Java API

In general, the CLI classes are good examples of using the API:

* [Main.java](src/main/java/org/clafer/cli/Main.java) - this is the main class of the `chocosolver.jar`.
* [Validate.java](src/main/java/org/clafer/cli/Validate.java) - checks assertions (run with `-v`).
* [REPL.java](src/main/java/org/clafer/cli/REPL.java) - read, eval, print loop (interactive interface, run with `--repl`).
* [Normal.java](src/main/java/org/clafer/cli/Normal.java) - runs batch instance generation or multi-objective optimization up to the given limit of instances (provided using `-n`) or unlimited (by default).
* [Util.java](src/main/java/org/clafer/cli/Util.java) - various shared utils.

Consider the following Clafer model.

```
Installation
    xor Status
        Ok
        Bad
    Time -> integer
        [this > 2]
```

Below is an example of using the API to build the model above. [AstModel](http://gsdlab.github.io/chocosolver/org/clafer/ast/AstModel.html) represents the implicit "root" of the model.
Every Clafer in the model is nested below it.

```java
import org.clafer.ast.*;
import static org.clafer.ast.Asts.*;

public static void main(String[] args) {
    AstModel model = newModel();

    AstConcreteClafer installation = model.addChild("Installation").withCard(Mandatory);
        // withCard(Mandatory) and withCard(1, 1) is the same. Pick the one you find more readable.
        AstConcreteClafer status = installation.addChild("Status").withCard(1, 1).withGroupCard(1, 1);
            AstConcreteClafer ok = status.addChild("Ok").withCard(Optional);
            // withCard(Optional) and withCard(0, 1) is the same.
            AstConcreteClafer bad = status.addChild("Bad").withCard(0, 1);
            // Note that ok and bad have an explicit optional cardinality, whereas
            // it was implicit in the oringal model.
        AstConcreteClafer time = installation.addChild("Time").withCard(1, 1).refTo(IntType);
            time.addConstraint(greaterThan(joinRef($this()), constant(2)));
            // Note that joinRef is explicit whereas it was implicit in the original model.
}
```

The [Asts](http://gsdlab.github.io/chocosolver/org/clafer/ast/Asts.html) class provides all the functions required for building the model. See the previous link for a list of all the supported expressions. More examples of building models: [structure](https://github.com/gsdlab/chocosolver/blob/master/src/test/java/org/clafer/SimpleStructureTest.java), [expressions](https://github.com/gsdlab/chocosolver/blob/master/src/test/java/org/clafer/SimpleConstraintTest.java), [quantifiers](https://github.com/gsdlab/chocosolver/blob/master/src/test/java/org/clafer/QuantifierTest.java), and [attributed feature models](https://github.com/gsdlab/chocosolver/blob/master/src/test/java/org/clafer/FeatureModelTest.java#L229).

### Finding Instances

The next step is to solve the model.

```java
import org.clafer.compiler.*;
import org.clafer.scope.*;

public static void main(String[] args) {
    ...
    ClaferSolver solver = ClaferCompiler.compile(model,
        Scope.setScope(installation, 1).setScope(status, 1).setScope(ok, 1).setScope(bad, 1).setScope(time, 1)
        // Set the scope of every Clafer to 1. The code above could be replaced with
        // "Scope.defaultScope(1)".
        .intLow(-16).intHigh(16));
        // intLow is the "suggested" lowest integer for solving. intHigh is the "suggested"
        // highest integer.
    // find will return true when the solver finds another instance.
    while (solver.find()) {
        // Print the solution in a format similar to ClaferIG.
        System.out.println(solver.instance());
    }
}
```

### Finding Optimal Instances

Optimizing on a single objective is supported. Suppose we wanted to optimize on the expression "sum time".

```java
import org.clafer.objective.*;
...
ClaferOptimizer solver = ClaferCompiler.compile(model,
    Scope.defaultScope(1).intLow(-16).intHigh(16),
    Objective.maximize(sum(global(time))));
while (solver.find()) {
    // The instances where time is maximal.
    System.out.println(solver.instance());
}

solver = ClaferCompiler.compile(model,
    Scope.defaultScope(1).intLow(-16).intHigh(16),
    Objective.minimize(sum(global(time))));
while (solver.find()) {
    // The instances where time is minimal.
    System.out.println(solver.instance());
}
```

The compiler can accept multiple objectives, in which case it will solve for all the Pareto-optimal solutions.

### Finding Min-Unsat

Consider the following Clafer model.
```clafer
Mob
Duck ?
Witch ?
Floats ?
[Floats => Duck]
[Floats <=> Witch]
[!Duck]
[Witch]
```
The model is overconstrained and has no solutions. The solver can help here as well.

```java
AstModel model = newModel();
AstConcreteClafer mob = model.addChild("Mob").withCard(0, 1);
AstConcreteClafer duck = model.addChild("Duck").withCard(0, 1);
AstConcreteClafer witch = model.addChild("Witch").withCard(0, 1);
AstConcreteClafer floats = model.addChild("Floats").withCard(0, 1);
model.addConstraint(some(mob));
model.addConstraint(implies(some(floats), some(duck)));
model.addConstraint(ifOnlyIf(some(floats), some(witch)));
model.addConstraint(none(duck));
model.addConstraint(some(witch));

ClaferUnsat unsat = ClaferCompiler.compileUnsat(model, Scope.defaultScope(1));
// Print the Min-Unsat and near-miss example.
System.out.println(unsat.minUnsat());
```

The above code will print two things.
First it will print "#(Witch) >= 1" which is the constraint that is unsatisfiable in the model, i.e., the last constraint that enforces there to be some witch.
Next, it will print the near-miss example `Mob#0`.
It means that removing the `some(Witch)` constraint would make the model satisfiable and an example of a solution (after removing the constraint), is the instance with exactly one mob and nothing else.
For this example, the min-unsat is not unique, so it is possible that the library may report another set of constraints although the set of constraints is guaranteed to have a size of one.

### Finding Unsat-Core

The above example found that removing one constraint will *fix* the model but you may be wondering why the model cannot have a witch.
In this case, it is more useful to compute the unsat-core instead.

```java
ClaferUnsat unsat = ClaferCompiler.compileUnsat(model, Scope.defaultScope(1));
// Print the Unsat-Core.
System.out.println(unsat.unsatCore());
```
The above code will print the last 4 constraints which are the constraints that are mutually unsatisfiable. What this means is that if you removed all constraints but these 4, the model is still unsatisfiable. The set of constraints is not guaranteed to be minimal but will likely be small.

### Javascript vs Java

If you prefer to use the Javascript API over the Java API:
```java
import org.clafer.javascript.Javascript;

public static void main(String[] args) {
    Javascript.readModel(
                "scope({Installation:1, Status:1, Ok:1, Bad:1, Time:1});" +
                "intRange(-16, 16);" +
                "Installation = Clafer('Installation').withCard(1, 1);" +
                ...
}
```

## Configuring as a Web Tool Backend

The configuration is done in the `<host-tool-path>/Server/Backends/backends.json` file, where `<host-tool-path>` is a path to the web-based tool (`ClaferIDE`, etc.) you want to configure to use `chocosolver` as a backend.

* An example configuration for [ClaferIDE](https://github.com/gsdlab/ClaferIDE):

```json
{
    "backends": [
        {
            "id": "chocoIG",
            "label": "Choco-based (IG + MOO)",
            "tooltip": "An instance generator and multi-objective optimizer based on Choco3 solver library",
            "accepted_format": "choco",
            "tool": "java",
            "tool_args": ["-jar", "/home/<user>/bin/chocosolver.jar", "--file=$filepath$", "--repl", "--prettify"],
            "tool_version_args": ["-jar", "/home/<user>/bin/chocosolver.jar", "--version"],
            "scope_options": {
                "set_default_scope" : {"command": "SetGlobalScope $value$\n", "label": "Default:", "argument": "--scope=$value$", "default_value": 1},
                "set_individual_scope": {"command": "setScope $clafer$ $value$\n"},
                "inc_all_scopes" : {"command": "IncGlobalScope $value$\n", "label": "All:", "default_value": 1},
                "inc_individual_scope": {"command": "incScope $clafer$ $value$\n"},
                "produce_scope_file" : {"command": "saveScopes\n"},
                "set_int_scope" : {"command": "maxInt $value$\n", "label": "Max. integer:", "argument": "--maxint=$value$", "default_value": 127}
            },
            "control_buttons": [
                {"id": "next_instance", "command": "n\n", "label" : "Next", "tooltip": "Next Instance"},
                {"id": "reload", "command": "r\n", "label" : "Reload", "tooltip": "Reload the model preserving scopes and other settings"},
                {"id": "quit", "command": "q\n", "label" : "Quit", "tooltip": "Exit the IG safely"}
            ],
            "presentation_specifics": {
                "prompt_title": "ChocoIG> "
            }
        },
    ]
}

```

* An example configuration for [ClaferMooVisualizer](https://github.com/gsdlab/ClaferMooVisualizer):

```json
{
    "backends": [
        {
            "id": "choco_moo",
            "label": "Choco-based (MOO with magnifier)",
            "tooltip": "A multi-objective optimizer based on Choco3 solver library",
            "accepted_format": "choco",
            "tool": "java",
            "tool_args": ["-jar", "/home/<user>/bin/chocosolver.jar", "--file=$filepath$", "--moo"],
            "tool_version_args": ["-jar", "/home/<user>/bin/chocosolver.jar", "--version"],
            "optimization_options": {
                "set_int_scope" : {"label": "Max. integer:", "argument": "--maxint=$value$", "default_value": 127},
                "set_default_scope" : {"label": "Default scopes:", "argument": "--scope=$value$", "default_value": 25}
            }
        },
    ]
}
```

* An example configuration for [ClaferConfigurator](https://github.com/gsdlab/ClaferConfigurator):

```json
{
    "backends": [
        {
            "id": "chocoIG",
            "label": "Choco-based (IG + MOO)",
            "tooltip": "An instance generator based on Choco3 solver library",
            "accepted_format": "choco",
            "tool": "java",
            "tool_args": ["-jar", "/home/<user>/bin/chocosolver.jar", "--file=$filepath$", "--repl"],
            "tool_version_args": ["-jar", "/home/<user>/bin/chocosolver.jar", "--version"],
            "scope_options": {
                "set_default_scope" : {"command": "SetGlobalScope $value$\n"},
                "set_individual_scope": {"command": "setScope $clafer$ $value$\n"},
                "inc_all_scopes" : {"command": "IncGlobalScope $value$\n"},
                "inc_individual_scope": {"command": "incScope $clafer$ $value$\n"},
                "set_int_scope" : {"command": "maxInt $value$\n", "default_value": 127}
            },
            "control_buttons": [
                {"id": "next_instance", "command": "n\n", "label" : "Next", "tooltip": "Next Instance"},
                {"id": "reload", "command": "r\n", "label" : "Reset", "tooltip": "Reset instance generation, applied scopes and other settings"},
                {"id": "quit", "command": "q\n", "label" : "Quit", "tooltip": "Exit the IG safely"}
            ],
            "presentation_specifics": {
                "prompt_title": "",
                "no_more_instances": "No more instances found. Please consider increasing scopes"
            }
        },
    ]
}
```

**Notes**:

* If you make any changes to the `backends.json`, restart the web-tool completely to make the changes take effect.
* If you done your configuration properly, the tool will restart successfully and the backend should be listed in the `Backends` dropdown list.
If the tool does not start, the reason may be either a syntax error in the `backends.json` file, or the paths specified in it are not correct or lead to an inaccessible `jar` file.
Also, check the `eXecute` permission on the `jar` file.

## Semantic Differences with the Alloy backend

Consider the following constraint.

```
[5 + 5 = -6]
```

For this backend, the constraint is always unsatisfiable.
For the Alloy backend, the constraint can be satisfied, depending on the set bitwidth.
Why? In the default bitwidth of 4, the number succeeding 7 is -8. Hence 5 + 5 is translated to 5 -> 6 -> 7 -> -8 -> -7 -> -6, hence the constraint is true.
For any other bitwidth, the constraint is false.
Overflow can be a problem for low bitwidths when dealing with arithmetic (in the new Alloy there's an option which prevents overflows).
This backend can also suffer from overflow.
It essentially, in regards to overflow, has a fixed bitwidth of 32.

## Possible Future Work?

* API for choosing branching strategy. Two reasons. The advantage of constraint programming is the ability to tune the solver to the specific problem. Choosing the right branching strategy can make a world of difference. Secondly, it allows the user to control the order of instances generated. For example, the user would like to see instances where Feature A is present and Feature B is absent before any other instances. This can be done by choosing the branching strategy.
* Reals

# Need help?

* Visit [language's website](http://clafer.org).
* Report issues to [issue tracker](https://github.com/gsdlab/chocosolver/issues)
