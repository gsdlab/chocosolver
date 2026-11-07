# chocosolver version 0.5.7, unreleased

* Minor release with Clafer 0.5.7.
* The names of clafers are their UIDs, which are their names since Clafer 0.5.7, qualified only where a name is not unique (`x__size`): the instances, also with `--prettify`, show them as they are. They cut a UID after its first `_` (`c0_size` was `size`, and `x__size` would have been `_size`).
* A name in a scope file or a REPL command is a UID or a qualified name with `__`, e.g., `Cfeatures` for all the clafers named so, `Component__Cfeatures` for those in a `Component`, and `root__Component__Cfeatures` for the one in a top-level `Component`; the older form `Component::Cfeatures` and `::Component::Cfeatures` is read too. The names are matched with the UIDs of the clafer and its ancestors, which no longer have a prefix to cut off.
* A join into a nested abstract clafer from a clafer that can be unused, e.g., `[ some this.parent.Task ]` under an optional child of `abstract System` with `abstract Task`, crashed with an `ArrayIndexOutOfBoundsException` when the abstract clafer's parent has several instances, e.g., two systems in one model.
* After the linear equation optimizer could not rewrite the equations of one model exactly, e.g., ones over integers up to 100000, it also kept the original equations of every later model in the same JVM, e.g., the REPL after a `reload`, alongside the rewritten ones. The result was the same but the models were larger. The flag was global and never reset; it now belongs to one optimization. Its tests failed depending on the order the test classes ran in.
* A wrong command line, i.e., an unknown option, an option without its argument or with a wrong one (e.g., `-n abc`, `--search Fastest`), no `--file` or an input file that does not exist, prints one line, e.g., `chocosolver: bogus is not a recognized option`, and a pointer to `--help`, and exits with 1. It ended with a Java stack trace, and a wrong argument only after compiling the model.
* A model that cannot be read, e.g., a `.js` file with a syntax error, exits with 1 after its error (`Unhandled compilation error occured`), also with `-v`: Clafer's `make test` validates its outputs so. It exited with 0.
* jopt-simple 5.0.4 (was 5.0.2), which parses the command line, and maven-assembly-plugin 3.8.0 (was 3.7.1), which builds the jar with dependencies. choco-solver 6.0.2 is already the latest release.
* Rhino 1.9.2 (was 1.9.1), which runs the JavaScript that Clafer generates, and SLF4J 2.0.20 (was slf4j-jdk14 1.7.21 from 2016, with slf4j-api 1.7.36), which passes the log messages of choco-solver's dependencies to `java.util.logging`.
* Explicit models are read directly, without the Clafer compiler: a `.cfr` file whose first line is `//# OPTIONS --explicit`, as `clafer -m clafer` writes it, or any `.cfr` file with `--explicit`, is read by `ExplicitParser`, with its scopes from its scope file (`model.des.cfr-scope` for `model.des.cfr`). Its errors are printed with their positions, e.g., `model.des.cfr:3:11: No clafer named y`, and chocosolver exits with 1. The REPL's `reload` reads it the same way, and `saveScopes` writes the scope file of any model name (it cut three characters off the name, i.e., the `.js`). For all 132 positive test models of Clafer with a Choco output, the explicit model and its scope file give the same instances as the `.js`. `AstPrinter.print` of a model with its objectives and assertions starts with that line.
  * The explicit model of a temporal model, which `clafer -m clafer` also writes, is refused with one error for each line with a temporal modifier or operator, e.g., `model.des.cfr:3:3: Chocosolver does not solve temporal models: initial is a temporal modifier; Alloy solves them (claferIG)`, in place of the errors that follow from them (`No clafer named X`).
* Java API, in preparation for a Clafer parser in Java:
  * The AST's expressions are records, and `AstExpr`, `AstSetExpr` and `AstBoolExpr` are sealed interfaces, so a pass over expressions is a `switch` that the compiler checks for a missing case. `AstExprVisitor` and `accept` are gone; `AstExprRewriter` dispatches with a `switch`, and its subclasses override the same `visit` methods as before. The accessors are the records' components, e.g., `join.left()` instead of `join.getLeft()`, `childRelation.childType()` instead of `getChildType()`, and `closure.reflexive()` instead of `isReflexive()`. Expressions are equal when their parts are, except locals (`AstLocal` stays a class), which are different variables even with the same name.
  * Source positions: `SourceSpan` (line and column, from 1, of the start and the end) on clafers (`clafer.at(span)`), constraints (`addConstraint(expr, span)`), assertions and objectives. A type error in a constraint, assertion or objective with a span starts with its position, e.g., `5:1: Cannot ...`, and the constraints of an unsat core or a Min-Unsat are printed with theirs, e.g., `[#(A) >= 1] at 2:5-2:14`. The JavaScript that Clafer generates has no positions yet, so the output is unchanged.
  * `AstPrinter` prints a model in a Clafer-like syntax with nothing implicit: UIDs, every card and group card as it applies, inherited ones too, `this`, `parent` and `dref`, and the objectives and assertions. Children, constraints and references are printed only where they are declared: in Clafer, a reference written on a sub clafer redefines the inherited one, even with the same target, so `ExplicitParser` takes a written reference as the clafer's own. An empty set, of any type, is printed as Clafer's `noclafer`, and parsed back as an empty set of the clafer of the set beside it (the other operand, or the other branch of an `if`), or of the model's first clafer. Nesting is in braces, `{}` for an empty clafer, so whitespace carries no meaning; the indentation is only for reading. The output is a Clafer model: `clafer -f -k -l --ss=none -m choco` compiles 129 of the 131 models to the same `.js` as the original's, up to the compiler's renaming of the UIDs (`c0_c0_A` for `c0_A`) in models with inheritance; strings stay Clafer literals, and a quantifier's names are separated by `;`, as in Clafer. The scopes are not in the model but in its `.cfr-scope` file.
  * `org.clafer.frontend.Lexer` splits this explicit form into tokens with their spans. Comments are set aside, and an error (an unexpected character, an unterminated string or comment, an unknown escape) is a diagnostic after which lexing goes on.
  * `org.clafer.frontend.ExplicitParser` reads the explicit form back into a model with its objectives and assertions, with the spans of the source, and reports errors as diagnostics and goes on. The round trip `.js` → model → explicit form → model prints the same and gives the same instances in the same order for all of Clafer's 131 positive test models with a Choco output; for the `.js` files that Clafer generated in 2015 (35 of chocosolver's 110 test models), which create all abstract clafers first, the instances are numbered differently. `AstPrinter` writes the abstract and concrete children of a clafer in the order of the source.
  * `org.clafer.scope.ScopeFile` reads and writes `.cfr-scope` files, which can now give every scope: besides `{"lpqName": name, "scope": n}`, the entries `{"defaultScope": n}`, `{"int": [low, high]}`, `{"mul": [low, high]}`, `{"stringLength": n}`, `{"chars": [low, high]}`, `{"decimalDigits": n}` and `{"decimal": [low, high]}`. `ScopeFile.print` writes all of them, every clafer by its UID, for the explicit form. On the command line, the file's scopes and ranges override the model's, and `--scope`, `--minint`, `--maxint` and `--decimal-digits` override the file's. A malformed file is an error that names the file and what is wrong, e.g., `model.cfr-scope: unknown entry "ints"`; files with only clafer scopes are read as before.

# chocosolver version 0.5.6, Nov 1, 2026

* `--addtypes`: with `--prettify`, each clafer of an instance is printed with its super clafer, e.g., `a1 : A -> e1`, as with `claferIG --addtypes`, in the normal, validation and REPL modes ([#43](https://github.com/gsdlab/chocosolver/issues/43)).
* Abstract clafers nested under concrete clafers, e.g., `abstract Port` under `System`, which Clafer compiles to `System.addAbstractChild("Port")` and chocosolver rejected with "Cannot find function addAbstractChild". As in claferIG, the concrete clafer is the parent of the abstract clafer's instances, and the instances are the same as claferIG's ([#23](https://github.com/gsdlab/chocosolver/issues/23)).
* Unsatisfiable models that crashed: a reference with no possible value, e.g., `x -> integer` with `[ x = 1 ]` and `[ x = 2 ]`, gave an `IllegalIntException` instead of no instances, and the `unsatCore` and `minUnsat` REPL commands gave an `UnsatisfiableException` when the cardinalities and scopes contradict each other without any constraint, e.g., `xor g` with `a 2` and `b 2`; they now say so.
* The instance of a clafer that redefines the reference of its super clafer, e.g., `a1 : A -> e1`, contains the children inherited from the super clafer; they were solved but not printed ([#42](https://github.com/gsdlab/chocosolver/issues/42)).

# chocosolver version 0.5.5, Oct 27, 2026

* Upgrade to choco-solver 6.0.2. The instances, Pareto fronts and the search of the default strategy are the same as with 4.10.18, and so are the times; `pickOnDom` and `conflictHistory` finish models that hit the time limit before.
* Java API: Choco's solver is final, so `ResetKeepsLimitsSolver` is replaced by `ResetKeepsLimits`. Time limits set with `ClaferSearch.limitTime` still hold across the restarts of the search; limits set on the Choco solver directly do not.
* Decimals: a primitive type `decimal` of numbers with a fixed number of fractional digits, the decimal scale of the model, solved as scaled integers, which Clafer 0.5.5 compiles to ([clafer#102](https://github.com/gsdlab/clafer/pull/102)). In the Javascript format: `refTo(Decimal)`, `decimal("19.99")`, `decimalScale(2)` and `decimalRange("-10", "10")`; on the command line, `--decimal-digits` overrides the scale. `decimal * decimal` and divisions with a decimal round half away from zero; a division of integers stays an integer division. The default range is the largest that fits Choco's integers, about ±214748.35 with 2 digits. Instances print decimals in their shortest form with at least one fractional digit, e.g. `199.0` and `12.5`. See [Decimals](README.md#decimals).
* Intervals of more than 65536 values are bounded domains in Choco instead of enumerated ones, and the products of such domains are no longer enumerated.
* Scope files: the CLI reads the scopes of clafers from the model's `.cfr-scope` file (`model.cfr-scope` next to `model.cfr` or `model.js`) if it exists, or from the file given by `--scope-file`, in all modes, and prints `Using the scopes from model.cfr-scope`. The file is a list of `{"lpqName": name, "scope": n}`, as `clafer --meta-data`, claferIG's and the REPL's `saveScopes` write it; a name is a UID or a (partially) qualified name, e.g., `Component::Cfeatures`, and a name that is not of a clafer in the model is an error. E.g., ClaferCaseStudies' `AADL_Complete.cfr-scope` makes its optimization 6 times faster with the same optimal instances. See [Getting started with the CLI](README.md#getting-started-with-the-cli).

# chocosolver version 0.5.4, Oct 25, 2026

* Models in several files: `--import=FILE` compiles the `.cfr` model together with the given file or URL, as the compiler's option does ([clafer#100](https://github.com/gsdlab/clafer/issues/100)); the files that the model imports in its `//# OPTIONS` directive are compiled with it too.
* Fixes: a constraint of an abstract clafer over its reference, when a sub clafer refines the reference to a subtype of the target (`abstract T -> Form`, `A : T -> Vector`), read the sub clafer's reference as another instance of the target, so a model could have no instances; `parent.parent` of an optional child of a nested abstract clafer with a larger scope than its parent crashed with an `ArrayIndexOutOfBoundsException` (also in the unsat core).
* With a `.cfr` model, the command line shows the compiler's errors when the model does not compile, or why there is no Choco output (e.g., temporal operators), instead of "File does not exist: model.js". It no longer uses a `.js` file left over from an earlier compilation, and file names may contain spaces.

[Release](https://github.com/gsdlab/chocosolver/pull/37)

# chocosolver version 0.5.3, Oct 15, 2026

* Upgrade to choco-solver 4.10.18.
* Time limits are a deadline for the whole search, including the restarts of optimization.
* The default search strategy is failureRateBased; a benchmark of the strategies is in `src/test/benchmark`.
* The command line uses the integer range of the model (`intRange`, from clafer's `--minint` and `--maxint`); `--minint` and `--maxint` still override it.
* Fixes: multiplication with integer ranges beyond 46340; constant folding of a subarray past the end; `Util.pairwise` reusing a consumed stream; a null in `Oracle.compile`; silent overflow in `Rational`; the scope and cardinality analysis of mutually recursive clafers.
* `parent` of a top-level abstract clafer is the union of the parents of its subclafers ([clafer#84](https://github.com/gsdlab/clafer/issues/84)).

[Release](https://github.com/gsdlab/chocosolver/pull/36)

# chocosolver version 0.5.2 released on Oct 9, 2026

[Release](https://github.com/gsdlab/chocosolver/pull/35)

# chocosolver version 0.4.4 released on Sep 19, 2016

[Release](https://github.com/gsdlab/chocosolver/pull/27)

# chocosolver version 0.4.3 released on Dec 22, 2015

[Release](https://github.com/gsdlab/chocosolver/pull/26)

# chocosolver version 0.4.2.1 released on Nov 03, 2015

[Release](https://github.com/gsdlab/chocosolver/pull/22)

# chocosolver version 0.4.2 released on Oct 16, 2015

[Release](https://github.com/gsdlab/chocosolver/pull/19)

# chocosolver version 0.4.0 released on Jul 29, 2015

[Release](https://github.com/gsdlab/chocosolver/pull/20)

# chocosolver version 0.3.10.1 released on May 13, 2015

[Release](https://github.com/gsdlab/chocosolver/pull/13)

# chocosolver version 0.3.10 released on Apr 23, 2015

[Release](https://github.com/gsdlab/chocosolver/pull/9)

# chocosolver version 0.3.9 released on Mar 07, 2015

[Release](https://github.com/gsdlab/chocosolver/pull/8)

# chocosolver version 0.3.8 released on Jan 29, 2015

[Release](https://github.com/gsdlab/chocosolver/pull/7)

# chocosolver version 0.3.7 released on Oct 24, 2014

[Release](https://github.com/gsdlab/chocosolver/pull/5)

# chocosolver version 0.3.6.1 released on Jul 08, 2014

[Release](https://github.com/gsdlab/chocosolver/pull/4)
