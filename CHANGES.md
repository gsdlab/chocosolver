# chocosolver version 0.5.4, unreleased

* Models in several files: `--import=FILE` compiles the `.cfr` model together with the given file or URL, as the compiler's option does ([clafer#100](https://github.com/gsdlab/clafer/issues/100)); the files that the model imports in its `//# OPTIONS` directive are compiled with it too.
* Fixes: a constraint of an abstract clafer over its reference, when a sub clafer refines the reference to a subtype of the target (`abstract T -> Form`, `A : T -> Vector`), read the sub clafer's reference as another instance of the target, so a model could have no instances; `parent.parent` of an optional child of a nested abstract clafer with a larger scope than its parent crashed with an `ArrayIndexOutOfBoundsException` (also in the unsat core).
* With a `.cfr` model, the command line shows the compiler's errors when the model does not compile, or why there is no Choco output (e.g., temporal operators), instead of "File does not exist: model.js". It no longer uses a `.js` file left over from an earlier compilation, and file names may contain spaces.

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
