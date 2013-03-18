importClass(Packages.org.clafer.Exprs);
importClass(Packages.org.clafer.tree.AtomicClafer);
importClass(Packages.org.clafer.tree.Card);
importClass(Packages.org.clafer.tree.IntConstraint);
importClass(Packages.org.clafer.tree.SetConstraint);

var __clafer__choco__exprs;
var __clafer__choco__solution = true;
var __clafer__choco__statistics = false;
var __clafer__choco__branching = false;
var __clafer__choco__solve = 1;
var int;

function showSolutions() {
    __clafer__choco__solution = true;
}

function hideSolutions() {
    __clafer__choco__solution = false;
}

function showStatistics() {
    __clafer__choco__statistics = true;
}

function hideStatistics() {
    __clafer__choco__statistics = false;
}

function showBranching() {
    __clafer__choco__branching = true;
}

function hideBranching() {
    __clafer__choco__branching = false;
}

function solveAll() {
    __clafer__choco__solve = 0;
}

function solve(n) {
    if (typeof n === "undefined") throw "solve: n is undefined"
    __clafer__choco__solve = n;
}

function maximize(objective) {
    if (typeof objective === "undefined") throw "maximize: objective is undefined"
    getExpr().objective(objective);
}

function intRange(low, high) {
    if (typeof low === "undefined") throw "intRange: low is undefined";
    if (typeof high === "undefined") throw "intRange: high is undefined";
    __clafer__choco__exprs = new Exprs(low, high);
    int = __clafer__choco__exprs.getIntType();
}

function getExpr() {
    if (typeof __clafer__choco__exprs === "undefined") throw "Did not set the integer range yet";
    return __clafer__choco__exprs;
}

function clafer(name, scope, card) {
    if (typeof name === "undefined") throw "clafer: name is undefined";
    if (typeof scope === "undefined") throw "clafer: scope is undefined";
    if (typeof card === "undefined") throw "clafer: card is undefined";
    return getExpr().newTopClafer(name, scope, card);
}

function abstract(name, scope) {
    if (typeof name === "undefined") throw "abstract: name is undefined";
    if (typeof scope === "undefined") throw "abstract: scope is undefined";
    return getExpr().newAbstractClafer(name, scope);
}

function card(low, high) {
    if (typeof low === "undefined") {
        if (typeof high === "undefined") {
            return new Card();
        }
        return new Card(0, high);
    }
    if (typeof high === "undefined") {
        return new Card(low);
    }
    return new Card(low, high);
}

function eq(e1, e2) {
    if (typeof e1 === "undefined") throw "eq: e1 is undefined";
    if (typeof e2 === "undefined") throw "eq: e2 is undefined";
    return getExpr().eq(e1, e2);
}

function constantInt(i) {
    if (typeof i === "undefined") throw "constantInt: i is undefined";
    return getExpr().constantInt(i);
}

function upcast(set) {
    if (typeof set === "undefined") throw "upcast: set is undefined";
    return getExpr().upcast(set);
}

function upcastTo(set, target) {
    if (typeof set === "undefined") throw "upcastTo: set is undefined";
    if (typeof target === "undefined") throw "upcastTo: set is undefined";
    return getExpr().upcastTo(set, target);
}

function join(set, child) {
    if (typeof set === "undefined") throw "join: set is undefined";
    if (typeof child === "undefined") throw "join: child is undefined";
    return getExpr().join(set, child);
}

function joinParent(set) {
    if (typeof set === "undefined") throw "joinParent: set is undefined";
    return getExpr().joinParent(set);
}

function joinRef(set) {
    if (typeof set === "undefined") throw "join: set is undefined";
    return getExpr().joinRef(set);
}

function none(set) {
    if (typeof set === "undefined") throw "none: set is undefined";
    return getExpr().none(set);
}

// Deprecate
function addConstraint(c, f) {
    c.addConstraint(new SetConstraint({apply:f}), new IntConstraint({apply:f}));
}

showStatistics();
hideSolutions();
solve(10);