importClass(Packages.choco.Choco);
importClass(Packages.choco.Options);

function alert(msg) {
    throw new Error(msg);
}

function set (name, low, high, decision) {
    if (typeof name === "undefined") alert("set: name is undefined");
    if (typeof low === "undefined") alert("set: low is undefined");
    if (typeof high === "undefined") alert("set: high is undefined");

    if(decision) {
        return Choco.makeSetVar(name, low, high, [Options.V_ENUM, Options.V_NO_DECISION]);
    }
    return Choco.makeSetVar(name, low, high, [Options.V_ENUM]);
}

function setArray (name, dimension, low, high, options) {
    if (typeof name === "undefined") alert("intArray: name is undefined");
    if (typeof dimension === "undefined") alert("intArray: dimension is undefined");

    if (typeof low === "undefined") alert("intArray: low is undefined");
    if (typeof high === "undefined") alert("intArray: high is undefined");

    if (!options) options = {};

    if (options.lowCard && options.highCard) {
        var lowCard = options.lowCard;
        var highCard = options.highCard;
        var sets = [];
        for (var i = 0; i < dimension; i++) {
            sets.push(
                set(name + "#" + i,
                    Math.min(i * lowCard + low, high),
                    Math.min((i + 1) * highCard + low, high),
                    options
                ));
        }
        return sets;
    }

    return Choco.makeSetVarArray(name, dimension, low, high, [Options.V_ENUM]);
}

function intArray (name, dimension, low, high, options) {
    if (typeof name === "undefined") alert("intArray: name is undefined");
    if (typeof dimension === "undefined") alert("intArray: dimension is undefined");

    if (typeof low === "undefined") alert("intArray: low is undefined");
    if (typeof high === "undefined") alert("intArray: high is undefined");

    if (!options) options = {};
    
    if (!options.enum && (options.bound || high - low > 100)) {
        return Choco.makeIntVarArray(name, dimension, low, high, [Options.V_BOUND]);
    }
    return Choco.makeIntVarArray(name, dimension, low, high, [Options.V_ENUM]);
}

function int (name, low, high, options) {
    if (typeof name === "undefined") alert("int: name is undefined");
    if (typeof low === "undefined") alert("int: low is undefined");
    if (typeof high === "undefined") alert("int: high is undefined");

    if (!options) options = {};

    if (!options.enum && (options.bound || high - low > 100)) {
        return Choco.makeIntVar(name, low, high, [Options.V_BOUND]);
    }
    return Choco.makeIntVar(name, low, high, [Options.V_ENUM]);
}

function bool (name) {
    if (typeof name === "undefined") alert("bool: name is undefined");
    return Choco.makeBooleanVar(name, []);
}

var _eq = Choco.eq;
function eq (arg1, arg2) {
    if(typeof arg1 === "undefined") alert("eq: arg1 is undefined");
    if(typeof arg2 === "undefined") alert("eq: arg2 is undefined");
    var cond = arg1.condition.concat(arg2.condition);
    cond.push(_eq(arg1.variable, arg2.variable));
    return and(cond);
}
function eqFixed(arg1, arg2) {
    if (typeof arg1 === "undefined") alert("eqFixed: arg1 is undefined");
    if (typeof arg2 === "undefined") alert("eqFixed: arg2 is undefined");

    var eqs = arg1.condition.concat(arg2.condition);
    
    if(arg1.numvars === arg2.numvars) {
        for(var i = 0; i < arg1.numvars; i++) {
            eqs.push(Choco.eq(arg1.variables[i], arg2.variables[i]));
        }

        return and(eqs);
    }
    return FALSE;
}
function between (low, int, high) {
    if (low === high) {
        return Choco.eq(int, low);
    } else if (high === Number.POSITIVE_INFINITY) {
        return Choco.geq(int, low);
    }
    return and([Choco.geq(int, low), Choco.leq(int, high)]);
}
var neq = Choco.neq;
var and = Choco["and(choco.kernel.model.constraints.Constraint[])"];
var _and = Choco["and(choco.kernel.model.constraints.Constraint[])"];
var or = Choco["or(choco.kernel.model.constraints.Constraint[])"];
var plus = Choco.plus;
var minus = Choco.minus;
var sum = Choco.sum;

var joinChildCount = 0;

function cardVar(expr) {
    return constraint(expr.variable.getCard(), expr.condition);
}

// Optimized for parents with many children each.
function joinChild (expr, scope, child, childScope) {
    var cond = expr.condition.slice();
    var childSets = []
    for (var i = 0; i < scope; i++) {
        joinChildCount++;
        var childSet = set("__joinChildInter" + joinChildCount, 0, childScope - 1);
        childSets.push(childSet);
        
        cond.push(ifThenElse(Choco.member(i, expr.variable), Choco.eq(childSet, child[i]), Choco.eq(childSet, emptySet)));
    }
    
    joinChildCount++;
    var s = set("__joinChild" + joinChildCount, 0, childScope - 1);

    cond.push(setUnion(childSets, s));
    return constraint(s, cond);
}

var joinParentCount = 0;

// Optimized for children with small scopes.
function joinParent (expr, scope, parent, parentScope) {
    var cond = expr.condition.slice();
    var parentSets = []
    for (var i = 0; i < scope; i++) {
        joinParentCount++;
        var parentSet = set("__joinParentInter" + joinParentCount, 0, parentScope - 1);
        var par = int("__joinParentInterPar" + joinParentCount, 0, parentScope - 1);
        parentSets.push(parentSet);
        cond.push(leqCard(parentSet, 1));

        cond.push(implies(Choco.member(i, expr.variable), _and([Choco.nth(constantInt(i), parent, par), Choco.member(par, parentSet)])));
        cond.push(implies(notMember(i, expr.variable), Choco.eq(parentSet, emptySet)));
    }
    
    joinParentCount++;
    var s = set("__joinParent" + joinParentCount, 0, parentScope - 1);

    addConstraint(setUnion(parentSets, s));
    return constraint(s, cond);
}

var joinRefCount = 0;

function constraint(v, c) {
    return {variable : v, condition : c}
}

function fixedConstraint(v, c) {
    if (typeof v.length === "undefined") alert ("fixedConstraint: v.length is undefined");
    if (typeof c.length === "undefined") alert ("fixedConstraint: c.length is undefined");
    return {variables : v, condition : c, numvars : v.length}
}

function joinRefFixed (expr, scope, ref, refLow, refHigh) {
    joinRefCount++;
    var card = expr.numvars;
    var refd  = intArray("@__joinRefFixedRefd" + joinRefCount, card, refLow, refHigh);

    var n = expr.condition;
    for (var i = 0; i < card; i++) {
        n.push(nth(expr.variables[i], ref, refd[i]));
    }
    addConstraint(implies(notMember(__this, curClafer), _and(refd.map(function(x) { return _eq(x, 0);}))));

    return fixedConstraint(refd, n);
}

// Optimized for references with VERY large domains (ie. integers).
function joinRef (expr, scope, ref, refLow, refHigh) {
    if (typeof expr === "undefined") alert ("joinRef: expr is undefined");
    if (typeof scope === "undefined") alert ("joinRef: scope is undefined");
    if (typeof ref === "undefined") alert ("joinRef: ref is undefined");
    if (typeof refLow === "undefined") alert ("joinRef: refLow is undefined");
    if (typeof refHigh === "undefined") alert ("joinRef: refHigh is undefined");

    var refSets = []
    for (var i = 0; i < scope; i++) {
        joinRefCount++;
        var refSet = set("__joinRefInter" + joinRefCount, refLow, refHigh);
        addConstraint(leqCard(refSet, 1));
        refSets.push(refSet);
        
        addConstraint(ifThenElse(member(i, expr),
            Choco.member(ref[i], refSet),
            Choco.eq(refSet, emptySet)));
    }
    
    joinRefCount++;
    var s = set("__joinRef" + joinRefCount, refLow, refHigh);

    addConstraint(setUnion(refSets, s));
    return s;
}

function setToFixed(expr, low, high, card) {
    switch (card) {
        case 0: throw new IllegalArguementException();
        case 1:
            var iv = int(name("setToFixed"), low, high);
            addConstraint(implies(notMember(__this, curClafer), _eq(iv, 0)));
            return fixedConstraint([iv], [member(expr, iv)]);
        default:
            var ivs = intArray(name("setToFixed"), card, low, high);
            addConstraint(implies(notMember(__this, curClafer), _and(ivs.map(function(x) { return _eq(x, 0);}))));
            return fixedConstraint(ivs, [member(expr, ivs)]);
    }
}

var varCounter = 0;
function name(n) {
    varCounter++;
    return n + varCounter;
}

function enumerate(list, num) {
    if (num <= 0) alert("enumerate: num <= 0");
    if (num == 1) return list.map(function(i) { return [i]; });
    
    var enums = [];
    var recenums = enumerate(list, num - 1);
    for (var l in list) {
        for (var k in recenums) {
            enums.push(recenums[k].concat([list[l]]));
        }
    }
    return enums;
}

function enumerateDisj(list, num) {
    if (num <= 0) alert("enumerate: num <= 0");
    if (num > list.length) return [];
    if (num == 1) return list.map(function(i) { return [i]; });
    
    var enums = [];
    for (var l = 0; l < list.length; l++) {
        var recenums = enumerateDisj(list.slice(l + 1), num - 1);
        for (var k in recenums) {
            enums.push([list[l]].concat(recenums[k]));
        }
    }
    return enums;
}

// Helper function, do not call outside of this library.
function quantify(pred, quant, sizeLow, sizeHigh, low, high, bodyFun, disj, numVars) {
    var elems = intArray(name("allQuant"), sizeHigh, low, high);
    
    var tests = [];

    for(var i = sizeLow; i <= sizeHigh; i++) {
        // If i == 0 then the quantifier is satisfied.
        if (i > 0) {
            var subelems = elems.slice(0, i);

            var enums;
            if (disj) {
                enums = enumerateDisj(subelems, numVars);
            } else {
                enums = enumerate(subelems, numVars);
            }

            if(enums.length > 0) {
                tests.push(Choco.implies(eqCard(quant, i),
                        and(
                            [allDifferents(subelems), members(quant, subelems), pred(enums.map(bodyFun))])
                        ));
            }
        }
    }
    return and(tests);
}

function allQuant(quant, sizeLow, sizeHigh, low, high, bodyFun, disj, numVars) {
    return quantify(and, quant, sizeLow, sizeHigh, low, high, bodyFun, disj, numVars);
}

function someQuant(quant, sizeLow, sizeHigh, low, high, bodyFun, disj, numVars) {
    return quantify(or, quant, sizeLow, sizeHigh, low, high, bodyFun, disj, numVars);
}

var emptySet = Choco.emptySet();

var setDisjoint = Choco.setDisjoint;

function setUnion (sets, union) {
    if (typeof sets === "undefined") alert("setUnion: sets is undefined");
    if (typeof union === "undefined") alert("setUnion: union is undefined");
    switch (sets.length) {
        case 0:
            throw new IllegalArgumentException();
        case 1:
            return Choco.eq(sets[0], union);
        default:
            return Choco.setUnion(sets, union);
    }
}

var unionCount = 0;

function union (sets, low, high) {
    switch (sets.length) {
        case 0:
            throw new IllegalArgumentException();
        case 1:
            return sets[0];
        default:
            unionCount++;
            unionSet = set("union" + unionCount, low, high);
            addConstraint(Choco.setUnion(sets, unionSet));
            return unionSet;
    }
}

var inverseSet = Choco.inverseSet;
var not = Choco.not;
var implies = Choco.implies;
function ifThenElse(a, b, c) {
    // The standard Choco.ifThenElse seems to be broken :(
    return and([Choco.implies(a, b), Choco.implies(not(a), c)]);
}
var ifOnlyIf = Choco.ifOnlyIf;
// Due to bug, this less than optimal function is needed :(
function members(s, is) {
    return and(is.map(function(i) {return member(s, i);}));
}
var member = Choco.member;
var notMember = Choco.notMember;
var nth = Choco.nth;
var geqCard = Choco.geqCard;
var leqCard = Choco.leqCard;
var eqCard = Choco.eqCard;

function seqCard (arg, v) {
    var cond = arg.condition.slice();
    cond.push(eqCard(arg.variable, v));
    return and(cond);
}
function sgeqCard (arg, v) {
    var cond = arg.condition.slice();
    cond.push(geqCard(arg.variable, v));
    return and(cond);
}
function betweenCard (low, set, high) {
    if (low === high) {
        return eqCard(set, low);
    } else if (high === Number.POSITIVE_INFINITY) {
        return geqCard(set, low);
    } else if (low === 0) {
        return leqCard(set, high);
    }
    return and([geqCard(set, low), leqCard(set, high)]);
}
// Due to bug
function allDifferents(is) {
    var tests = []
    for(var i = 0; i < is.length; i++) {
        for (var j = i + 1; j < is.length; j++) {
            tests.push(neq(is[i], is[j]));
        }
    }
    return and(tests);
}
var allDifferent = Choco.allDifferent
var inverseSet = Choco.inverseSet
var increasingNValue = Choco.increasingNValue
var reifiedConstraint = Choco.reifiedConstraint

function addConstraint(c) {
    if(typeof c === "undefined") alert("addConstraint: constraint is undefined");
    m.addConstraint(c);
}
function addVariable(v) {
    m.addVariable(v);
}
function addObjectiveVariable(v) {
    m.addVariable([Options.V_OBJECTIVE], v);
}
function addVariables(vs) {
    m.addVariables(vs);
}

function getVar(v) {
    return s.getVar(v);
}

var reify__counter = 0;

function reifyConstraint(constraint) {
    reify__counter++;
    var reify = bool("__reifiy__" + reify__counter);
    addConstraint(reifiedConstraint(reify, constraint));
    return reify;
}

function uniqueRef(parent, parentScope, ref) {
    var scope = ref.length;
    var fulldiff = [];
    for(var diff1 = 0; diff1 < scope; diff1++) {
        var refdiff = [];
        for(var diff2 = diff1 + 1; diff2 < scope; diff2++) {
            var test = and([Choco.eq(parent[diff1], __this), Choco.eq(parent[diff2], __this)])
            refdiff.push(Choco.implies(test, Choco.neq(ref[diff1], ref[diff2])));
        }
        if(refdiff.length > 0) {
            fulldiff = fulldiff.concat(refdiff);
        }
    }
    if(fulldiff.length == 1) {
        return fulldiff[0];
    }
    return and(fulldiff);
}

function trace(obj) {
    println("trace:" + obj);
    return obj;
}

var offsetCount = 0;

// {s | q in base & s = q + off}
function offset(base, off) {
    if(off == 0) {
        return base;
    }
    offsetCount++;
    var lowb = base.getLowB();
    var uppb = base.getUppB();
    var s = set("@offset" + offsetCount, lowb + off, uppb + off);
    for(var i = lowb; i <= uppb; i++) {
        addConstraint(ifOnlyIf(member(i, base), member(i + off, s)));
    }
    return s;
}

function constant(i) {
    return constraint(Choco.constant(i), []);
}
var constantInt = Choco["constant(int)"];
function constantIntFixed(i) {
    return fixedConstraint([constantInt(i)], []);
}
var among = Choco.among;
var increasingNValue = Choco.increasingNValue;

var sumCount = 0;

function sumSet(s, low, high) {
    if (typeof s === "undefined") alert("sumSet: set is undefined");
    if (typeof low === "undefined") alert("sumSet: low is undefined");
    if (typeof high === "undefined") alert("sumSet: high is undefined");

    var lowB = s.getLowB();
    var uppB = s.getUppB();
    var lowSumB = low == 0 ? lowB : (low / 2) * (lowB + lowB + low - 1);
    var uppSumB = (high / 2) * (uppB - high + 1  + uppB);
    
    sumCount++;
    var result = int("sumSet" + sumCount, lowSumB, uppSumB);
    var elems  = intArray("sumSetElems" + sumCount, high, s.getLowB(), s.getUppB());

    var imp = Choco.implies;
    if (low == high) {
        imp = constf;
    }

    if (high >= 2) {
        addConstraint(allDifferent(elems));
    }
    for (var i = low; i <= high; i++) {
        if (i == 0) {
            addConstraint(imp(eqCard(s, 0), eq(result, 0)));
        } else if(i == 1) {
            addConstraint(imp(eqCard(s, 1), member(result, s)));
        } else {
            var subelems = elems.slice(0, i);
            addConstraint(imp(eqCard(s, i), and([eq(result, sum(subelems))].concat(allMembers(subelems, s)))));
        }
    }
    return result;
}

function constf(x, y) {
    return y;
}

function allMembers(ivs, sv) {
    memb = [];
    for (var i in ivs) {
        memb.push(member(ivs[i], sv));
    }
    return memb;
}

function range (low, high){
    var r = [];
    for (var i = 0; i <= high; i++) {
        r[i] = low + i;
    }
    return r;
}

var singletonCounter = 0;

function singleton(expr) {
    var val = expr.variable;
    var cond = expr.condition.slice();

    singletonCounter++;
    var sv = set("__singleton__" + val.getName() + "_" + singletonCounter, val.getLowB(), val.getUppB(), true);
    
    addConstraint(Choco.eqCard(sv, 1));
    addConstraint(Choco.member(val, sv));
    return constraint(sv, cond);
}

function singletonExpr(expr) {
    // TODO fix when bug patched.
    singletonCounter++;
    var singleSv = Choco.makeSetVar("__singletonExpr__" + expr.getName() + "_" + singletonCounter, expr.getLowB(), expr.getUppB(), [Options.V_ENUM]);
    var singleIv = int("__singletonExpri__" + expr.getName() + "_" + singletonCounter, expr.getLowB(), expr.getUppB());

    addConstraint(eq(singleIv, expr));
    addConstraint(eqCard(singleSv, 1));
    addConstraint(member(singleIv, singleSv));
    return singleSv;
}

var TRUE = Choco.TRUE;
var FALSE = Choco.FALSE;