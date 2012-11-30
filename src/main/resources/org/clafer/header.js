importClass(Packages.choco.Choco);
importClass(Packages.choco.Options);
importClass(Packages.choco.kernel.model.variables.ConstantFactory);
importClass(Packages.org.clafer.Util);
importClass(java.util.Arrays);

function alert(msg) {
    throw new Error(msg);
}

function set (name, low, high) {
    if (typeof name === "undefined") alert("set: name is undefined");
    if (typeof low === "undefined") alert("set: low is undefined");
    if (typeof high === "undefined") alert("set: high is undefined");

    return Choco.makeSetVar(name, low, high, [Options.V_ENUM]);
}

function setArray (name, dimension, low, high) {
    if (typeof name === "undefined") alert("setArray: name is undefined");
    if (typeof dimension === "undefined") alert("setArray: dimension is undefined");
    if (typeof low === "undefined") alert("setArray: low is undefined");
    if (typeof high === "undefined") alert("setArray: high is undefined");

    return Choco.makeSetVarArray(name, dimension, low, high, [Options.V_ENUM]);
}

function intArray (name, dimension, low, high) {
    if (typeof name === "undefined") alert("intArray: name is undefined");
    if (typeof dimension === "undefined") alert("intArray: dimension is undefined");

    if (typeof low === "undefined" && typeof high === "undefined") {
        return Choco.makeIntVarArray(name, dimension, []);    
    }

    return Choco.makeIntVarArray(name, dimension, low, high, [Options.V_ENUM]);
}

function int (name, low, high) {
    if (typeof name === "undefined") alert("int: name is undefined");
    if (typeof low === "undefined") alert("int: low is undefined");
    if (typeof high === "undefined") alert("int: high is undefined");

    return Choco.makeIntVar(name, low, high, [Options.V_ENUM]);
}

function bool (name) {
    if (typeof name === "undefined") alert("bool: name is undefined");
    return Choco.makeBooleanVar(name, []);
}

var eq = Choco.eq;
function between (low, int, high) {
    if (low === high) {
        return eq(int, low);
    } else if (high === Number.POSITIVE_INFINITY) {
        return geq(int, low);
    }
    return and([geq(int, low), leq(int, high)]);
}
var neq = Choco.neq;
var and = Choco["and(choco.kernel.model.constraints.Constraint[])"];
var sum = Choco.sum;

var joinChildCount = 0;

// Optimized for parents with many children each.
function joinChild (expr, scope, child, childScope) {
    var childSets = []
    for (var i = 0; i < scope; i++) {
        joinChildCount++;
        var childSet = set("__joinChildInter" + joinChildCount, 0, childScope - 1);
        childSets.push(childSet);
        
        addConstraint(ifThenElse(member(i, expr), eq(childSet, child[i]), eq(childSet, emptySet)));
    }
    
    joinChildCount++;
    var s = set("__joinChild" + joinChildCount, 0, childScope - 1);

    addConstraint(setUnion(childSets, s));
    return s;
}

var joinParentCount = 0;

// Optimized for children with small scopes.
function joinParent (expr, scope, parent, parentScope) {
    var parentSets = []
    for (var i = 0; i < scope; i++) {
        joinParentCount++;
        var parentSet = set("__joinParentInter" + joinParentCount, 0, parentScope - 1);
        var par = int("__joinParentInterPar" + joinParentCount, 0, parentScope - 1);
        parentSets.push(parentSet);
        addConstraint(leqCard(parentSet, 1));
        addConstraint(ifThenElse(member(i, expr), and([nth(constantInt(i), parent, par), member(par, parentSet)]), eq(parentSet, emptySet)));
    }
    
    joinParentCount++;
    var s = set("__joinParent" + joinParentCount, 0, parentScope - 1);

    addConstraint(setUnion(parentSets, s));
    return s;
}

var joinRefCount = 0;

// Optimized for references with VERY large domains (ie. integers).
function joinRef (expr, scope, ref, refLow, refHigh) {
    var refSets = []
    for (var i = 0; i < scope; i++) {
        joinRefCount++;
        var refSet = set("__joinRefInter" + joinRefCount, refLow, refHigh);
        addConstraint(leqCard(refSet, 1));
        refSets.push(refSet);
        
        addConstraint(ifThenElse(member(i, expr), member(ref[i], refSet), eq(refSet, emptySet)));
    }
    
    joinRefCount++;
    var s = set("__joinRef" + joinRefCount, refLow, refHigh);

    addConstraint(setUnion(refSets, s));
    return s;
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
            return eq(sets[0], union);
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
    return and([implies(a, b), implies(not(a), c)]);
}
var ifOnlyIf = Choco.ifOnlyIf;
var member = Choco.member;
var notMember = Choco.notMember;
var nth = Choco.nth;
var geqCard = Choco.geqCard;
var leqCard = Choco.leqCard;
var eqCard = Choco.eqCard;
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
function addVariables(vs) {
    m.addVariables(vs);
}

function getVar(v) {
    return s.getVar(v);
}

var combine = Util.combine

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
            refdiff.push(implies(eq(parent[diff1], parent[diff2]), neq(ref[diff1], ref[diff2])));
        }
        if(refdiff.length > 0) {
            fulldiff.push(implies(neq(parent[diff1], parentScope), and(refdiff)));
        }
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

var constant = Choco.constant;
var constantInt = Choco["constant(int)"];

function range (low, high){
    var r = [];
    for (var i = 0; i <= high; i++) {
        r[i] = low + i;
    }
    return r;
}

var singletonCounter = 0;

// TODO: deprecate when optimized fix-size sets are implemented
function singleton(val) {
    singletonCounter++;
    var sv = set(val.getName() + "__singleton" + singletonCounter, -10000, 1000);
    addConstraint(eqCard(sv, 1));
    addConstraint(member(val, sv));
    return sv;
}

// http://stackoverflow.com/questions/1433382/how-to-convert-rhino-javascript-arrays-to-java-arrays
function convertArray(type, arr) {
    var jArr = java.lang.reflect.Array.newInstance(type, arr.length);
    for (var i = 0; i < arr.length; i++) {
        jArr[i] = arr[i];
    }
    return jArr;
};

var TRUE = Choco.TRUE;
var FALSE = Choco.FALSE;