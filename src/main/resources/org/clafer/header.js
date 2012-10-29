importClass(Packages.choco.Choco);
importClass(Packages.choco.Options);
importClass(Packages.org.clafer.Util);
importClass(java.util.Arrays);

function alert(msg) {
    throw new Error(msg);
}

function partition(set, partitions) {
    if (typeof set === "undefined") alert("partition: set is undefined");
    if (typeof partitions === "undefined") alert("partition: partitions is undefined");

    switch (partitions.length) {
        case 0:
            throw new IllegalArgumentException();
        case 1:
            addConstraint(eq(set, partitions[0]));
            break;
        default:
            addConstraint(setDisjoint(partitions));
            addConstraint(setUnion(partitions, set));
    }
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

function int (name, low, high) {
    if (typeof name === "undefined") alert("int: name is undefined");
    if (typeof low === "undefined") alert("int: low is undefined");
    if (typeof high === "undefined") alert("int: high is undefined");

    return Choco.makeIntVar(name, low, high, [Options.V_ENUM]);
}

var eq = Choco.eq;
function and(a1, a2) {
    return Choco["and(choco.kernel.model.constraints.Constraint[])"]([a1, a2]);
}
var setDisjoint = Choco.setDisjoint;
var setUnion = Choco.setUnion;
var implies = Choco.implies;
var member = Choco.member;
var notMember = Choco.notMember;
var geqCard = Choco.geqCard;
var leqCard = Choco.leqCard;
var eqCard = Choco.eqCard;

function addConstraint(c) {
    m.addConstraint(c);
}
function getVar(v) {
    return s.getVar(v);
}

var combine = Util.combine


// http://stackoverflow.com/questions/1433382/how-to-convert-rhino-javascript-arrays-to-java-arrays
function convertArray(type, arr) {
    var jArr = java.lang.reflect.Array.newInstance(type, arr.length);
    for (var i = 0; i < arr.length; i++) {
        jArr[i] = arr[i];
    }
    return jArr;
};