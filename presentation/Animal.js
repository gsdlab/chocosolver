// Scopes
var scope__root = 1;
var scope__low__root = 0;
var scope__high__root = scope__root - 1;
var scope__c2_Head = 9;
var scope__low__c2_Head = 0;
var scope__high__c2_Head = scope__c2_Head - 1;
var scope__c3_Eye = 18;
var scope__low__c3_Eye = 0;
var scope__high__c3_Eye = scope__c3_Eye - 1;
var scope__c4_Ear = 18;
var scope__low__c4_Ear = 0;
var scope__high__c4_Ear = scope__c4_Ear - 1;
var scope__c5_Mouth = 9;
var scope__low__c5_Mouth = 0;
var scope__high__c5_Mouth = scope__c5_Mouth - 1;
var scope__c6_Age = 9;
var scope__low__c6_Age = 0;
var scope__high__c6_Age = scope__c6_Age - 1;
var scope__c16_Torso = 9;
var scope__low__c16_Torso = 0;
var scope__high__c16_Torso = scope__c16_Torso - 1;
var scope__c17_Leg = 36;
var scope__low__c17_Leg = 0;
var scope__high__c17_Leg = scope__c17_Leg - 1;
var scope__c18_Feet = 36;
var scope__low__c18_Feet = 0;
var scope__high__c18_Feet = scope__c18_Feet - 1;
var scope__c19_Cat = 4;
var scope__low__c19_Cat = 0;
var scope__high__c19_Cat = scope__c19_Cat - 1;
var scope__c20_Whiskers = 24;
var scope__low__c20_Whiskers = 0;
var scope__high__c20_Whiskers = scope__c20_Whiskers - 1;
var scope__c21_Rhino = 3;
var scope__low__c21_Rhino = 0;
var scope__high__c21_Rhino = scope__c21_Rhino - 1;
var scope__c22_Horn = 3;
var scope__low__c22_Horn = 0;
var scope__high__c22_Horn = scope__c22_Horn - 1;
var scope__c23_Elephant = 2;
var scope__low__c23_Elephant = 0;
var scope__high__c23_Elephant = scope__c23_Elephant - 1;
var scope__c24_Trunk = 2;
var scope__low__c24_Trunk = 0;
var scope__high__c24_Trunk = scope__c24_Trunk - 1;
var scope__c1_Animal = scope__c19_Cat + scope__c21_Rhino + scope__c23_Elephant;
var scope__low__c1_Animal = 0;
var scope__high__c1_Animal = scope__c1_Animal - 1;
var scope__low__integer = -2000;
var scope__high__integer = 2000;

// Offsets
var offset__c19_Cat = 0;
var offset__c21_Rhino = 4;
var offset__c23_Elephant = 7;

// Clafer - root/c1_Animal
c1_Animal = set("c1_Animal", 0, scope__high__c1_Animal);

// Clafer - root/c19_Cat
c19_Cat = set("c19_Cat", 0, scope__high__c19_Cat);
__c19_Cat = setArray("__c19_Cat", scope__root, 0, scope__high__c19_Cat);
addConstraint(setUnion(__c19_Cat, c19_Cat));
c19_Cat__parent = intArray("c19_Cat__parent", scope__c19_Cat, 0, scope__root, {lowCard: 4, highCard: 4});
addConstraint(inverseSet(c19_Cat__parent, __c19_Cat.concat(setArray("__c19_Cat__unused", 1, 0,                  scope__high__c19_Cat))));
addConstraint(increasingNValue(int("c19_Cat__num", 0, scope__c19_Cat), c19_Cat__parent));

// Clafer - root/c21_Rhino
c21_Rhino = set("c21_Rhino", 0, scope__high__c21_Rhino);
__c21_Rhino = setArray("__c21_Rhino", scope__root, 0, scope__high__c21_Rhino);
addConstraint(setUnion(__c21_Rhino, c21_Rhino));
c21_Rhino__parent = intArray("c21_Rhino__parent", scope__c21_Rhino, 0, scope__root, {lowCard: 3, highCard: 3});
addConstraint(inverseSet(c21_Rhino__parent, __c21_Rhino.concat(setArray("__c21_Rhino__unused", 1, 0,                  scope__high__c21_Rhino))));
addConstraint(increasingNValue(int("c21_Rhino__num", 0, scope__c21_Rhino), c21_Rhino__parent));

// Clafer - root/c23_Elephant
c23_Elephant = set("c23_Elephant", 0, scope__high__c23_Elephant);
__c23_Elephant = setArray("__c23_Elephant", scope__root, 0, scope__high__c23_Elephant);
addConstraint(setUnion(__c23_Elephant, c23_Elephant));
c23_Elephant__parent = intArray("c23_Elephant__parent", scope__c23_Elephant, 0, scope__root, {lowCard: 2, highCard: 2});
addConstraint(inverseSet(c23_Elephant__parent, __c23_Elephant.concat(setArray("__c23_Elephant__unused", 1, 0,                  scope__high__c23_Elephant))));
addConstraint(increasingNValue(int("c23_Elephant__num", 0, scope__c23_Elephant), c23_Elephant__parent));

// Clafer - c1_Animal/c2_Head
c2_Head = set("c2_Head", 0, scope__high__c2_Head);
__c2_Head = setArray("__c2_Head", scope__c1_Animal, 0, scope__high__c2_Head);
addConstraint(setUnion(__c2_Head, c2_Head));
c2_Head__parent = intArray("c2_Head__parent", scope__c2_Head, 0, scope__c1_Animal, {lowCard: 1, highCard: 1});
addConstraint(inverseSet(c2_Head__parent, __c2_Head.concat(setArray("__c2_Head__unused", 1, 0,                  scope__high__c2_Head))));
addConstraint(increasingNValue(int("c2_Head__num", 0, scope__c2_Head), c2_Head__parent));

// Clafer - c1_Animal/c6_Age
c6_Age = set("c6_Age", 0, scope__high__c6_Age);
__c6_Age = setArray("__c6_Age", scope__c1_Animal, 0, scope__high__c6_Age);
addConstraint(setUnion(__c6_Age, c6_Age));
c6_Age__parent = intArray("c6_Age__parent", scope__c6_Age, 0, scope__c1_Animal, {lowCard: 1, highCard: 1});
addConstraint(inverseSet(c6_Age__parent, __c6_Age.concat(setArray("__c6_Age__unused", 1, 0,                  scope__high__c6_Age))));
addConstraint(increasingNValue(int("c6_Age__num", 0, scope__c6_Age), c6_Age__parent));

// Clafer - c1_Animal/c16_Torso
c16_Torso = set("c16_Torso", 0, scope__high__c16_Torso);
__c16_Torso = setArray("__c16_Torso", scope__c1_Animal, 0, scope__high__c16_Torso);
addConstraint(setUnion(__c16_Torso, c16_Torso));
c16_Torso__parent = intArray("c16_Torso__parent", scope__c16_Torso, 0, scope__c1_Animal, {lowCard: 1, highCard: 1});
addConstraint(inverseSet(c16_Torso__parent, __c16_Torso.concat(setArray("__c16_Torso__unused", 1, 0,                  scope__high__c16_Torso))));
addConstraint(increasingNValue(int("c16_Torso__num", 0, scope__c16_Torso), c16_Torso__parent));

// Clafer - c1_Animal/c17_Leg
c17_Leg = set("c17_Leg", 0, scope__high__c17_Leg);
__c17_Leg = setArray("__c17_Leg", scope__c1_Animal, 0, scope__high__c17_Leg);
addConstraint(setUnion(__c17_Leg, c17_Leg));
c17_Leg__parent = intArray("c17_Leg__parent", scope__c17_Leg, 0, scope__c1_Animal, {lowCard: 4, highCard: 4});
addConstraint(inverseSet(c17_Leg__parent, __c17_Leg.concat(setArray("__c17_Leg__unused", 1, 0,                  scope__high__c17_Leg))));
addConstraint(increasingNValue(int("c17_Leg__num", 0, scope__c17_Leg), c17_Leg__parent));

// Clafer - c2_Head/c3_Eye
c3_Eye = set("c3_Eye", 0, scope__high__c3_Eye);
__c3_Eye = setArray("__c3_Eye", scope__c2_Head, 0, scope__high__c3_Eye);
addConstraint(setUnion(__c3_Eye, c3_Eye));
c3_Eye__parent = intArray("c3_Eye__parent", scope__c3_Eye, 0, scope__c2_Head, {lowCard: 2, highCard: 2});
addConstraint(inverseSet(c3_Eye__parent, __c3_Eye.concat(setArray("__c3_Eye__unused", 1, 0,                  scope__high__c3_Eye))));
addConstraint(increasingNValue(int("c3_Eye__num", 0, scope__c3_Eye), c3_Eye__parent));

// Clafer - c2_Head/c4_Ear
c4_Ear = set("c4_Ear", 0, scope__high__c4_Ear);
__c4_Ear = setArray("__c4_Ear", scope__c2_Head, 0, scope__high__c4_Ear);
addConstraint(setUnion(__c4_Ear, c4_Ear));
c4_Ear__parent = intArray("c4_Ear__parent", scope__c4_Ear, 0, scope__c2_Head, {lowCard: 2, highCard: 2});
addConstraint(inverseSet(c4_Ear__parent, __c4_Ear.concat(setArray("__c4_Ear__unused", 1, 0,                  scope__high__c4_Ear))));
addConstraint(increasingNValue(int("c4_Ear__num", 0, scope__c4_Ear), c4_Ear__parent));

// Clafer - c2_Head/c5_Mouth
c5_Mouth = set("c5_Mouth", 0, scope__high__c5_Mouth);
__c5_Mouth = setArray("__c5_Mouth", scope__c2_Head, 0, scope__high__c5_Mouth);
addConstraint(setUnion(__c5_Mouth, c5_Mouth));
c5_Mouth__parent = intArray("c5_Mouth__parent", scope__c5_Mouth, 0, scope__c2_Head, {lowCard: 1, highCard: 1});
addConstraint(inverseSet(c5_Mouth__parent, __c5_Mouth.concat(setArray("__c5_Mouth__unused", 1, 0,                  scope__high__c5_Mouth))));
addConstraint(increasingNValue(int("c5_Mouth__num", 0, scope__c5_Mouth), c5_Mouth__parent));

// Clafer - c17_Leg/c18_Feet
c18_Feet = set("c18_Feet", 0, scope__high__c18_Feet);
__c18_Feet = setArray("__c18_Feet", scope__c17_Leg, 0, scope__high__c18_Feet);
addConstraint(setUnion(__c18_Feet, c18_Feet));
c18_Feet__parent = intArray("c18_Feet__parent", scope__c18_Feet, 0, scope__c17_Leg, {lowCard: 1, highCard: 1});
addConstraint(inverseSet(c18_Feet__parent, __c18_Feet.concat(setArray("__c18_Feet__unused", 1, 0,                  scope__high__c18_Feet))));
addConstraint(increasingNValue(int("c18_Feet__num", 0, scope__c18_Feet), c18_Feet__parent));

// Clafer - c19_Cat/c20_Whiskers
c20_Whiskers = set("c20_Whiskers", 0, scope__high__c20_Whiskers);
__c20_Whiskers = setArray("__c20_Whiskers", scope__c19_Cat, 0, scope__high__c20_Whiskers);
addConstraint(setUnion(__c20_Whiskers, c20_Whiskers));
c20_Whiskers__parent = intArray("c20_Whiskers__parent", scope__c20_Whiskers, 0, scope__c19_Cat, {lowCard: 6, highCard: 6});
addConstraint(inverseSet(c20_Whiskers__parent, __c20_Whiskers.concat(setArray("__c20_Whiskers__unused", 1, 0,                  scope__high__c20_Whiskers))));
addConstraint(increasingNValue(int("c20_Whiskers__num", 0, scope__c20_Whiskers), c20_Whiskers__parent));

// Clafer - c21_Rhino/c22_Horn
c22_Horn = set("c22_Horn", 0, scope__high__c22_Horn);
__c22_Horn = setArray("__c22_Horn", scope__c21_Rhino, 0, scope__high__c22_Horn);
addConstraint(setUnion(__c22_Horn, c22_Horn));
c22_Horn__parent = intArray("c22_Horn__parent", scope__c22_Horn, 0, scope__c21_Rhino, {lowCard: 1, highCard: 1});
addConstraint(inverseSet(c22_Horn__parent, __c22_Horn.concat(setArray("__c22_Horn__unused", 1, 0,                  scope__high__c22_Horn))));
addConstraint(increasingNValue(int("c22_Horn__num", 0, scope__c22_Horn), c22_Horn__parent));

// Clafer - c23_Elephant/c24_Trunk
c24_Trunk = set("c24_Trunk", 0, scope__high__c24_Trunk);
__c24_Trunk = setArray("__c24_Trunk", scope__c23_Elephant, 0, scope__high__c24_Trunk);
addConstraint(setUnion(__c24_Trunk, c24_Trunk));
c24_Trunk__parent = intArray("c24_Trunk__parent", scope__c24_Trunk, 0, scope__c23_Elephant, {lowCard: 1, highCard: 1});
addConstraint(inverseSet(c24_Trunk__parent, __c24_Trunk.concat(setArray("__c24_Trunk__unused", 1, 0,                  scope__high__c24_Trunk))));
addConstraint(increasingNValue(int("c24_Trunk__num", 0, scope__c24_Trunk), c24_Trunk__parent));

// Abstract - c1_Animal/c19_Cat
for (var i = 0; i < scope__c19_Cat; i++) {
    addConstraint(ifOnlyIf(member(i, c19_Cat), member(i + offset__c19_Cat, c1_Animal)));
}

// Abstract - c1_Animal/c21_Rhino
for (var i = 0; i < scope__c21_Rhino; i++) {
    addConstraint(ifOnlyIf(member(i, c21_Rhino), member(i + offset__c21_Rhino, c1_Animal)));
}

// Abstract - c1_Animal/c23_Elephant
for (var i = 0; i < scope__c23_Elephant; i++) {
    addConstraint(ifOnlyIf(member(i, c23_Elephant), member(i + offset__c23_Elephant, c1_Animal)));
}

// Child - root/c19_Cat (4,4)
for (var i = 0; i < __c19_Cat.length; i++) {
    addConstraint(implies(member(i, root), eqCard(__c19_Cat[i], 4)));
    addConstraint(implies(notMember(i, root), eqCard(__c19_Cat[i], 0)));
}

// Child - root/c21_Rhino (3,3)
for (var i = 0; i < __c21_Rhino.length; i++) {
    addConstraint(implies(member(i, root), eqCard(__c21_Rhino[i], 3)));
    addConstraint(implies(notMember(i, root), eqCard(__c21_Rhino[i], 0)));
}

// Child - root/c23_Elephant (2,2)
for (var i = 0; i < __c23_Elephant.length; i++) {
    addConstraint(implies(member(i, root), eqCard(__c23_Elephant[i], 2)));
    addConstraint(implies(notMember(i, root), eqCard(__c23_Elephant[i], 0)));
}

// Child - c1_Animal/c2_Head (1,1)
for (var i = 0; i < __c2_Head.length; i++) {
    addConstraint(implies(member(i, c1_Animal), eqCard(__c2_Head[i], 1)));
    addConstraint(implies(notMember(i, c1_Animal), eqCard(__c2_Head[i], 0)));
}

// Child - c1_Animal/c6_Age (1,1)
for (var i = 0; i < __c6_Age.length; i++) {
    addConstraint(implies(member(i, c1_Animal), eqCard(__c6_Age[i], 1)));
    addConstraint(implies(notMember(i, c1_Animal), eqCard(__c6_Age[i], 0)));
}

// Child - c1_Animal/c16_Torso (1,1)
for (var i = 0; i < __c16_Torso.length; i++) {
    addConstraint(implies(member(i, c1_Animal), eqCard(__c16_Torso[i], 1)));
    addConstraint(implies(notMember(i, c1_Animal), eqCard(__c16_Torso[i], 0)));
}

// Child - c1_Animal/c17_Leg (4,4)
for (var i = 0; i < __c17_Leg.length; i++) {
    addConstraint(implies(member(i, c1_Animal), eqCard(__c17_Leg[i], 4)));
    addConstraint(implies(notMember(i, c1_Animal), eqCard(__c17_Leg[i], 0)));
}

// Child - c2_Head/c3_Eye (2,2)
for (var i = 0; i < __c3_Eye.length; i++) {
    addConstraint(implies(member(i, c2_Head), eqCard(__c3_Eye[i], 2)));
    addConstraint(implies(notMember(i, c2_Head), eqCard(__c3_Eye[i], 0)));
}

// Child - c2_Head/c4_Ear (2,2)
for (var i = 0; i < __c4_Ear.length; i++) {
    addConstraint(implies(member(i, c2_Head), eqCard(__c4_Ear[i], 2)));
    addConstraint(implies(notMember(i, c2_Head), eqCard(__c4_Ear[i], 0)));
}

// Child - c2_Head/c5_Mouth (1,1)
for (var i = 0; i < __c5_Mouth.length; i++) {
    addConstraint(implies(member(i, c2_Head), eqCard(__c5_Mouth[i], 1)));
    addConstraint(implies(notMember(i, c2_Head), eqCard(__c5_Mouth[i], 0)));
}

// Child - c17_Leg/c18_Feet (1,1)
for (var i = 0; i < __c18_Feet.length; i++) {
    addConstraint(implies(member(i, c17_Leg), eqCard(__c18_Feet[i], 1)));
    addConstraint(implies(notMember(i, c17_Leg), eqCard(__c18_Feet[i], 0)));
}

// Child - c19_Cat/c20_Whiskers (6,6)
for (var i = 0; i < __c20_Whiskers.length; i++) {
    addConstraint(implies(member(i, c19_Cat), eqCard(__c20_Whiskers[i], 6)));
    addConstraint(implies(notMember(i, c19_Cat), eqCard(__c20_Whiskers[i], 0)));
}

// Child - c21_Rhino/c22_Horn (1,1)
for (var i = 0; i < __c22_Horn.length; i++) {
    addConstraint(implies(member(i, c21_Rhino), eqCard(__c22_Horn[i], 1)));
    addConstraint(implies(notMember(i, c21_Rhino), eqCard(__c22_Horn[i], 0)));
}

// Child - c23_Elephant/c24_Trunk (1,1)
for (var i = 0; i < __c24_Trunk.length; i++) {
    addConstraint(implies(member(i, c23_Elephant), eqCard(__c24_Trunk[i], 1)));
    addConstraint(implies(notMember(i, c23_Elephant), eqCard(__c24_Trunk[i], 0)));
}

c6_Age__ref = intArray("c6_Age__ref", scope__c6_Age, scope__low__integer, scope__high__integer);
addVariable(c6_Age__ref);
curClafer = root;
for (var __this = 0; __this < scope__root; __this++) {
}
curClafer = c1_Animal;
for (var __this = 0; __this < scope__c1_Animal; __this++) {
    addConstraint(implies(member(__this, c1_Animal), uniqueRef(c6_Age__parent, scope__c1_Animal, c6_Age__ref)));
}
curClafer = c2_Head;
for (var __this = 0; __this < scope__c2_Head; __this++) {
}
curClafer = c3_Eye;
for (var __this = 0; __this < scope__c3_Eye; __this++) {
}
curClafer = c4_Ear;
for (var __this = 0; __this < scope__c4_Ear; __this++) {
}
curClafer = c5_Mouth;
for (var __this = 0; __this < scope__c5_Mouth; __this++) {
}
curClafer = c6_Age;
for (var __this = 0; __this < scope__c6_Age; __this++) {
    // Break symmetry for references that don't appear in the model.
    addConstraint(implies(notMember(__this, c6_Age), _eq(c6_Age__ref[__this], 0)));
}
curClafer = c16_Torso;
for (var __this = 0; __this < scope__c16_Torso; __this++) {
}
curClafer = c17_Leg;
for (var __this = 0; __this < scope__c17_Leg; __this++) {
}
curClafer = c18_Feet;
for (var __this = 0; __this < scope__c18_Feet; __this++) {
}
curClafer = c19_Cat;
for (var __this = 0; __this < scope__c19_Cat; __this++) {
}
curClafer = c20_Whiskers;
for (var __this = 0; __this < scope__c20_Whiskers; __this++) {
}
curClafer = c21_Rhino;
for (var __this = 0; __this < scope__c21_Rhino; __this++) {
}
curClafer = c22_Horn;
for (var __this = 0; __this < scope__c22_Horn; __this++) {
}
curClafer = c23_Elephant;
for (var __this = 0; __this < scope__c23_Elephant; __this++) {
}
curClafer = c24_Trunk;
for (var __this = 0; __this < scope__c24_Trunk; __this++) {
}
function solution() {
    solution__root(0, "");
}

function solution__root(parent, indent) {
    var i = getVar(__root[parent]).getValue();
    for (var j in i) {
        var k = i[j];
        println(indent + "root" + k);
        solution__c19_Cat(k, "    " + indent);
        solution__c21_Rhino(k, "    " + indent);
        solution__c23_Elephant(k, "    " + indent);
    }
}

function solution__c1_Animal(k, indent) {
        solution__c2_Head(k, "    " + indent);
        solution__c6_Age(k, "    " + indent);
        solution__c16_Torso(k, "    " + indent);
        solution__c17_Leg(k, "    " + indent);
}

function solution__c2_Head(parent, indent) {
    var i = getVar(__c2_Head[parent]).getValue();
    for (var j in i) {
        var k = i[j];
        println(indent + "Head" + k);
        solution__c3_Eye(k, "    " + indent);
        solution__c4_Ear(k, "    " + indent);
        solution__c5_Mouth(k, "    " + indent);
    }
}

function solution__c3_Eye(parent, indent) {
    var i = getVar(__c3_Eye[parent]).getValue();
    for (var j in i) {
        var k = i[j];
        println(indent + "Eye" + k);
    }
}

function solution__c4_Ear(parent, indent) {
    var i = getVar(__c4_Ear[parent]).getValue();
    for (var j in i) {
        var k = i[j];
        println(indent + "Ear" + k);
    }
}

function solution__c5_Mouth(parent, indent) {
    var i = getVar(__c5_Mouth[parent]).getValue();
    for (var j in i) {
        var k = i[j];
        println(indent + "Mouth" + k);
    }
}

function solution__c6_Age(parent, indent) {
    var i = getVar(__c6_Age[parent]).getValue();
    for (var j in i) {
        var k = i[j];
        println(indent + "Age" + k);
        println("    " + indent + "ref = " + getVar(c6_Age__ref[k]).getValue());
    }
}

function solution__c16_Torso(parent, indent) {
    var i = getVar(__c16_Torso[parent]).getValue();
    for (var j in i) {
        var k = i[j];
        println(indent + "Torso" + k);
    }
}

function solution__c17_Leg(parent, indent) {
    var i = getVar(__c17_Leg[parent]).getValue();
    for (var j in i) {
        var k = i[j];
        println(indent + "Leg" + k);
        solution__c18_Feet(k, "    " + indent);
    }
}

function solution__c18_Feet(parent, indent) {
    var i = getVar(__c18_Feet[parent]).getValue();
    for (var j in i) {
        var k = i[j];
        println(indent + "Feet" + k);
    }
}

function solution__c19_Cat(parent, indent) {
    var i = getVar(__c19_Cat[parent]).getValue();
    for (var j in i) {
        var k = i[j];
        println(indent + "Cat" + k);
        solution__c1_Animal(k + offset__c19_Cat, indent);
        solution__c20_Whiskers(k, "    " + indent);
    }
}

function solution__c20_Whiskers(parent, indent) {
    var i = getVar(__c20_Whiskers[parent]).getValue();
    for (var j in i) {
        var k = i[j];
        println(indent + "Whiskers" + k);
    }
}

function solution__c21_Rhino(parent, indent) {
    var i = getVar(__c21_Rhino[parent]).getValue();
    for (var j in i) {
        var k = i[j];
        println(indent + "Rhino" + k);
        solution__c1_Animal(k + offset__c21_Rhino, indent);
        solution__c22_Horn(k, "    " + indent);
    }
}

function solution__c22_Horn(parent, indent) {
    var i = getVar(__c22_Horn[parent]).getValue();
    for (var j in i) {
        var k = i[j];
        println(indent + "Horn" + k);
    }
}

function solution__c23_Elephant(parent, indent) {
    var i = getVar(__c23_Elephant[parent]).getValue();
    for (var j in i) {
        var k = i[j];
        println(indent + "Elephant" + k);
        solution__c1_Animal(k + offset__c23_Elephant, indent);
        solution__c24_Trunk(k, "    " + indent);
    }
}

function solution__c24_Trunk(parent, indent) {
    var i = getVar(__c24_Trunk[parent]).getValue();
    for (var j in i) {
        var k = i[j];
        println(indent + "Trunk" + k);
    }
}

