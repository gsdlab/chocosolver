defaultScope(1);
intRange(-8, 7);

c1_A = Clafer("c1_A").withCard(1, 1);
c2_x = c1_A.addChild("c2_x").withCard(0, 1);
c3_ref1 = Clafer("c3_ref1").withCard(1, 1);
c13_ref2 = Clafer("c13_ref2").withCard(1, 1);
c3_ref1.refToUnique(c1_A);
c13_ref2.refToUnique(c3_ref1);
c13_ref2.addConstraint(some(join(joinRef(joinRef($this())), c2_x)));
