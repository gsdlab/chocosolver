defaultScope(1);
intRange(-8, 7);

c1_A = Abstract("c1_A");
c2_b = c1_A.addChild("c2_b").withCard(0, 1);
c3_A1 = Clafer("c3_A1").withCard(1, 1).extending(c1_A);
c6_C = Clafer("c6_C").withCard(1, 1);
c7_b = c6_C.addChild("c7_b").withCard(0, 1);
c12_a = c6_C.addChild("c12_a").withCard(1, 1);
c15_d = c12_a.addChild("c15_d").withCard(1, 1);
c16_b = c15_d.addChild("c16_b").withCard(0, 1);
c12_a.refToUnique(c1_A);
c3_A1.addConstraint(none(join($this(), c2_b)));
c6_C.addConstraint(some(join(global(c6_C), c7_b)));
c12_a.addConstraint(some(join(join($this(), c15_d), c16_b)));
