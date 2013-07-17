defaultScope(1);
intRange(-8, 7);

c1_A = Abstract("c1_A");
c2_b = c1_A.addChild("c2_b").withCard(0, 1);
c3_C = Clafer("c3_C").withCard(1, 1);
c4_b = c3_C.addChild("c4_b").withCard(0, 1);
c5_a = c3_C.addChild("c5_a").withCard(1, 1).extending(c1_A);
c8_d = c5_a.addChild("c8_d").withCard(1, 1);
c9_b = c8_d.addChild("c9_b").withCard(0, 1);
c5_a.addConstraint(some(join(join($this(), c8_d), c9_b)));
