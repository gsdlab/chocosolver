defaultScope(1);
intRange(-8, 7);

c1_A = Clafer("c1_A").withCard(1, 1);
c2_B = c1_A.addChild("c2_B").withCard(1, 1);
c3_C = c1_A.addChild("c3_C").withCard(0, 1);
c4_D = Clafer("c4_D").withCard(1, 1);
c4_D.refToUnique(c1_A);
