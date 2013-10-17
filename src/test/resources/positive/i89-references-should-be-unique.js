defaultScope(3);
intRange(-8, 7);

c1_A = Abstract("c1_A");
c2_a = Clafer("c2_a").withCard(1).extending(c1_A);
c3_setRefToA = Clafer("c3_setRefToA").withCard(3, 3);
c13_multisetRefToA = Clafer("c13_multisetRefToA").withCard(3, 3);
c3_setRefToA.refToUnique(c1_A);
c13_multisetRefToA.refTo(c1_A);
