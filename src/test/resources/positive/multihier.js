defaultScope(6);
intRange(-8, 7);

c1_A = Abstract("c1_A");
c3_B = Abstract("c3_B").extending(c1_A);
c2_Z = c1_A.addChild("c2_Z").withCard(1);
c4_C = Clafer("c4_C").withCard(1, 1).extending(c3_B);
c9_D = Clafer("c9_D").withCard(1, 1).extending(c1_A);
c4_C.addConstraint(greaterThanEqual(card(join($this(), c2_Z)), constant(4)));
c9_D.addConstraint(greaterThanEqual(card(join($this(), c2_Z)), constant(2)));
