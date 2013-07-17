defaultScope(1);
intRange(-8, 7);

c1_A = Abstract("c1_A");
c2_a = c1_A.addChild("c2_a").withCard(1, 1);
c3_A1 = Clafer("c3_A1").withCard(1, 1).extending(c1_A);
c2_a.refTo(Int);
c3_A1.addConstraint(equal(joinRef(join($this(), c2_a)), constant(10)));
