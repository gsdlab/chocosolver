defaultScope(1);
intRange(-8, 7);

c1_D = Clafer("c1_D").withCard(1, 1);
c2_b = c1_D.addChild("c2_b").withCard(0, 1);
c3_c = c1_D.addChild("c3_c").withCard(0, 1);
c3_c.refTo(Int);
c1_D.addConstraint(equal(joinRef(join($this(), c3_c)), constant(5)));
c1_D.addConstraint(equal(joinRef(join($this(), c3_c)), constant(5)));
c1_D.addConstraint(some(join($this(), c2_b)));
