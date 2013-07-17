defaultScope(1);
intRange(-8, 7);

c1_A = Clafer("c1_A").withCard(1, 1);
c2_i = c1_A.addChild("c2_i").withCard(1, 1);
c2_i.refTo(Int);
c1_A.addConstraint(equal(joinRef(join($this(), c2_i)), constant(1)));
c1_A.addConstraint(equal(joinRef(join($this(), c2_i)), constant(1)));
c2_i.addConstraint(equal(joinRef($this()), constant(1)));
