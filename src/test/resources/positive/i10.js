defaultScope(1);
intRange(-8, 7);

c1_A = Clafer("c1_A").withCard(1, 1);
c2_x = c1_A.addChild("c2_x").withCard(1, 1);
c2_x.refTo(Int);
c2_x.addConstraint(greaterThanEqual(joinRef($this()), constant(0)));
