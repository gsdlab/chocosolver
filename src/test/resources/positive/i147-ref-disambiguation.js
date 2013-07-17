defaultScope(1);
intRange(-8, 7);

c1_a = Clafer("c1_a").withCard(0, 1);
c2_b = c1_a.addChild("c2_b").withCard(0, 1);
c1_a.refToUnique(Int);
c2_b.addConstraint(equal(joinRef(joinParent($this())), constant(4)));
