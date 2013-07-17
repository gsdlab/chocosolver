defaultScope(1);
intRange(-8, 7);

c1_A = Clafer("c1_A").withCard(1, 1);
c2_b = c1_A.addChild("c2_b").withCard(0, 1);
c2_b.refTo(c1_A);
c1_A.addConstraint(some($this()));
c2_b.addConstraint(some(joinParent($this())));
