defaultScope(1);
intRange(-8, 7);

c1_A = Clafer("c1_A").withCard(1, 1);
c2_x = c1_A.addChild("c2_x").withCard(0, 1);
c3_y = c1_A.addChild("c3_y").withCard(0, 1);
c1_A.addConstraint(and(implies(some(join($this(), c2_x)), none(join($this(), c3_y))), implies(some(join($this(), c3_y)), none(join($this(), c2_x)))));
