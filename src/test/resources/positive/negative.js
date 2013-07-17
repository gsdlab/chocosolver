defaultScope(1);
intRange(-8, 7);

c1_z = Clafer("c1_z").withCard(1, 1);
c2_y = c1_z.addChild("c2_y").withCard(1, 1);
c2_y.refTo(Int);
c1_z.addConstraint(equal(joinRef(join($this(), c2_y)), constant(-1)));
