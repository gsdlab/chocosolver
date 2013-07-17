defaultScope(1);
intRange(-8, 7);

c1_X = Clafer("c1_X").withCard(1, 1);
c2_x = c1_X.addChild("c2_x").withCard(1, 1);
c3_y = c2_x.addChild("c3_y").withCard(1, 1);
c6_Z = Clafer("c6_Z").withCard(1, 1);
c7_z = c6_Z.addChild("c7_z").withCard(1, 1);
c8_y = c7_z.addChild("c8_y").withCard(1, 1);
c7_z.refTo(Int);
c1_X.addConstraint(some(join($this(), c2_x)));
c6_Z.addConstraint(greaterThan(joinRef(join($this(), c7_z)), constant(0)));
