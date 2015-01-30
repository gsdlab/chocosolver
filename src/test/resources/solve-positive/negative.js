defaultScope(1);
intRange(-8, 7);
stringLength(16);

c0_z = Clafer("c0_z").withCard(1, 1);
c0_y = c0_z.addChild("c0_y").withCard(1, 1);
c0_y.refTo(Int);
c0_z.addConstraint(equal(joinRef(join($this(), c0_y)), constant(-1)));
