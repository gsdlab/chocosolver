defaultScope(1);
intRange(-8, 7);
stringLength(16);

c0_x = Clafer("c0_x").withCard(1, 1);
c0_x.refTo(Int);
Constraint(equal(joinRef(global(c0_x)), div(minus(add(constant(1), constant(2))), constant(3))));
