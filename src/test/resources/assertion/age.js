defaultScope(1);
intRange(-8, 7);
stringLength(16);

age = Clafer("age").withCard(1, 1);
age.refTo(Int);
age.addConstraint(equal(joinRef($this()), constant(10)));

assert(greaterThanEqual(joinRef(global(age)), constant(0)));
