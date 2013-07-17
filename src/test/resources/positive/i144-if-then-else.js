defaultScope(1);
intRange(-8, 7);

c1_AliceAge = Clafer("c1_AliceAge").withCard(1, 1);
c11_BobAge = Clafer("c11_BobAge").withCard(1, 1);
c21_BobsBirthday = Clafer("c21_BobsBirthday").withCard(0, 1);
c1_AliceAge.refToUnique(Int);
c11_BobAge.refToUnique(Int);
Constraint(equal(ifThenElse(some(global(c21_BobsBirthday)), joinRef(global(c11_BobAge)), joinRef(global(c1_AliceAge))), constant(5)));
