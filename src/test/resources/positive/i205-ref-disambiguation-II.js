defaultScope(4);
intRange(-8, 7);

c10_Car = Clafer("c10_Car").withCard(4, 4);
c11_owner = c10_Car.addChild("c11_owner").withCard(1, 1);
c21_Person = Clafer("c21_Person").withCard(4, 4);
c11_owner.refToUnique(c21_Person);
Constraint(all([disjDecl([c1 = local("c1"), c2 = local("c2")], global(c10_Car))], notEqual(joinRef(join(c1, c11_owner)), joinRef(join(c2, c11_owner)))));
