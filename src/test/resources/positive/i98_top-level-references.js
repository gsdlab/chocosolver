defaultScope(1);
intRange(-8, 7);

c1_Person = Abstract("c1_Person");
c13_NewBorn = Abstract("c13_NewBorn");
c2_age = c1_Person.addChild("c2_age").withCard(1, 1);
c12_married = c1_Person.addChild("c12_married").withCard(0, 1);
c2_age.refToUnique(Int);
c13_NewBorn.refToUnique(c1_Person);
c13_NewBorn.addConstraint(and(equal(joinRef(join(global(c1_Person), c2_age)), constant(0)), none(join(global(c1_Person), c12_married))));
