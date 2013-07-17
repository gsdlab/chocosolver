defaultScope(1);
intRange(-8, 7);

c1_Person = Clafer("c1_Person").withCard(1, 1);
c2_maritalStatus = c1_Person.addChild("c2_maritalStatus").withCard(1, 1);
c3_married = c2_maritalStatus.addChild("c3_married").withCard(1, 1);
c3_married.addConstraint(some(joinParent(joinParent($this()))));
