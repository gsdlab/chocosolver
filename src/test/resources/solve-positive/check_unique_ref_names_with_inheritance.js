defaultScope(1);
intRange(-8, 7);
stringLength(16);

c0_date = Abstract("c0_date");
c0_Person = Abstract("c0_Person");
c0_Name = c0_Person.addChild("c0_Name").withCard(1, 1);
c0_DateOfBirth = c0_Person.addChild("c0_DateOfBirth").withCard(1, 1).extending(c0_date);
c0_JohnDoe = Clafer("c0_JohnDoe").withCard(1, 1).extending(c0_Person);
c0_date.refTo(string);
c0_Name.refTo(string);
c0_JohnDoe.addConstraint(and(equal(joinRef(join($this(), c0_Name)), constant("\"John\"")), equal(joinRef(join($this(), c0_DateOfBirth)), constant("\"01/02/1985\""))));
