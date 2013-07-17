defaultScope(1);
intRange(-8, 7);

c1_Person = Abstract("c1_Person");
c2_Age = c1_Person.addChild("c2_Age").withCard(1, 1);
c3_Team = Clafer("c3_Team").withCard(1, 1);
c4_Contractor = c3_Team.addChild("c4_Contractor").withCard(1, 1).extending(c1_Person);
c5_since = c4_Contractor.addChild("c5_since").withCard(1, 1);
c15_Member = c3_Team.addChild("c15_Member").withCard(2);
c25_Alice = Clafer("c25_Alice").withCard(1, 1).extending(c1_Person);
c26_Bob = Clafer("c26_Bob").withCard(1, 1).extending(c1_Person);
c2_Age.refTo(Int);
c5_since.refToUnique(Int);
c15_Member.refToUnique(c1_Person);
