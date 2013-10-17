defaultScope(4);
intRange(-8, 7);

c1_Person = Abstract("c1_Person");
c4_WaitingLine = Abstract("c4_WaitingLine");
c2_JohnDoe = Clafer("c2_JohnDoe").withCard(1, 1).extending(c1_Person);
c3_MaryJane = Clafer("c3_MaryJane").withCard(1, 1).extending(c1_Person);
c5_participants = c4_WaitingLine.addChild("c5_participants");
c15_BusLine = Clafer("c15_BusLine").withCard(1, 1).extending(c4_WaitingLine);
c23_JohnAndMaryLine = Clafer("c23_JohnAndMaryLine").withCard(1, 1).extending(c4_WaitingLine);
c5_participants.refToUnique(c1_Person);
c15_BusLine.addConstraint(and($in(global(c2_JohnDoe), joinRef(join($this(), c5_participants))), $in(global(c3_MaryJane), joinRef(join($this(), c5_participants)))));
c23_JohnAndMaryLine.addConstraint(equal(joinRef(join($this(), c5_participants)), union(global(c2_JohnDoe), global(c3_MaryJane))));
