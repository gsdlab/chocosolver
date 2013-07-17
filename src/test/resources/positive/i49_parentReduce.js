defaultScope(1);
intRange(-8, 7);

c1_claferA = Clafer("c1_claferA").withCard(1, 1);
c2_claferB = c1_claferA.addChild("c2_claferB").withCard(1, 1);
c5_claferC = c2_claferB.addChild("c5_claferC").withCard(1, 1);
c2_claferB.addConstraint(some(joinParent($this())));
c5_claferC.addConstraint(some(joinParent($this())));
c5_claferC.addConstraint(some(join(joinParent($this()), c5_claferC)));
