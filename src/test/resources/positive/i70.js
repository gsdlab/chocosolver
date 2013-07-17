defaultScope(1);
intRange(-8, 7);

c1_Clafer1 = Clafer("c1_Clafer1").withCard(1, 1);
c2_type = c1_Clafer1.addChild("c2_type").withCard(1, 1).withGroupCard(1, 1);
c3_Type1 = c2_type.addChild("c3_Type1").withCard(0, 1);
c4_Type2 = c2_type.addChild("c4_Type2").withCard(0, 1);
c5_Clafer2 = Clafer("c5_Clafer2").withCard(1, 1);
c6_clafer1 = c5_Clafer2.addChild("c6_clafer1").withCard(1, 1);
c16_type = c5_Clafer2.addChild("c16_type").withCard(1, 1).withGroupCard(1, 1);
c17_Type1 = c16_type.addChild("c17_Type1").withCard(0, 1);
c18_Type2 = c16_type.addChild("c18_Type2").withCard(0, 1);
c6_clafer1.refToUnique(c1_Clafer1);
c5_Clafer2.addConstraint(implies(some(join(join($this(), c16_type), c17_Type1)), some(join(join(joinRef(join($this(), c6_clafer1)), c2_type), c4_Type2))));
c5_Clafer2.addConstraint(implies(some(join(join($this(), c16_type), c17_Type1)), some(join(join(joinRef(join($this(), c6_clafer1)), c2_type), c4_Type2))));
