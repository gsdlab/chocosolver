defaultScope(1);
intRange(-8, 7);

c1_person = Clafer("c1_person").withCard(1, 1);
c2_haha = c1_person.addChild("c2_haha").withCard(1, 1);
c3_lala = c2_haha.addChild("c3_lala").withCard(1, 1);
c4_age = c3_lala.addChild("c4_age").withCard(1, 1);
c5_ble = c1_person.addChild("c5_ble").withCard(1, 1);
c6_married = c5_ble.addChild("c6_married").withCard(0, 1);
c4_age.refTo(Int);
c6_married.addConstraint(greaterThanEqual(joinRef(join(join(join(joinParent(joinParent($this())), c2_haha), c3_lala), c4_age)), constant(18)));
