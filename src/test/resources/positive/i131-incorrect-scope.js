defaultScope(1);
intRange(-8, 7);

c1_Animal = Abstract("c1_Animal");
c2_leg = c1_Animal.addChild("c2_leg");
c3_Dog = Clafer("c3_Dog").extending(c1_Animal);
c8_Spider = Clafer("c8_Spider").extending(c1_Animal);
c3_Dog.addConstraint(equal(card(join($this(), c2_leg)), constant(4)));
c8_Spider.addConstraint(equal(card(join($this(), c2_leg)), constant(8)));
