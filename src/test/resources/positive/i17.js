defaultScope(1);
intRange(-8, 7);

c1_A = Abstract("c1_A");
c2_b = c1_A.addChild("c2_b");
c3_c = c2_b.addChild("c3_c").withCard(0, 1);
c1_A.addConstraint(some([decl([b1 = local("b1")], join($this(), c2_b))], some(join(b1, c3_c))));
