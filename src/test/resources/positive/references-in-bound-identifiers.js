defaultScope(1);
intRange(-8, 7);

c1_A = Clafer("c1_A").withCard(1, 1);
c2_B = c1_A.addChild("c2_B");
c3_C = c2_B.addChild("c3_C").withCard(0, 1);
c2_B.refTo(Int);
c1_A.addConstraint(all([decl([b = local("b")], join($this(), c2_B))], some(join(b, c3_C))));
c1_A.addConstraint(all([decl([b = local("b")], join($this(), c2_B))], equal(add(joinRef(b), constant(1)), constant(2))));
