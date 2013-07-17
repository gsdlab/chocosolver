defaultScope(1);
intRange(-8, 7);

c1_a = Clafer("c1_a").withCard(0, 1);
c2_b = Clafer("c2_b").withCard(1, 1);
c3_c = Clafer("c3_c").withCard(1);
c4_d = Clafer("c4_d").withCard(2);
c5_e = c4_d.addChild("c5_e").withCard(0, 1);
c6_f = c4_d.addChild("c6_f").withCard(1, 1);
c7_g = c4_d.addChild("c7_g").withCard(2);
