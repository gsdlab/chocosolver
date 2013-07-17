defaultScope(1);
intRange(-8, 7);

c1_Path = Abstract("c1_Path");
c2_p = c1_Path.addChild("c2_p").withCard(0, 1).extending(c1_Path);
c3_isDir = c1_Path.addChild("c3_isDir").withCard(0, 1);
c13_pth = c1_Path.addChild("c13_pth").withCard(1, 1).extending(c1_Path);
c1_Path.addConstraint(and(some(join($this(), c3_isDir)), some(join($this(), c2_p))));
