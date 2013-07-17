defaultScope(1);
intRange(-8, 7);

c1_Dimension = Abstract("c1_Dimension");
c21_DimensionLevel = Abstract("c21_DimensionLevel");
c2_levels = c1_Dimension.addChild("c2_levels");
c22_belongsTo = c21_DimensionLevel.addChild("c22_belongsTo").withCard(1, 1);
c32_dim1 = Clafer("c32_dim1").withCard(1, 1).extending(c1_Dimension);
c33_dim2 = Clafer("c33_dim2").withCard(1, 1).extending(c1_Dimension);
c34_dimLevel1 = Clafer("c34_dimLevel1").withCard(1, 1).extending(c21_DimensionLevel);
c35_dimLevel2 = Clafer("c35_dimLevel2").withCard(1, 1).extending(c21_DimensionLevel);
c2_levels.refToUnique(c21_DimensionLevel);
c22_belongsTo.refToUnique(c1_Dimension);
Constraint(some([disjDecl([dl1 = local("dl1"), dl2 = local("dl2")], global(c21_DimensionLevel))], equal(joinRef(join(dl1, c22_belongsTo)), joinRef(join(dl2, c22_belongsTo)))));
c1_Dimension.addConstraint(all([decl([dl = local("dl")], join($this(), c2_levels))], equal(joinRef(join(joinRef(dl), c22_belongsTo)), $this())));
