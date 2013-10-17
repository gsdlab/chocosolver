defaultScope(2);
intRange(-8, 7);

c1_Measurable = Abstract("c1_Measurable");
c2_performance = c1_Measurable.addChild("c2_performance").withCard(1, 1);
c3_D = Clafer("c3_D").withCard(1, 1).extending(c1_Measurable);
c4_C = Clafer("c4_C").withCard(1, 1).extending(c1_Measurable);
c2_performance.refTo(Int);
Constraint(equal(joinRef(join(global(c4_C), c2_performance)), minus(joinRef(join(global(c3_D), c2_performance)))));
