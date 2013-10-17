defaultScope(5);
intRange(-8, 7);

c1_Course = Abstract("c1_Course");
c13_Teacher = Abstract("c13_Teacher");
c2_taughtBy = c1_Course.addChild("c2_taughtBy");
c3_x = c2_taughtBy.addChild("c3_x").withCard(0, 1);
c14_teaches = c13_Teacher.addChild("c14_teaches");
c40_course = Clafer("c40_course").withCard(5, 5).extending(c1_Course);
c41_teacher = Clafer("c41_teacher").withCard(3, 3).extending(c13_Teacher);
c2_taughtBy.refToUnique(c13_Teacher);
c14_teaches.refToUnique(c1_Course);
Constraint(all([decl([c = local("c")], global(c1_Course))], all([decl([t = local("t")], join(c, c2_taughtBy))], and(some(join(t, c3_x)), $in(c, joinRef(join(joinRef(t), c14_teaches)))))));
