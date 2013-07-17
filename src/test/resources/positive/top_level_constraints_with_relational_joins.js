defaultScope(1);
intRange(-8, 7);

c1_Course = Abstract("c1_Course");
c12_TA = Abstract("c12_TA");
c2_assistants = c1_Course.addChild("c2_assistants");
c13_first = c12_TA.addChild("c13_first").withCard(0, 1);
c28_CompilerGradStudent = Clafer("c28_CompilerGradStudent").withCard(1, 1).extending(c12_TA);
c29_AIGradStudent = Clafer("c29_AIGradStudent").withCard(1, 1).extending(c12_TA);
c30_CompilerCourse = Clafer("c30_CompilerCourse").withCard(1, 1).extending(c1_Course);
c31_MachineLearningCourse = Clafer("c31_MachineLearningCourse").withCard(1, 1).extending(c1_Course);
c42_numerOfAssistants = Clafer("c42_numerOfAssistants").withCard(1, 1);
c2_assistants.refToUnique(c12_TA);
c13_first.refToUnique(c1_Course);
c42_numerOfAssistants.refTo(Int);
Constraint(equal(joinRef(join(global(c28_CompilerGradStudent), c13_first)), global(c30_CompilerCourse)));
Constraint(equal(joinRef(join(global(c29_AIGradStudent), c13_first)), global(c31_MachineLearningCourse)));
Constraint(equal(joinRef(global(c42_numerOfAssistants)), card(join(global(c1_Course), c2_assistants))));
c13_first.addConstraint($in(joinParent($this()), joinRef(join(joinRef($this()), c2_assistants))));
