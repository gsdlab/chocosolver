scope({c0_Course:2, c0_TA:2, c0_assistants:4, c0_first:2});
defaultScope(1);
intRange(-8, 7);
stringLength(16);

c0_Course = Abstract("c0_Course");
c0_TA = Abstract("c0_TA");
c0_assistants = c0_Course.addChild("c0_assistants");
c0_first = c0_TA.addChild("c0_first").withCard(0, 1);
c0_CompilerGradStudent = Clafer("c0_CompilerGradStudent").withCard(1, 1).extending(c0_TA);
c0_AIGradStudent = Clafer("c0_AIGradStudent").withCard(1, 1).extending(c0_TA);
c0_CompilerCourse = Clafer("c0_CompilerCourse").withCard(1, 1).extending(c0_Course);
c0_MachineLearningCourse = Clafer("c0_MachineLearningCourse").withCard(1, 1).extending(c0_Course);
c0_numerOfAssistants = Clafer("c0_numerOfAssistants").withCard(1, 1);
c0_assistants.refToUnique(c0_TA);
c0_first.refTo(c0_Course);
c0_numerOfAssistants.refTo(Int);
Constraint(equal(joinRef(join(global(c0_CompilerGradStudent), c0_first)), global(c0_CompilerCourse)));
Constraint(equal(joinRef(join(global(c0_AIGradStudent), c0_first)), global(c0_MachineLearningCourse)));
Constraint(implies(some(global(c0_numerOfAssistants)), equal(joinRef(global(c0_numerOfAssistants)), card(join(global(c0_Course), c0_assistants)))));
c0_first.addConstraint($in(joinParent($this()), joinRef(join(joinRef($this()), c0_assistants))));