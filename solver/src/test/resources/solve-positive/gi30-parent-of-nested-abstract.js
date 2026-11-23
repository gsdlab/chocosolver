scope({c0_Port:2, c0_allPorts:2});
defaultScope(1);
intRange(-8, 7);
stringLength(16);

c0_Component = Abstract("c0_Component");
c0_Port = c0_Component.addAbstractChild("c0_Port");
c0_allPorts = c0_Component.addChild("c0_allPorts");
c0_WinController = Clafer("c0_WinController").withCard(1, 1);
c0_cmd = c0_WinController.addChild("c0_cmd").withCard(1, 1);
c0_powerOut = c0_WinController.addChild("c0_powerOut").withCard(1, 1);
c0_allPorts.refToUnique(c0_Port);
c0_Component.addConstraint(all([decl([p = local("p")], join(global(c0_Component), c0_Port))], ifOnlyIf(equal(joinParent(p), $this()), $in(p, joinRef(join($this(), c0_allPorts))))));
c0_WinController.extending(c0_Component);
c0_cmd.extending(c0_Port);
c0_powerOut.extending(c0_Port);
