importClass(Packages.org.clafer.ast.Asts);

var scope = rc.setScope.bind(rc);
var defaultScope = rc.setDefaultScope.bind(rc);
var intRange = rc.setIntRange.bind(rc);
var clafer = rc.getModel().addTopClafer.bind(rc.getModel());
var abstract = rc.getModel().addAbstractClafer.bind(rc.getModel());

var int = Asts.IntType;
var $this = Asts.$this;
var constant = Asts.constant;
var join = Asts.join;
var joinParent = Asts.joinParent;
var joinRef = Asts.joinRef;
var equal = Asts.equal;
var notEqual = Asts.notEqual;
var lessThan = Asts.lessThan;
var lessThanEqual = Asts.lessThanEqual;
var greaterThan = Asts.greaterThan;
var greaterThanEqual = Asts.greaterThanEqual;
var none = Asts.none;