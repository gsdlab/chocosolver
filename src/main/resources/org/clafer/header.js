importClass(Packages.org.clafer.ast.Ast);

var setScope = rc.setScope.bind(rc);
var clafer = rc.getModel().addTopClafer.bind(rc.getModel());
var abstract = rc.getModel().addAbstractClafer.bind(rc.getModel());

var int = Ast.IntType;
var $this = Ast.$this;
var constantInt = Ast.constantInt;
var join = Ast.join;
var joinParent = Ast.joinParent;
var joinRef = Ast.joinRef;
var equal = Ast.equal;
var notEqual = Ast.notEqual;
var lessThan = Ast.lessThan;
var lessThanEqual = Ast.lessThanEqual;
var greaterThan = Ast.greaterThan;
var greaterThanEqual = Ast.greaterThanEqual;
var none = Ast.none;