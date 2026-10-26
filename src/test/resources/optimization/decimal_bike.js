// Decimals: the Javascript clafer would generate for this model.
//
// abstract Feature
//     cost -> decimal
//     weight -> decimal
//
// abstract Bike
//     Light : Feature ?
//         [ cost = 19.99 ]
//         [ weight = 0.25 ]
//     Rack : Feature ?
//         [ cost = 34.5 ]
//         [ weight = 1.2 ]
//     Bell : Feature ?
//         [ cost = 4.75 ]
//         [ weight = 0.05 ]
//     Frame : Feature
//         [ cost = 250 ]
//         [ weight = 9.8 ]
//     subtotal -> decimal = sum Feature.cost
//     tax -> decimal = subtotal * 0.13
//     total -> decimal = subtotal + tax
//     weightPerFeature -> decimal = (sum Feature.weight) / #Feature
//     [ total <= 340 ]
//     [ weightPerFeature < 5 ]
//
// aBike : Bike
//
// << max aBike.total >>
//
// Three configurations satisfy the constraints: Bell (total 287.87), Light and
// Bell (310.46), Rack and Bell (326.85, the tax 289.25 * 0.13 = 37.6025 rounds
// to 37.60). The optimum is Rack and Bell.
scope({c0_Feature:4, c0_cost:4, c0_weight:4});
defaultScope(1);
stringLength(16);
decimalScale(2);

c0_Feature = Abstract("c0_Feature");
c0_Bike = Abstract("c0_Bike");
c0_cost = c0_Feature.addChild("c0_cost").withCard(1, 1);
c0_weight = c0_Feature.addChild("c0_weight").withCard(1, 1);
c0_Light = c0_Bike.addChild("c0_Light").withCard(0, 1).extending(c0_Feature);
c0_Rack = c0_Bike.addChild("c0_Rack").withCard(0, 1).extending(c0_Feature);
c0_Bell = c0_Bike.addChild("c0_Bell").withCard(0, 1).extending(c0_Feature);
c0_Frame = c0_Bike.addChild("c0_Frame").withCard(1, 1).extending(c0_Feature);
c0_subtotal = c0_Bike.addChild("c0_subtotal").withCard(1, 1);
c0_tax = c0_Bike.addChild("c0_tax").withCard(1, 1);
c0_total = c0_Bike.addChild("c0_total").withCard(1, 1);
c0_weightPerFeature = c0_Bike.addChild("c0_weightPerFeature").withCard(1, 1);
c0_aBike = Clafer("c0_aBike").withCard(1, 1).extending(c0_Bike);
c0_cost.refTo(Decimal);
c0_weight.refTo(Decimal);
c0_subtotal.refTo(Decimal);
c0_tax.refTo(Decimal);
c0_total.refTo(Decimal);
c0_weightPerFeature.refTo(Decimal);
c0_Light.addConstraint(equal(joinRef(join($this(), c0_cost)), decimal("19.99")));
c0_Light.addConstraint(equal(joinRef(join($this(), c0_weight)), decimal("0.25")));
c0_Rack.addConstraint(equal(joinRef(join($this(), c0_cost)), decimal("34.5")));
c0_Rack.addConstraint(equal(joinRef(join($this(), c0_weight)), decimal("1.2")));
c0_Bell.addConstraint(equal(joinRef(join($this(), c0_cost)), decimal("4.75")));
c0_Bell.addConstraint(equal(joinRef(join($this(), c0_weight)), decimal("0.05")));
c0_Frame.addConstraint(equal(joinRef(join($this(), c0_cost)), constant(250)));
c0_Frame.addConstraint(equal(joinRef(join($this(), c0_weight)), decimal("9.8")));
c0_Bike.addConstraint(implies(some(join($this(), c0_subtotal)), equal(joinRef(join($this(), c0_subtotal)), sum(join(global(c0_Feature), c0_cost)))));
c0_Bike.addConstraint(implies(some(join($this(), c0_tax)), equal(joinRef(join($this(), c0_tax)), mul(joinRef(join($this(), c0_subtotal)), decimal("0.13")))));
c0_Bike.addConstraint(implies(some(join($this(), c0_total)), equal(joinRef(join($this(), c0_total)), add(joinRef(join($this(), c0_subtotal)), joinRef(join($this(), c0_tax))))));
c0_Bike.addConstraint(implies(some(join($this(), c0_weightPerFeature)), equal(joinRef(join($this(), c0_weightPerFeature)), div(sum(join(global(c0_Feature), c0_weight)), card(global(c0_Feature))))));
c0_Bike.addConstraint(lessThanEqual(joinRef(join($this(), c0_total)), constant(340)));
c0_Bike.addConstraint(lessThan(joinRef(join($this(), c0_weightPerFeature)), constant(5)));
max(joinRef(join(global(c0_aBike), c0_total)));
