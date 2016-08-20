package org.clafer;

import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstModel;
import static org.clafer.ast.Asts.$this;
import static org.clafer.ast.Asts.IntType;
import static org.clafer.ast.Asts.Mandatory;
import static org.clafer.ast.Asts.Optional;
import static org.clafer.ast.Asts.card;
import static org.clafer.ast.Asts.constant;
import static org.clafer.ast.Asts.equal;
import static org.clafer.ast.Asts.global;
import static org.clafer.ast.Asts.ifThenElse;
import static org.clafer.ast.Asts.in;
import static org.clafer.ast.Asts.join;
import static org.clafer.ast.Asts.joinParent;
import static org.clafer.ast.Asts.joinRef;
import static org.clafer.ast.Asts.lessThanEqual;
import static org.clafer.ast.Asts.newModel;
import static org.clafer.ast.Asts.sum;
import org.clafer.compiler.ClaferCompiler;
import org.clafer.compiler.ClaferOptimizer;
import org.clafer.compiler.ClaferSolver;
import org.clafer.objective.Objective;
import org.clafer.scope.Scope;
import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import org.junit.Test;

/**
 *
 * @author jimmy
 */
public class SymmetryBreakingTest {

    /**
     * <pre>
     * Patron 2
     *     Food 1..2
     *     Drink 1..2
     * </pre>
     */
    @Test(timeout = 60000)
    public void breakChildrenSwap() {
        AstModel model = newModel();

        AstConcreteClafer patron = model.addChild("Patron").withCard(2, 2);
        AstConcreteClafer food = patron.addChild("Food").withCard(1, 2);
        AstConcreteClafer drink = patron.addChild("Drink").withCard(1, 2);

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(3));
        assertEquals(5, solver.allInstances().length);
    }

    /**
     * <pre>
     * abstract Eater
     *     Food 1..2
     * Patron : Eater 2
     *     Drink 1..2
     * </pre>
     */
    @Test(timeout = 60000)
    public void breakInheritedChildrenSwap() {
        AstModel model = newModel();

        AstAbstractClafer eater = model.addAbstract("Eater");
        AstConcreteClafer food = eater.addChild("Food").withCard(1, 2);
        AstConcreteClafer patron = model.addChild("Patron").extending(eater).withCard(2, 2);
        AstConcreteClafer drink = patron.addChild("Drink").withCard(1, 2);

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(3));
        assertEquals(5, solver.allInstances().length);
    }

    /**
     * <pre>
     * abstract Eater
     *     Food 1..2
     *     Drink 1..2
     * Patron : Eater 2
     * </pre>
     */
    @Test(timeout = 60000)
    public void breakOnlyInheritedChildrenSwap() {
        AstModel model = newModel();

        AstAbstractClafer eater = model.addAbstract("Eater");
        AstConcreteClafer food = eater.addChild("Food").withCard(1, 2);
        AstConcreteClafer drink = eater.addChild("Drink").withCard(1, 2);
        AstConcreteClafer patron = model.addChild("Patron").extending(eater).withCard(2, 2);

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(3));
        assertEquals(5, solver.allInstances().length);
    }

    /**
     * <pre>
     * Patron 2
     *     Money ->> integer 1..2
     * </pre>
     */
    @Test(timeout = 60000)
    public void dontBreakUnrelatedRefs() {
        AstModel model = newModel();

        AstConcreteClafer patron = model.addChild("Patron").withCard(2, 2);
        AstConcreteClafer money = patron.addChild("Money").refTo(IntType).withCard(1, 2);

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(4).intLow(0).intHigh(1));
        assertEquals(15, solver.allInstances().length);
    }

    /**
     * <pre>
     * Person 2
     *     Likes -> Person
     * </pre>
     */
    @Test(timeout = 60000)
    public void dontBreakSingleCircularRef() {
        AstModel model = newModel();

        AstConcreteClafer person = model.addChild("Person").withCard(2, 2);
        AstConcreteClafer likes = person.addChild("Likes").refTo(person).withCard(Mandatory);

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(2));
        assertEquals(3, solver.allInstances().length);
    }

    /**
     * <pre>
     * A ->> B 3
     * B ->> A 3
     * </pre>
     */
    @Test(timeout = 60000)
    public void breakHalfPairCircularRef() {
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A").withCard(3, 3);
        AstConcreteClafer b = model.addChild("B").withCard(3, 3);
        a.refTo(b);
        b.refTo(a);
        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(3));
        // Ideally, this should be 25 but the current strategy should have 54 instances.
        assertEquals(54, solver.allInstances().length);
    }

    /**
     * <pre>
     * abstract Service
     *     cpu: integer
     *     machine -> Machine // the -> means every service must be allocated to some machine
     *     [ this in machine.services ]
     *
     * MailService : Service
     *     [cpu = 8]
     *
     * SearchService : Service
     *     [cpu = 7]
     *
     * CalendarService : Service
     *     [cpu = 5]
     *
     * DriveService : Service
     *     [cpu = 4]
     *
     * GroupsService : Service
     *     [cpu = 5]
     *
     * abstract Machine
     *     services -> Service*
     *     [this.machine = parent]
     *     total_cpu : integer = sum services.cpu
     *     [total_cpu &lt;= this.cpuLimit]
     *     cpuLimit : integer
     *     isFree : integer = (if (#this.services = 0) then 1 else 0)
     *
     * GoogleCA : Machine
     *     [cpuLimit = 20]
     *
     * GoogleNY : Machine
     *     [cpuLimit = 10]
     *
     * GoogleTX : Machine
     *     [cpuLimit = 11]
     *
     * abstract Task
     *     total_free : integer = sum Machine.isFree
     *
     * MyTask: Task
     * &lt;&lt;max MyTask.total_free&gt;&gt;
     * </pre>
     */
    @Test(timeout = 60000)
    public void breakServices() {
        AstModel model = newModel();

        AstAbstractClafer c0_Service = model.addAbstract("Service");
        AstAbstractClafer c0_Machine = model.addAbstract("c0_Machine");
        AstAbstractClafer c0_Task = model.addAbstract("c0_Task");
        AstConcreteClafer c0_cpu = c0_Service.addChild("c0_cpu").withCard(1, 1);
        AstConcreteClafer c0_machine = c0_Service.addChild("c0_machine").withCard(1, 1);
        AstConcreteClafer c0_MailService = model.addChild("c0_MailService").withCard(1, 1).extending(c0_Service);
        AstConcreteClafer c0_SearchService = model.addChild("c0_SearchService").withCard(1, 1).extending(c0_Service);
        AstConcreteClafer c0_CalendarService = model.addChild("c0_CalendarService").withCard(1, 1).extending(c0_Service);
        AstConcreteClafer c0_DriveService = model.addChild("c0_DriveService").withCard(1, 1).extending(c0_Service);
        AstConcreteClafer c0_GroupsService = model.addChild("c0_GroupsService").withCard(1, 1).extending(c0_Service);
        AstConcreteClafer c0_services = c0_Machine.addChild("c0_services");
        AstConcreteClafer c0_total_cpu = c0_Machine.addChild("c0_total_cpu").withCard(1, 1);
        AstConcreteClafer c0_cpuLimit = c0_Machine.addChild("c0_cpuLimit").withCard(1, 1);
        AstConcreteClafer c0_isFree = c0_Machine.addChild("c0_isFree").withCard(1, 1);
        AstConcreteClafer c0_GoogleCA = model.addChild("c0_GoogleCA").withCard(1, 1).extending(c0_Machine);
        AstConcreteClafer c0_GoogleNY = model.addChild("c0_GoogleNY").withCard(1, 1).extending(c0_Machine);
        AstConcreteClafer c0_GoogleTX = model.addChild("c0_GoogleTX").withCard(1, 1).extending(c0_Machine);
        AstConcreteClafer c0_total_free = c0_Task.addChild("c0_total_free").withCard(1, 1);
        AstConcreteClafer c0_MyTask = model.addChild("c0_MyTask").withCard(1, 1).extending(c0_Task);
        c0_cpu.refTo(IntType);
        c0_machine.refToUnique(c0_Machine);
        c0_services.refToUnique(c0_Service);
        c0_total_cpu.refTo(IntType);
        c0_cpuLimit.refTo(IntType);
        c0_isFree.refTo(IntType);
        c0_total_free.refTo(IntType);
        c0_Service.addConstraint(in($this(), joinRef(join(joinRef(join($this(), c0_machine)), c0_services))));
        c0_MailService.addConstraint(equal(joinRef(join($this(), c0_cpu)), constant(8)));
        c0_SearchService.addConstraint(equal(joinRef(join($this(), c0_cpu)), constant(7)));
        c0_CalendarService.addConstraint(equal(joinRef(join($this(), c0_cpu)), constant(5)));
        c0_DriveService.addConstraint(equal(joinRef(join($this(), c0_cpu)), constant(4)));
        c0_GroupsService.addConstraint(equal(joinRef(join($this(), c0_cpu)), constant(5)));
        c0_Machine.addConstraint(equal(joinRef(join($this(), c0_total_cpu)), sum(join(joinRef(join($this(), c0_services)), c0_cpu))));
        c0_Machine.addConstraint(lessThanEqual(joinRef(join($this(), c0_total_cpu)), joinRef(join($this(), c0_cpuLimit))));
        c0_Machine.addConstraint(equal(joinRef(join($this(), c0_isFree)), ifThenElse(equal(card(join($this(), c0_services)), constant(0)), constant(1), constant(0))));
        c0_services.addConstraint(equal(joinRef(join(joinRef($this()), c0_machine)), joinParent($this())));
        c0_GoogleCA.addConstraint(equal(joinRef(join($this(), c0_cpuLimit)), constant(20)));
        c0_GoogleNY.addConstraint(equal(joinRef(join($this(), c0_cpuLimit)), constant(10)));
        c0_GoogleTX.addConstraint(equal(joinRef(join($this(), c0_cpuLimit)), constant(11)));
        c0_Task.addConstraint(equal(joinRef(join($this(), c0_total_free)), sum(join(global(c0_Machine), c0_isFree))));

        ClaferOptimizer solver = ClaferCompiler.compile(
                model,
                Scope.setScope(c0_services, 5).setScope(c0_Machine, 4)
                .setScope(c0_Service, 5).setScope(c0_Task, 1)
                .setScope(c0_cpu, 5).setScope(c0_cpuLimit, 4)
                .setScope(c0_isFree, 4).setScope(c0_machine, 5)
                .setScope(c0_total_cpu, 4).setScope(c0_total_free, 1)
                .intLow(-128).intHigh(128),
                Objective.maximize(joinRef(join(global(c0_MyTask), c0_total_free))));
        int count = 0;
        while (solver.find()) {
            assertArrayEquals(new int[]{1}, solver.optimalValues());
            count++;
        }
        assertEquals(7, count);
    }

    /**
     * <pre>
     * Patron 2
     *     Food +
     *         Cheese *
     * </pre>
     */
    @Test(timeout = 60000)
    public void breakGrandChildrenSwap() {
        /*
         * 19 nonisomorphic solutions:
         * Run "length $ nubBy isomorphic $ makeModel 3" with the Haskell code below.
         * 
         *   import Control.Monad
         *   import Data.Functor
         *   import Data.List
         * 
         *   data Model = Model {person1::Person, person2::Person} deriving (Eq, Ord, Show)
         *   data Person = Person {foods::[Food]} deriving (Eq, Ord, Show)
         *   data Food = Food {cheeses::[Cheese]} deriving (Eq, Ord, Show)
         *   data Cheese = Cheese deriving (Eq, Ord, Show)
         * 
         *   people model = [person1 model, person2 model]
         * 
         *   class Iso a where
         *       isomorphic :: a -> a -> Bool
         * 
         *   instance Iso Model where
         *       isomorphic model1 model2 =
         *           ((person1 model1 `isomorphic` person1 model2) && (person2 model1 `isomorphic` person2 model2)) ||
         *           ((person1 model1 `isomorphic` person2 model2) && (person2 model1 `isomorphic` person1 model2))
         * 
         *   instance Iso Person where
         *       isomorphic Person{foods = foods1} Person{foods = foods2}
         *           | length foods1 == length foods2 =
         *               or $ do
         *                   pfoods2 <- permutations foods2
         *                   return $ and $ zipWith isomorphic foods1 pfoods2
         *           | otherwise = False
         * 
         *   instance Iso Food where
         *       isomorphic food1 food2 = length (cheeses food1) == length (cheeses food2)
         * 
         *   instance Iso Cheese where
         *       isomorphic _ _ = True
         * 
         *   numberOfFood = length . (>>= foods) . people
         *   numberOfCheese = length . (>>= cheeses) . (>>= foods) . people
         * 
         *   printModel model =
         *       printPerson =<< people model
         *       where
         *           printPerson person = "Person\n" ++ (printFood =<< foods person)
         *           printFood food = "    Food\n" ++ (printCheese =<< cheeses food)
         *           printCheese _ = "        Cheese\n"
         * 
         *   {-
         *    - Person 2
         *    -    Food +
         *    -        Cheese *
         *    -}        
         *   makeModel scope =
         *       do
         *           p1 <- makePerson
         *           p2 <- makePerson
         *           let m = Model p1 p2
         *           guard $ numberOfFood m <= scope
         *           guard $ numberOfCheese m <= scope
         *           return m
         *       where
         *           makePerson = do
         *               feed <- [1..scope]
         *               Person <$> replicateM feed makeFood
         *           makeFood = do
         *               topping <- [0..scope]
         *               return $ Food $ replicate topping Cheese
         */
        AstModel model = newModel();

        AstConcreteClafer patron = model.addChild("Patron").withCard(2, 2);
        AstConcreteClafer food = patron.addChild("Food").withCard(1);
        AstConcreteClafer cheese = food.addChild("Cheese").withCard(0);

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(3));
        assertEquals(19, solver.allInstances().length);
    }

    /**
     * <pre>
     * a : A *
     * setRefToA -> a 3
     * multisetRefToA ->> a 3
     * </pre>
     */
    @Test(timeout = 60000)
    public void breakRefSwap() {
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("a");
        AstConcreteClafer setRefToA = model.addChild("setRefToA").refToUnique(a).withCard(3, 3);
        AstConcreteClafer multisetRefToA = model.addChild("multisetRefToA").refTo(a).withCard(3, 3);

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(3));
        assertEquals(3, solver.allInstances().length);
    }

    /**
     * <pre>
     * abstract A
     * a : A *
     * setRefToA -> A 3
     * multisetRefToA ->> A 3
     * </pre>
     */
    @Test(timeout = 60000)
    public void breakAbstractRefSwap() {
        AstModel model = newModel();

        AstAbstractClafer A = model.addAbstract("A");
        AstConcreteClafer a = model.addChild("a").extending(A);
        AstConcreteClafer setRefToA = model.addChild("setRefToA").refToUnique(A).withCard(3, 3);
        AstConcreteClafer multisetRefToA = model.addChild("multisetRefToA").refTo(A).withCard(3, 3);

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(3));
        assertEquals(3, solver.allInstances().length);
    }

    /**
     * <pre>
     * abstract A
     * a : A
     * b : A
     * c : A
     * setRefToA -> A 3
     * multisetRefToA ->> A 3
     * </pre>
     */
    @Test(timeout = 60000)
    public void breakAbstractRefSwapMultipleConcrete() {
        AstModel model = newModel();

        AstAbstractClafer A = model.addAbstract("A");
        AstConcreteClafer a = model.addChild("a").extending(A).withCard(Mandatory);
        AstConcreteClafer b = model.addChild("b").extending(A).withCard(Mandatory);
        AstConcreteClafer c = model.addChild("c").extending(A).withCard(Mandatory);
        AstConcreteClafer setRefToA = model.addChild("setRefToA").refToUnique(A).withCard(3, 3);
        AstConcreteClafer multisetRefToA = model.addChild("multisetRefToA").refTo(A).withCard(3, 3);

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(3));
        assertEquals(10, solver.allInstances().length);
    }

    /**
     * <pre>
     * abstract A
     * a : A 4
     * b -> A
     * c -> A
     * d -> a
     * e -> a
     * </pre>
     */
    @Test(timeout = 60000)
    public void breakNonIsomorphicSourceIsomorphicTarget() {
        AstModel model = newModel();

        AstAbstractClafer A = model.addAbstract("A");
        AstConcreteClafer a = model.addChild("1").extending(A).withCard(4, 4);
        AstConcreteClafer b = model.addChild("b").refTo(A).withCard(Mandatory);
        AstConcreteClafer c = model.addChild("c").refTo(A).withCard(Mandatory);
        AstConcreteClafer d = model.addChild("d").refTo(a).withCard(Mandatory);
        AstConcreteClafer e = model.addChild("e").refTo(a).withCard(Mandatory);

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(4));
        assertEquals(15, solver.allInstances().length);
    }

    /**
     * <pre>
     * A 3
     *     B ?
     * C -> A
     * D -> A
     * </pre>
     */
    @Test(timeout = 60000)
    public void breakTargetRefWithChildren() {
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A").withCard(3, 3);
        AstConcreteClafer b = a.addChild("B").withCard(Optional);
        AstConcreteClafer c = model.addChild("C").refTo(a).withCard(Mandatory);
        AstConcreteClafer d = model.addChild("D").refTo(a).withCard(Mandatory);

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.setScope(b, 1).defaultScope(3));
        assertEquals(7, solver.allInstances().length);
    }
}
