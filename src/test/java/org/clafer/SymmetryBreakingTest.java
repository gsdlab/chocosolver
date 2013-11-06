package org.clafer;

import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstModel;
import static org.clafer.ast.Asts.*;
import org.clafer.compiler.ClaferCompiler;
import org.clafer.compiler.ClaferSolver;
import org.clafer.scope.Scope;
import static org.junit.Assert.*;
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
    public void dontBreakCircularRef() {
        AstModel model = newModel();

        AstConcreteClafer person = model.addChild("Person").withCard(2, 2);
        AstConcreteClafer likes = person.addChild("Likes").refTo(person).withCard(Mandatory);

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(2));
        // Ideally, this should be 3 but the current strategy should have 4 instances.
        assertEquals(4, solver.allInstances().length);
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

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.set(b, 1).defaultScope(3));
        assertEquals(7, solver.allInstances().length);
    }
}
