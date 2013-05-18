package org.clafer;

import org.clafer.ast.scope.Scope;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstModel;
import org.clafer.ast.Asts;
import org.clafer.compiler.ClaferCompiler;
import org.clafer.compiler.ClaferSolver;
import static org.junit.Assert.*;
import org.junit.Test;

/**
 *
 * @author jimmy
 */
public class SymmetryBreakingTest {

    @Test
    public void breakChildrenSwap() {
        AstModel model = Asts.newModel();

        AstConcreteClafer patron = model.addTopClafer("Patron").withCard(2, 2);
        AstConcreteClafer food = patron.addChild("Food").withCard(1, 2);
        AstConcreteClafer drink = patron.addChild("Drink").withCard(1, 2);

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(3).toScope());

        assertEquals(5, solver.allInstances().length);
    }

    /**
     * Currently fails.
     * 
     * 
     */
    @Test
    public void breakGrandChildrenSwap() {
        /*
         * 19 nonisomorphic solutions:
         * Run "length $ nubBy isomorphic $  makeModel 3" with the Haskell code below.
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
         *                   return False
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
        AstModel model = Asts.newModel();

        AstConcreteClafer patron = model.addTopClafer("Patron").withCard(2, 2);
        AstConcreteClafer food = patron.addChild("Food").withCard(1);
        AstConcreteClafer cheese = food.addChild("Cheese").withCard(0);

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(3).toScope());

        assertEquals(19, solver.allInstances().length);
    }
}
