package org.clafer.ontology;

import org.clafer.domain.Domains;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import org.junit.Test;

/**
 *
 * @author jimmy
 */
public class KnowledgeDatabaseTest {

    @Test
    public void testIsA() {
        KnowledgeDatabase kd = new KnowledgeDatabase();
        Concept colour = kd.newConcept("Colour");
        Concept red = kd.newConcept("Red");
        Concept blue = kd.newConcept("Blue");
        Concept shape = kd.newConcept("Shape");
        Concept threeD = kd.newConcept("3d");
        Concept twoD = kd.newConcept("2d");
        Concept cube = kd.newConcept("cube");
        Concept square = kd.newConcept("square");

        kd.newIsA(red, colour);
        kd.newIsA(blue, colour);
        kd.newIsA(threeD, shape);
        kd.newIsA(twoD, shape);
        kd.newIsA(cube, threeD);
        kd.newIsA(square, twoD);

        Oracle oracle = kd.oracle();

        assertTrue(oracle.isA(colour, colour));
        assertTrue(oracle.isA(red, colour));
        assertTrue(oracle.isA(blue, colour));
        assertFalse(oracle.isA(shape, colour));
        assertFalse(oracle.isA(threeD, colour));
        assertFalse(oracle.isA(twoD, colour));
        assertFalse(oracle.isA(cube, colour));
        assertFalse(oracle.isA(square, colour));

        assertFalse(oracle.isA(colour, red));
        assertTrue(oracle.isA(red, red));
        assertFalse(oracle.isA(blue, red));
        assertFalse(oracle.isA(shape, red));
        assertFalse(oracle.isA(threeD, red));
        assertFalse(oracle.isA(twoD, red));
        assertFalse(oracle.isA(cube, red));
        assertFalse(oracle.isA(square, red));

        assertFalse(oracle.isA(colour, blue));
        assertFalse(oracle.isA(red, blue));
        assertTrue(oracle.isA(blue, blue));
        assertFalse(oracle.isA(shape, blue));
        assertFalse(oracle.isA(threeD, blue));
        assertFalse(oracle.isA(twoD, blue));
        assertFalse(oracle.isA(cube, blue));
        assertFalse(oracle.isA(square, blue));

        assertFalse(oracle.isA(colour, shape));
        assertFalse(oracle.isA(red, shape));
        assertFalse(oracle.isA(blue, shape));
        assertTrue(oracle.isA(shape, shape));
        assertTrue(oracle.isA(threeD, shape));
        assertTrue(oracle.isA(twoD, shape));
        assertTrue(oracle.isA(cube, shape));
        assertTrue(oracle.isA(square, shape));

        assertFalse(oracle.isA(colour, threeD));
        assertFalse(oracle.isA(red, threeD));
        assertFalse(oracle.isA(blue, threeD));
        assertFalse(oracle.isA(shape, threeD));
        assertTrue(oracle.isA(threeD, threeD));
        assertFalse(oracle.isA(twoD, threeD));
        assertTrue(oracle.isA(cube, threeD));
        assertFalse(oracle.isA(square, threeD));

        assertFalse(oracle.isA(colour, twoD));
        assertFalse(oracle.isA(red, twoD));
        assertFalse(oracle.isA(blue, twoD));
        assertFalse(oracle.isA(shape, twoD));
        assertFalse(oracle.isA(threeD, twoD));
        assertTrue(oracle.isA(twoD, twoD));
        assertFalse(oracle.isA(cube, twoD));
        assertTrue(oracle.isA(square, twoD));

        assertFalse(oracle.isA(colour, cube));
        assertFalse(oracle.isA(red, cube));
        assertFalse(oracle.isA(blue, cube));
        assertFalse(oracle.isA(shape, cube));
        assertFalse(oracle.isA(threeD, cube));
        assertFalse(oracle.isA(twoD, cube));
        assertTrue(oracle.isA(cube, cube));
        assertFalse(oracle.isA(square, cube));

        assertFalse(oracle.isA(colour, square));
        assertFalse(oracle.isA(red, square));
        assertFalse(oracle.isA(blue, square));
        assertFalse(oracle.isA(shape, square));
        assertFalse(oracle.isA(threeD, square));
        assertFalse(oracle.isA(twoD, square));
        assertFalse(oracle.isA(cube, square));
        assertTrue(oracle.isA(square, square));
    }

    @Test
    public void testHasA() {
        KnowledgeDatabase kd = new KnowledgeDatabase();
        Concept colour = kd.newConcept("Colour");
        Concept primary = kd.newConcept("Primary");
        Concept red = kd.newConcept("Red");
        Concept blue = kd.newConcept("Blue");
        Concept orange = kd.newConcept("Orange");
        Concept intensity = kd.newConcept("Intensity");
        Concept purity = kd.newConcept("Purity");
        Concept fruit = kd.newConcept("Fruit");

        kd.newIsA(primary, colour);
        kd.newIsA(red, primary);
        kd.newIsA(blue, primary);
        kd.newIsA(orange, colour);

        kd.newHasA(colour, intensity);
        kd.newHasA(primary, purity);
        kd.newHasA(orange, fruit);

        Oracle oracle = kd.oracle();

        assertFalse(oracle.hasA(colour, colour));
        assertFalse(oracle.hasA(primary, colour));
        assertFalse(oracle.hasA(red, colour));
        assertFalse(oracle.hasA(blue, colour));
        assertFalse(oracle.hasA(orange, colour));
        assertFalse(oracle.hasA(intensity, colour));
        assertFalse(oracle.hasA(purity, colour));
        assertFalse(oracle.hasA(fruit, colour));

        assertFalse(oracle.hasA(colour, primary));
        assertFalse(oracle.hasA(primary, primary));
        assertFalse(oracle.hasA(red, primary));
        assertFalse(oracle.hasA(blue, primary));
        assertFalse(oracle.hasA(orange, primary));
        assertFalse(oracle.hasA(intensity, primary));
        assertFalse(oracle.hasA(purity, primary));
        assertFalse(oracle.hasA(fruit, primary));

        assertFalse(oracle.hasA(colour, red));
        assertFalse(oracle.hasA(primary, red));
        assertFalse(oracle.hasA(red, red));
        assertFalse(oracle.hasA(blue, red));
        assertFalse(oracle.hasA(orange, red));
        assertFalse(oracle.hasA(intensity, red));
        assertFalse(oracle.hasA(purity, red));
        assertFalse(oracle.hasA(fruit, red));

        assertFalse(oracle.hasA(colour, blue));
        assertFalse(oracle.hasA(primary, blue));
        assertFalse(oracle.hasA(red, blue));
        assertFalse(oracle.hasA(blue, blue));
        assertFalse(oracle.hasA(orange, blue));
        assertFalse(oracle.hasA(intensity, blue));
        assertFalse(oracle.hasA(purity, blue));
        assertFalse(oracle.hasA(fruit, blue));

        assertFalse(oracle.hasA(colour, orange));
        assertFalse(oracle.hasA(primary, orange));
        assertFalse(oracle.hasA(red, orange));
        assertFalse(oracle.hasA(blue, orange));
        assertFalse(oracle.hasA(orange, orange));
        assertFalse(oracle.hasA(intensity, orange));
        assertFalse(oracle.hasA(purity, orange));
        assertFalse(oracle.hasA(fruit, orange));

        assertTrue(oracle.hasA(colour, intensity));
        assertTrue(oracle.hasA(primary, intensity));
        assertTrue(oracle.hasA(red, intensity));
        assertTrue(oracle.hasA(blue, intensity));
        assertTrue(oracle.hasA(orange, intensity));
        assertFalse(oracle.hasA(intensity, intensity));
        assertFalse(oracle.hasA(purity, intensity));
        assertFalse(oracle.hasA(fruit, intensity));

        assertFalse(oracle.hasA(colour, purity));
        assertTrue(oracle.hasA(primary, purity));
        assertTrue(oracle.hasA(red, purity));
        assertTrue(oracle.hasA(blue, purity));
        assertFalse(oracle.hasA(orange, purity));
        assertFalse(oracle.hasA(intensity, purity));
        assertFalse(oracle.hasA(purity, purity));
        assertFalse(oracle.hasA(fruit, purity));

        assertFalse(oracle.hasA(colour, fruit));
        assertFalse(oracle.hasA(primary, fruit));
        assertFalse(oracle.hasA(red, fruit));
        assertFalse(oracle.hasA(blue, fruit));
        assertTrue(oracle.hasA(orange, fruit));
        assertFalse(oracle.hasA(intensity, fruit));
        assertFalse(oracle.hasA(purity, fruit));
        assertFalse(oracle.hasA(fruit, fruit));
    }

    @Test
    public void testAssignmentSubIsA() {
        KnowledgeDatabase kd = new KnowledgeDatabase();
        Concept colour = kd.newConcept("Colour");
        Concept primary = kd.newConcept("Primary");
        Concept red = kd.newConcept("Red");
        Concept blue = kd.newConcept("Blue");
        Concept orange = kd.newConcept("Orange");
        Concept intensity = kd.newConcept("Intensity");

        kd.newIsA(primary, colour);
        kd.newIsA(red, primary);
        kd.newIsA(blue, primary);
        kd.newIsA(orange, colour);

        kd.newHasA(colour, intensity);

        kd.newAssignment(new Path(red, intensity), 1);
        kd.newAssignment(new Path(blue, intensity), 2);

        Oracle oracle = kd.oracle();

        assertNull(oracle.getAssignment(colour, intensity));
        assertEquals(Domains.enumDomain(1, 2), oracle.getAssignment(primary, intensity));
        assertEquals(Domains.constantDomain(1), oracle.getAssignment(red, intensity));
        assertEquals(Domains.constantDomain(2), oracle.getAssignment(blue, intensity));
        assertNull(oracle.getAssignment(orange, intensity));
        assertNull(oracle.getAssignment(intensity));
    }

    @Test
    public void testAssignmentEquality() {
        KnowledgeDatabase kd = new KnowledgeDatabase();
        Concept party = kd.newConcept("Party");
        Concept alice = kd.newConcept("Alice");
        Concept bob = kd.newConcept("Bob");

        kd.newHasA(party, alice);
        kd.newHasA(party, bob);

        kd.newAssignment(new Path(alice), 1);
        kd.newEquality(new Path(party, alice), new Path(party, bob));

        Oracle oracle = kd.oracle();

        assertEquals(Domains.constantDomain(1), oracle.getAssignment(alice));
        assertEquals(Domains.constantDomain(1), oracle.getAssignment(bob));
    }

    @Test
    public void testAssignmentEqualityIntersection() {
        KnowledgeDatabase kd = new KnowledgeDatabase();
        Concept party = kd.newConcept("Party");
        Concept alice = kd.newConcept("Alice");
        Concept bob = kd.newConcept("Bob");

        kd.newHasA(party, alice);
        kd.newHasA(party, bob);

        kd.newAssignment(new Path(alice), Domains.boundDomain(1, 2));
        kd.newAssignment(new Path(bob), Domains.boundDomain(2, 3));
        kd.newEquality(new Path(party, alice), new Path(party, bob));

        Oracle oracle = kd.oracle();

        assertEquals(Domains.constantDomain(2), oracle.getAssignment(alice));
        assertEquals(Domains.constantDomain(2), oracle.getAssignment(bob));
    }

    @Test
    public void testAssignmentEqualitySubIsA() {
        KnowledgeDatabase kd = new KnowledgeDatabase();
        Concept car = kd.newConcept("Car");
        Concept feature = kd.newConcept("Feature");
        Concept cost = kd.newConcept("Cost");
        Concept breaks = kd.newConcept("Breaks");
        Concept horn = kd.newConcept("Horn");
        Concept wheel = kd.newConcept("Wheel");

        kd.newIsA(breaks, feature);
        kd.newIsA(horn, feature);
        kd.newIsA(wheel, feature);

        kd.newHasA(feature, cost);
        kd.newHasA(car, breaks);
        kd.newHasA(car, horn);
        kd.newHasA(car, wheel);

        kd.newAssignment(new Path(breaks, cost), 2);
        kd.newAssignment(new Path(horn, cost), 1);
        kd.newEquality(new Path(car, breaks, cost), new Path(car, wheel, cost));

        Oracle oracle = kd.oracle();

        assertEquals(Domains.enumDomain(1, 2), oracle.getAssignment(feature, cost));
        assertEquals(Domains.constantDomain(2), oracle.getAssignment(breaks, cost));
        assertEquals(Domains.constantDomain(1), oracle.getAssignment(horn, cost));
        assertEquals(Domains.constantDomain(2), oracle.getAssignment(wheel, cost));
    }

    @Test
    public void testAssignmentEqualityUnknown() {
        KnowledgeDatabase kd = new KnowledgeDatabase();
        Concept party = kd.newConcept("Party");
        Concept alice = kd.newConcept("Alice");
        Concept bob = kd.newConcept("Bob");

        kd.newHasA(party, alice);
        kd.newHasA(party, bob);

        kd.newEquality(new Path(party, alice), new Path(party, bob));

        Oracle oracle = kd.oracle();

        assertNull(oracle.getAssignment(alice));
        assertNull(oracle.getAssignment(bob));
    }

    @Test
    public void testAssignmentSupIsA() {
        KnowledgeDatabase kd = new KnowledgeDatabase();
        Concept colour = kd.newConcept("Colour");
        Concept primary = kd.newConcept("Primary");
        Concept red = kd.newConcept("Red");
        Concept blue = kd.newConcept("Blue");
        Concept orange = kd.newConcept("Orange");
        Concept intensity = kd.newConcept("Intensity");

        kd.newIsA(primary, colour);
        kd.newIsA(red, primary);
        kd.newIsA(blue, primary);
        kd.newIsA(orange, colour);

        kd.newHasA(colour, intensity);

        kd.newAssignment(new Path(primary, intensity), 1);
        kd.newAssignment(new Path(colour, intensity), Domains.enumDomain(1, 2));

        Oracle oracle = kd.oracle();

        assertEquals(Domains.enumDomain(1, 2), oracle.getAssignment(colour, intensity));
        assertEquals(Domains.enumDomain(1), oracle.getAssignment(primary, intensity));
        assertEquals(Domains.constantDomain(1), oracle.getAssignment(red, intensity));
        assertEquals(Domains.constantDomain(1), oracle.getAssignment(blue, intensity));
        assertEquals(Domains.enumDomain(1, 2), oracle.getAssignment(orange, intensity));
        assertEquals(Domains.enumDomain(1, 2), oracle.getAssignment(intensity));
    }

    @Test
    public void testAssignmentGeneralIsA() {
        KnowledgeDatabase kd = new KnowledgeDatabase();
        Concept colour = kd.newConcept("Colour");
        Concept primary = kd.newConcept("Primary");
        Concept red = kd.newConcept("Red");
        Concept blue = kd.newConcept("Blue");
        Concept orange = kd.newConcept("Orange");
        Concept intensity = kd.newConcept("Intensity");

        kd.newIsA(primary, colour);
        kd.newIsA(red, primary);
        kd.newIsA(blue, primary);
        kd.newIsA(orange, colour);

        kd.newHasA(colour, intensity);

        kd.newAssignment(new Path(primary, intensity), 1);
        // Specify that all intensity is 1 or 2.
        kd.newAssignment(new Path(intensity), Domains.enumDomain(1, 2));

        Oracle oracle = kd.oracle();

        // Should infer that colour.intensity is constrained by the above equation.
        assertEquals(Domains.enumDomain(1, 2), oracle.getAssignment(colour, intensity));
        assertEquals(Domains.enumDomain(1), oracle.getAssignment(primary, intensity));
        assertEquals(Domains.constantDomain(1), oracle.getAssignment(red, intensity));
        assertEquals(Domains.constantDomain(1), oracle.getAssignment(blue, intensity));
        assertEquals(Domains.enumDomain(1, 2), oracle.getAssignment(orange, intensity));
        assertEquals(Domains.enumDomain(1, 2), oracle.getAssignment(intensity));
    }

    @Test
    public void testTaxonomy() {
        KnowledgeDatabase kd = new KnowledgeDatabase();
        Concept id = kd.newConcept("Id");
        Concept animal = kd.newConcept("Animal");
        Concept plant = kd.newConcept("Plant");
        Concept mammal = kd.newConcept("Mammal");
        Concept dog = kd.newConcept("Dog");
        Concept human = kd.newConcept("Human");
        Concept idCard = kd.newConcept("IdCard");
        Concept bird = kd.newConcept("Bird");
        Concept robin = kd.newConcept("Robin");
        Concept sparrow = kd.newConcept("Sparrow");
        Concept nonOwner = kd.newConcept("NonOwner");
        Concept owner = kd.newConcept("Owner");
        Concept pet = kd.newConcept("Pet");
        Concept bed = kd.newConcept("Bed");

        kd.newIsA(mammal, animal);
        kd.newIsA(dog, mammal);
        kd.newIsA(human, mammal);
        kd.newIsA(bird, animal);
        kd.newIsA(robin, bird);
        kd.newIsA(sparrow, bird);
        kd.newIsA(nonOwner, human);
        kd.newIsA(owner, human);
        kd.newIsA(pet, animal);

        kd.newHasA(animal, id);
        kd.newHasA(plant, id);
        kd.newHasA(human, idCard);
        kd.newHasA(owner, pet);
        kd.newHasA(pet, owner);
        kd.newHasA(human, bed);
        kd.newHasA(pet, bed);

        kd.newAssignment(new Path(dog, id), Domains.constantDomain(4));
        kd.newAssignment(new Path(human, id), Domains.boundDomain(1, 2));
        kd.newAssignment(new Path(bird, id), Domains.constantDomain(3));
        kd.newAssignment(new Path(owner, id), Domains.constantDomain(1));

        kd.newEquality(new Path(human, id), new Path(human, idCard));

        Oracle oracle = kd.oracle();

        assertTrue(oracle.isA(mammal, animal));
        assertTrue(oracle.isA(dog, animal));
        assertTrue(oracle.isA(human, animal));
        assertTrue(oracle.isA(bird, animal));
        assertTrue(oracle.isA(sparrow, animal));
        assertTrue(oracle.isA(robin, animal));
        assertTrue(oracle.isA(nonOwner, animal));
        assertTrue(oracle.isA(owner, animal));
        assertTrue(oracle.isA(pet, animal));

        assertTrue(oracle.isA(dog, mammal));
        assertTrue(oracle.isA(human, mammal));
        assertTrue(oracle.isA(nonOwner, mammal));
        assertTrue(oracle.isA(owner, mammal));

        assertTrue(oracle.isA(nonOwner, human));
        assertTrue(oracle.isA(owner, human));

        assertTrue(oracle.isA(robin, bird));
        assertTrue(oracle.isA(sparrow, bird));

        assertTrue(oracle.hasA(animal, id));
        assertTrue(oracle.hasA(mammal, id));
        assertTrue(oracle.hasA(dog, id));
        assertTrue(oracle.hasA(human, id));
        assertTrue(oracle.hasA(bird, id));
        assertTrue(oracle.hasA(robin, id));
        assertTrue(oracle.hasA(sparrow, id));
        assertTrue(oracle.hasA(nonOwner, id));
        assertTrue(oracle.hasA(owner, id));
        assertTrue(oracle.hasA(pet, id));
        assertTrue(oracle.hasA(bird, id));
        assertTrue(oracle.hasA(plant, id));

        assertTrue(oracle.hasA(owner, pet));

        assertTrue(oracle.hasA(pet, owner));

        assertTrue(oracle.hasA(human, bed));
        assertTrue(oracle.hasA(nonOwner, bed));
        assertTrue(oracle.hasA(owner, bed));
        assertTrue(oracle.hasA(pet, bed));

        assertEquals(Domains.enumDomain(1, 2, 4), oracle.getAssignment(new Path(mammal, id)));
        assertNull(oracle.getAssignment(new Path(animal, id)));
        assertEquals(Domains.enumDomain(1), oracle.getAssignment(new Path(owner, idCard)));
        assertEquals(Domains.enumDomain(1, 2), oracle.getAssignment(new Path(human, idCard)));
    }

    @Test
    public void testEqualitySubIsASubIsA() {
        KnowledgeDatabase db = new KnowledgeDatabase();
        Concept animal = db.newConcept("Animal");
        Concept human = db.newConcept("Human");
        Concept raven = db.newConcept("Raven");
        Concept habitat = db.newConcept("Habitat");
        Concept house = db.newConcept("House");
        Concept nest = db.newConcept("Nest");
        Concept cost = db.newConcept("Cost");
        Concept size = db.newConcept("Size");

        db.newIsA(human, animal);
        db.newIsA(raven, animal);
        db.newIsA(house, habitat);
        db.newIsA(nest, habitat);
        db.newHasA(animal, house);
        db.newHasA(house, cost);
        db.newHasA(house, size);

        db.newAssignment(new Path(human, house, size), 5);
        db.newAssignment(new Path(human, nest, size), 0);
        db.newAssignment(new Path(raven, house, size), 0);
        db.newAssignment(new Path(raven, nest, size), 1);
        db.newEquality(new Path(animal, habitat, cost), new Path(animal, habitat, size));

        Oracle o = db.oracle();

        assertEquals(Domains.enumDomain(0, 1, 5), o.getAssignment(animal, habitat, cost));
        // A human's habitat is either 0 or 5. Cannot know that a human's habitat is not a nest.
        assertEquals(Domains.enumDomain(0, 5), o.getAssignment(human, house, cost));
        assertEquals(Domains.enumDomain(0, 1), o.getAssignment(raven, nest, cost));
    }
}
