package de.uniba.sme.bambirds.feedback;

import java.awt.Rectangle;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import de.uniba.sme.bambirds.common.objects.ab.ABObject;
import de.uniba.sme.bambirds.common.objects.ab.ABType;
import de.uniba.sme.bambirds.common.objects.ab.shape.Rect;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

public class ComparisonStructureTest {

    private static ABObject object;
    private static ABObject nullObject;
    private static ABObject uninitializedTypeObject;
    private static List<Double> splitValues;
    private static List<Double> nullValues;
    private static final ABType[] TYPES = new ABType[]{ ABType.Pig, ABType.Ice, ABType.Wood, ABType.Stone, ABType.TNT };
    private static ComparisonStructure initializedComparisonElement;

    public ComparisonStructureTest(){
    }

    @BeforeAll static void initialize(){
        object = new ABObject(new Rectangle(35,2,8,10), ABType.Pig);
        nullObject = null;
        uninitializedTypeObject = new ABObject(new Rectangle(22,45,12,87), ABType.Duck);
        splitValues = new ArrayList<>(Arrays.asList(80.6,82.5,85.));
        nullValues = null;
        initializedComparisonElement = new ComparisonStructure();
        for(ABType type : TYPES){
            initializedComparisonElement.addSplitValues(type, splitValues);
        }
    }

    @Test
    public void addingNullValuesFails(){
        ComparisonStructure comparisonElement = new ComparisonStructure();
        assertThrows(NullPointerException.class, () -> {
            comparisonElement.addSplitValues(TYPES[0], nullValues);
        }, "Expected NullPointerException when adding null as split values");
    }

    @Test
    public void addingSplitValuesMoreThanOnceFails(){
        assertThrows(IllegalArgumentException.class, () -> {
            initializedComparisonElement.addSplitValues(TYPES[0], splitValues);
        }, "Expected IllegalArgumentException when adding split values more than once");
    }

    @Test
    public void addingWrongTypeObjectFails(){
        assertThrows(IllegalArgumentException.class, () -> {
            initializedComparisonElement.addObject(uninitializedTypeObject);
        },"Expected IllegalArgumentException when adding object of not initialized type");
    }

    @Test
    public void addingNullObjectFails(){
        assertThrows(NullPointerException.class, () -> {
            initializedComparisonElement.addObject(nullObject);
        }, "Expected NullPointerException when adding null as object");
    }

    @Test
    public void addingObjectBeforeAddingSplitValuesFails(){
        ComparisonStructure comparisonElement = new ComparisonStructure();
        assertThrows(IllegalArgumentException.class, () -> {
            comparisonElement.addObject(object);
        }, "Expected IllegalArgumentException when adding object of not initialized type");
    }

    @Test
    public void addingSequencesIsConsistentWithAddingObjects(){
        SequenceObject sequence = new SequenceObject(object);
        initializedComparisonElement.addObject(object);
        initializedComparisonElement.addSequence(sequence);
        ABType type = object.getType();
        List<List<ABObject>> objectClusterList = initializedComparisonElement.getObjectTypeList(type);
        List<List<SequenceObject>> sequenceClusterList = initializedComparisonElement.getSequenceTypeList(type);
        assertEquals(objectClusterList.get(0).get(0), sequenceClusterList.get(0).get(0).getLastObject());
    }

    @Test
    @Disabled("Failing")
    public void correctClassificationFromCorrectInput(){
        for(double i=80.0;i<86.0;i += 0.1){
            ABObject object = new Rect(0,0,10,i/10,0,ABType.Pig);
            initializedComparisonElement.addObject(object);
            double firstSplit = splitValues.get(0);
            double secondSplit = splitValues.get(1);
            double thirdSplit = splitValues.get(2);
            ABType type = object.getType();
            List<List<ABObject>> objectLists = initializedComparisonElement.getObjectTypeList(type);
            if(i < firstSplit){
                assertTrue(objectLists.get(0).contains(object));
            }else if(i >= firstSplit && i < secondSplit){
                assertTrue(objectLists.get(1).contains(object));
            }else if(i >= secondSplit && i < thirdSplit){
                assertTrue(objectLists.get(2).contains(object));
            }else{
                assertTrue(objectLists.get(3).contains(object));
            }
        }
    }
}