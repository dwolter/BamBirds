package de.uniba.sme.bambirds.feedback;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;
import java.awt.Rectangle;

import de.uniba.sme.bambirds.common.objects.ab.ABObject;
import de.uniba.sme.bambirds.common.objects.ab.ABType;

public class SequenceObjectTest {

    private final ABObject object1;
    private final ABObject object2;
    private final ABObject nullObject;
    private final NullPointerException nullPointerException;
    private final ArrayIndexOutOfBoundsException arrayIndexOutOfBoundsException;

    public SequenceObjectTest(){
        this.nullPointerException = new NullPointerException("SequenceObject: The first object can not be unknown.");
        this.arrayIndexOutOfBoundsException = new ArrayIndexOutOfBoundsException("SequenceObject: Position is not accessible.");
        this.object1 = new ABObject(new Rectangle(35,2,4,17), ABType.Wood);
        this.object2 = new ABObject(new Rectangle(22,45,12,87), ABType.Ice);
        this.nullObject = null;
    }

    @Test
    public void initializationWithNullFails(){
        try{
            new SequenceObject(nullObject);
            fail("Expected NullPointerException.");
        }catch(Exception e){
            assertEquals(nullPointerException.getMessage(), e.getMessage());
        }
    }

    @Test
    public void changingFirstObjectToNullFails(){
        SequenceObject notEmptyConstructor = new SequenceObject(object1);
        try{
            notEmptyConstructor.changeObjectAtPosition(0, nullObject);
            fail("Expected NullPointerException.");
        }catch(Exception e){
            assertEquals(nullPointerException.getMessage(), e.getMessage());
        }
    }

    @Test
    public void addingNullAsFirstObjectFails(){
        SequenceObject emptyConstructor = new SequenceObject();
        try{
            emptyConstructor.addObject(nullObject);
            fail("Expected NullPointerException.");
        }catch(Exception e){
            assertEquals(nullPointerException.getMessage(), e.getMessage());
        }
    }

    @Test
    public void changingObjectAtPositionOverMaxLengthFails(){
        for(int i=1;i<11;i++){
            SequenceObject sequence = new SequenceObject(object1);
            try{
                sequence.changeObjectAtPosition(i,object2);
                fail("Expected ArrayIndexOutOfBoundsException.");
            }catch(Exception e){
                assertEquals(arrayIndexOutOfBoundsException.getMessage(), e.getMessage());
            }
        }
    }

    @Test
    public void changingObjectAtPositionUnderZeroFails(){
        for(int i=-10;i<0;i++){
            SequenceObject sequence = new SequenceObject(object1);
            try{
                sequence.changeObjectAtPosition(i,object2);
                fail("Expected ArrayIndexOutOfBoundsException.");
            }catch(Exception e){
                assertEquals(arrayIndexOutOfBoundsException.getMessage(), e.getMessage());
            }
        }
    }

    @Test
    public void addingFirstObjectIsConsistentWithConstructorInitialization(){
        SequenceObject notEmptyConstructor = new SequenceObject(object1);
        SequenceObject emptyConstructor = new SequenceObject();
        emptyConstructor.addObject(object1);
        assertEquals(notEmptyConstructor, emptyConstructor);
    }

    @Test
    public void changingFirstObjectIsConsistentWithConstructorInitialization(){
        SequenceObject notEmptyConstructor1 = new SequenceObject(object1);
        SequenceObject notEmptyConstructor2 = new SequenceObject(object2);
        notEmptyConstructor2.changeObjectAtPosition(0, object1);
        assertEquals(notEmptyConstructor1, notEmptyConstructor2);
    }

    @Test
    public void typeIsUnmovedWhenOnlyOneObject(){
        SequenceObject sequence = new SequenceObject(object1);
        assertEquals(SequenceType.UNMOVED, sequence.getSequenceType());
    }
    
    @Test
    public void typeIsMovedWhenMoreThanOneObject(){
        SequenceObject sequence = new SequenceObject();
        sequence.addObject(object1);
        sequence.addObject(object2);
        assertEquals(SequenceType.MOVED, sequence.getSequenceType());
    }

    @Test
    public void typeIsDestroyedWhenLastObjectNull(){
        SequenceObject sequence = new SequenceObject();
        sequence.addObject(object1);
        sequence.addObject(nullObject);
        assertEquals(SequenceType.DESTROYED, sequence.getSequenceType());
    }
}