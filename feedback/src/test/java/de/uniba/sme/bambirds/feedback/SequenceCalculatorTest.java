package de.uniba.sme.bambirds.feedback;

import java.awt.Color;
import java.awt.Rectangle;
import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

import de.uniba.sme.bambirds.common.objects.ab.ABObject;
import de.uniba.sme.bambirds.common.objects.ab.ABType;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;
import static org.mockito.Mockito.*;

public class SequenceCalculatorTest {

    private final NullPointerException nullPointerException;
    private final IllegalArgumentException illegalArgumentException;
    private final List<FeedbackScene> nullSceneList;
    private final BufferedImage image;
    private static final int MIN_TYPE = 9;
    private static final int MAX_TYPE = 12;
    private static final int SCENE_AMOUNT = 10;
    private static final int OBJECT_AMOUNT = 10; // per sequenceList
    private List<FeedbackScene> sceneList;
    private List<SequenceObject> unmovedObjects;
    private List<SequenceObject> movedObjects;
    private List<SequenceObject> destroyedObjects;
    private List<List<ABObject>> objectsOfSceneList;
    private ABType type;
    private SequenceObject sequence;
    private ABObject object;

    public SequenceCalculatorTest() {
        this.nullPointerException = new NullPointerException("FeedbackController: SceneList can not be null.");
        this.illegalArgumentException = new IllegalArgumentException(
                "FeedbackController: At least first and last scene of shot expected.");
        this.nullSceneList = null;
        this.image = new BufferedImage(300, 300, BufferedImage.TYPE_INT_RGB);
    }

    private ABType calculateRandomType(){
        return ABType.fromID(calculateRandomInt(MIN_TYPE, MAX_TYPE));
    }

    private int calculateRandomInt(int min, int max){
        if(max <= min){
            throw new IllegalArgumentException("FeedbackControllerTest: Min has to be lower than Max.");
        }
        int difference = (max-min)-1;
        return (int) (Math.random()*difference) + min;
    }

    private class SequenceComparator implements Comparator<SequenceObject>{
        @Override
	    public int compare(SequenceObject o1, SequenceObject o2) {
		    if(o1.getGlobalID() < o2.getGlobalID()){
                return -1;
            }else if(o1.getGlobalID() > o2.getGlobalID()){
                return 1;
            }else{
                return 0;
            }
	    }
    }

    @Test
    public void calculationWithNullReferenceFails(){
        try{
            new SequenceCalculator(nullSceneList).calculate();
            fail("Expected NullPointerException");
        }catch(Exception e){
            assertEquals(nullPointerException.getMessage(), e.getMessage());
        }
    }

    @Test
    public void calculationWithLessThanTwoScenesFail(){
        FeedbackScene scene = mock(FeedbackScene.class);
        List<FeedbackScene> sceneList = new ArrayList<>();
        for(int i=0;i < 2;i++){
            try{
                new SequenceCalculator(sceneList).calculate();
                fail("Expected IllegalArgumentException");
            }catch(Exception e){
                assertEquals(illegalArgumentException.getMessage(), e.getMessage());
            }
            sceneList.add(scene);
        }
    }

    @Test
    public void sortUnmovedObjectsCorrect(){
        for(int i=1; i <= OBJECT_AMOUNT; i++){
            initializeEmptyLists();
            for(int j=0; j < i*3;j++){
                initializeSequenceAndType();
                for(int s=0; s < SCENE_AMOUNT;s++){
                    initializeObjectsListAndObject(j);
                    if(objectIsStatus(SequenceType.UNMOVED, j)){
                        object = getObjectOfStatus(SequenceType.UNMOVED, s, j);
                        if(s == SCENE_AMOUNT-1){
                            sequence.addObject(object);
                        }
                    }
                    addObject(object, s);
                }
                addSequenceToCorrectList(sequence, j);
            }
            createSceneMocks();
            testCorrectness(sceneList);
        }
    }

    @Test
    public void sortUnmovedAndThenDestroyedObjectsCorrect(){
        for(int i=1; i <= OBJECT_AMOUNT; i++){
            initializeEmptyLists();
            for(int j=0; j < i*3;j++){
                initializeSequenceAndType();
                for(int s=0; s < SCENE_AMOUNT;s++){
                    initializeObjectsListAndObject(j);
                    if(objectIsStatus(SequenceType.DESTROYED, j)){
                        if(s == SCENE_AMOUNT-1){
                            object = getObjectOfStatus(SequenceType.DESTROYED, s, j);
                            sequence.addObject(object);
                        }else{
                            object = getObjectOfStatus(SequenceType.UNMOVED, s, j);
                            if(s == SCENE_AMOUNT-2){
                                sequence.addObject(object);
                            }
                        }
                    }
                    addObject(object, s);
                }
                addSequenceToCorrectList(sequence, j);
            }
            createSceneMocks();
            testCorrectness(sceneList);
        }
    }

    @Test
    public void sortUnmovedAndThenMovedObjectsCorrect(){
        for(int i=1; i <= OBJECT_AMOUNT; i++){
            initializeEmptyLists();
            for(int j=0; j < i*3;j++){
                initializeSequenceAndType();
                for(int s=0; s < SCENE_AMOUNT;s++){
                    initializeObjectsListAndObject(j);
                    if(objectIsStatus(SequenceType.MOVED, j)){
                        if(s == SCENE_AMOUNT-1){
                            object = getObjectOfStatus(SequenceType.MOVED, s, j);
                            sequence.addObject(object);
                        }else{
                            object = getObjectOfStatus(SequenceType.UNMOVED, s, j);
                            if(s == SCENE_AMOUNT-2){
                                sequence.addObject(object);
                            }
                        }
                    }
                    addObject(object, s);
                }
                addSequenceToCorrectList(sequence, j);
            }
            createSceneMocks();
            testCorrectness(sceneList);
        }
    }

    @Test
    public void sortMovedObjectsCorrect(){
        for(int i=1; i <= OBJECT_AMOUNT; i++){
            initializeEmptyLists();
            for(int j=0; j < i*3;j++){
                initializeSequenceAndType();
                for(int s=0; s < SCENE_AMOUNT;s++){
                    initializeObjectsListAndObject(j);
                    if(objectIsStatus(SequenceType.MOVED, j)){
                        object = getObjectOfStatus(SequenceType.MOVED, s, j);
                        sequence.addObject(object);
                    }
                    addObject(object, s);
                }
                addSequenceToCorrectList(sequence, j);
            }
            createSceneMocks();
            testCorrectness(sceneList);
        }
    }

    @Test
    public void sortMovedAndThenDestroyedObjectsCorrect(){
        for(int i=1; i <= OBJECT_AMOUNT; i++){
            initializeEmptyLists();
            for(int j=0; j < i*3;j++){
                initializeSequenceAndType();
                for(int s=0; s < SCENE_AMOUNT;s++){
                    initializeObjectsListAndObject(j);
                    if(objectIsStatus(SequenceType.DESTROYED, j)){
                        if(s == SCENE_AMOUNT-1){
                            object = getObjectOfStatus(SequenceType.DESTROYED, s, j);
                            sequence.addObject(object);
                        }else{
                            object = getObjectOfStatus(SequenceType.MOVED, s, j);
                            sequence.addObject(object);
                        }
                    }
                    addObject(object, s);
                }
                addSequenceToCorrectList(sequence, j);
            }
            createSceneMocks();
            testCorrectness(sceneList);
        }
    }

    @Test
    public void sortAllObjectsCorrect(){
        for(int i=1; i <= OBJECT_AMOUNT; i++){
            initializeEmptyLists();
            for(int j=0; j < i*3;j++){
                initializeSequenceAndType();
                for(int s=0; s < SCENE_AMOUNT;s++){
                    initializeObjectsListAndObject(j);
                    if(objectIsStatus(SequenceType.UNMOVED, j)){
                        object = getObjectOfStatus(SequenceType.UNMOVED, s, j);
                        if(s == SCENE_AMOUNT-1){
                            sequence.addObject(object);
                        }
                    }else if(objectIsStatus(SequenceType.MOVED, j)){
                        object = getObjectOfStatus(SequenceType.MOVED, s, j);
                        sequence.addObject(object);
                    }else if(objectIsStatus(SequenceType.DESTROYED, j)){
                        object = getObjectOfStatus(SequenceType.DESTROYED, s, j);
                        sequence.addObject(object);
                    }
                    addObject(object, s);
                }
                addSequenceToCorrectList(sequence, j);
            }
            createSceneMocks();
            testCorrectness(sceneList);
        }
    }

    @Test
    public void sortAllObjectsCorrectWithHiddenObjects(){
        for(int i=1; i <= OBJECT_AMOUNT; i++){
            initializeEmptyLists();
            for(int j=0; j < i*3;j++){
                initializeSequenceAndType();
                for(int s=0; s < SCENE_AMOUNT;s++){
                    initializeObjectsListAndObject(j);
                    if(s == calculateRandomInt(1, SCENE_AMOUNT-1) && !objectIsStatus(SequenceType.UNMOVED, j)){
                        /* create random hidden element */
                        object = null;
                        sequence.addObject(object);
                    }else{
                        if(objectIsStatus(SequenceType.UNMOVED, j)){
                            object = getObjectOfStatus(SequenceType.UNMOVED, s, j);
                            if(s == SCENE_AMOUNT-1){
                                sequence.addObject(object);
                            }
                        }else if(objectIsStatus(SequenceType.MOVED, j)){
                            object = getObjectOfStatus(SequenceType.MOVED, s, j);
                            sequence.addObject(object);
                        }else if(objectIsStatus(SequenceType.DESTROYED, j)){
                            object = getObjectOfStatus(SequenceType.DESTROYED, s, j);
                            sequence.addObject(object);
                        }
                    }
                    addObject(object, s);
                }
                addSequenceToCorrectList(sequence, j);
            }
            createSceneMocks();
            testCorrectness(sceneList);
        }
    }

    @Test
    public void sortAllObjectsCorrectWithAdditionalUnsimilarObjects(){
        for(int i=1; i <= OBJECT_AMOUNT; i++){
            initializeEmptyLists();
            for(int j=0; j < i*3;j++){
                initializeSequenceAndType();
                for(int s=0; s < SCENE_AMOUNT;s++){
                    initializeObjectsListAndObject(j);
                    if(s == calculateRandomInt(1, SCENE_AMOUNT-1)){
                        /* create random obstacle */
                        objectsOfSceneList.get(s).add(getObjectOfStatus(null, s, j));
                    }
                    if(objectIsStatus(SequenceType.UNMOVED, j)){
                        object = getObjectOfStatus(SequenceType.UNMOVED, s, j);
                        if(s == SCENE_AMOUNT-1){
                            sequence.addObject(object);
                        }
                    }else if(objectIsStatus(SequenceType.MOVED, j)){
                        object = getObjectOfStatus(SequenceType.MOVED, s, j);
                        sequence.addObject(object);
                    }else if(objectIsStatus(SequenceType.DESTROYED, j)){
                        object = getObjectOfStatus(SequenceType.DESTROYED, s, j);
                        sequence.addObject(object);
                    }
                    addObject(object, s);
                }
                addSequenceToCorrectList(sequence, j);
            }
            createSceneMocks();
            testCorrectness(sceneList);
        }
    }

    @Test
    public void sortAllObjectsCorrectWithAdditionalUnsimilarAndHiddenObjects(){
        for(int i=1; i <= OBJECT_AMOUNT; i++){
            initializeEmptyLists();
            for(int j=0; j < i*3;j++){
                initializeSequenceAndType();
                for(int s=0; s < SCENE_AMOUNT;s++){
                    initializeObjectsListAndObject(j);
                    if(s == calculateRandomInt(1, SCENE_AMOUNT-1)){
                        /* create random obstacle */
                        objectsOfSceneList.get(s).add(getObjectOfStatus(null, s, j));
                    }
                    if(s == calculateRandomInt(1, SCENE_AMOUNT-1) && !objectIsStatus(SequenceType.UNMOVED, j)){
                        /* create random hidden element */
                        object = null;
                        sequence.addObject(object);
                    }else{
                        if(objectIsStatus(SequenceType.UNMOVED, j)){
                            object = getObjectOfStatus(SequenceType.UNMOVED, s, j);
                            if(s == SCENE_AMOUNT-1){
                                sequence.addObject(object);
                            }
                        }else if(objectIsStatus(SequenceType.MOVED, j)){
                            object = getObjectOfStatus(SequenceType.MOVED, s, j);
                            sequence.addObject(object);
                        }else if(objectIsStatus(SequenceType.DESTROYED, j)){
                            object = getObjectOfStatus(SequenceType.DESTROYED, s, j);
                            sequence.addObject(object);
                        }
                    }
                    addObject(object, s);
                }
                addSequenceToCorrectList(sequence, j);
            }
            createSceneMocks();
            testCorrectness(sceneList);
        }
    }

    private ABObject getObjectOfStatus(SequenceType status, int s, int j){
        if(status == SequenceType.UNMOVED){
            return new ABObject(new Rectangle(j*5,j*5,10,10), type);
        }else if(status == SequenceType.MOVED){
            return new ABObject(new Rectangle(j*5+s,j*5,10,10), type);
        }else if(status == SequenceType.DESTROYED){
            return (s == SCENE_AMOUNT-1)?null:new ABObject(new Rectangle(j*5+s,j*5,10,10), type);
        }else{
            return new ABObject(new Rectangle(j*5+s,j*5,30,20), calculateRandomType());
        }
    }

    private boolean objectIsStatus(SequenceType status, int j){
        switch(status){
            case UNMOVED: return ((j % 3) == 0);
            case MOVED: return ((j % 3) == 1);
            case DESTROYED: return ((j % 3) == 2);
            default: return false;
        }
    }

    private void initializeEmptyLists(){
        sceneList = new ArrayList<>();
        unmovedObjects = new ArrayList<>();
        movedObjects = new ArrayList<>();
        destroyedObjects = new ArrayList<>();
        objectsOfSceneList = new ArrayList<>();
    }

    private void initializeSequenceAndType(){
        type = calculateRandomType();
        sequence = new SequenceObject();
    }

    private void initializeObjectsListAndObject(int j){
        if(j == 0){
            objectsOfSceneList.add(new ArrayList<>());
        }
        object = null;
    }

    private void addObject(ABObject object, int s){
        if(object != null){
            objectsOfSceneList.get(s).add(object);
        }
    }

    private void addSequenceToCorrectList(SequenceObject sequence, int j){
        if(!sequence.equals(new SequenceObject())){
            if(objectIsStatus(SequenceType.UNMOVED, j)){
                unmovedObjects.add(sequence);
            }else if(objectIsStatus(SequenceType.MOVED, j)){
                movedObjects.add(sequence);
            }else if(objectIsStatus(SequenceType.DESTROYED, j)){
                destroyedObjects.add(sequence);
            }
        }
    }

    private void createSceneMocks(){
        for(int s=0; s < SCENE_AMOUNT;s++){
            FeedbackScene scene = mock(FeedbackScene.class);
            when(scene.getObjects()).thenReturn(objectsOfSceneList.get(s));
            when(scene.getImage()).thenReturn(image);
            when(scene.drawImage(true, true, Color.RED)).thenReturn(image);
            when(scene.drawImage(true, false, Color.BLACK)).thenReturn(image);
            sceneList.add(scene);
        }
    }

    private void testCorrectness(List<FeedbackScene> sceneList){
        SequenceCalculator sequenceCalculator = new SequenceCalculator(sceneList);
        sequenceCalculator.calculate();
        List<SequenceObject> givenList = sequenceCalculator.getUnmovedObjects();
        givenList.sort(new SequenceComparator());
        assertEquals(unmovedObjects, givenList);
        givenList = sequenceCalculator.getMovedObjects();
        givenList.sort(new SequenceComparator());
        assertEquals(movedObjects, givenList);
        givenList = sequenceCalculator.getDestroyedObjects();
        givenList.sort(new SequenceComparator());
        assertEquals(destroyedObjects, givenList);
    }
}