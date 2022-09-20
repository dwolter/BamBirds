package de.uniba.sme.bambirds.feedback;

import de.uniba.sme.bambirds.common.objects.Timer;
import de.uniba.sme.bambirds.common.objects.ab.ABObject;
import de.uniba.sme.bambirds.common.objects.ab.ABType;
import de.uniba.sme.bambirds.common.utils.VisualDebugger;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.PriorityQueue;
import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Collectors;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * Class for calculating sequences({@link de.uniba.sme.bambirds.feedback.SequenceObject}) of objects 
 * ({@link de.uniba.sme.bambirds.common.objects.ab.ABObject}) given a list of scenes ({@link de.uniba.sme.bambirds.feedback.FeedbackScene}). 
 * A sequence contains a list of the objects that form a consistent movement throughout the scenes. Sequences are sorted in the 
 * three lists [unmoved], [moved] and [destroyed] which can be accessed via {@link #getUnmovedObjects()}, {@link #getMovedObjects()} 
 * and {@link #getDestroyedObjects()}. [Unmoved] sequences do not change their position, [moved] sequences do change their position 
 * and [destroyed] sequences can not be found at least in the last scene. <br> <br>
 * 
 * Requirement: The first scene shall be taken before any object moves, the last scene shall be taken when no object is 
 * moving anymore.
 * 
 * @see de.uniba.sme.bambirds.feedback.SequenceObject
 * @see de.uniba.sme.bambirds.feedback.FeedbackScene
 * @see de.uniba.sme.bambirds.common.objects.ab.ABObject
 *
 * @author uni (original idea), pascal (redesign & implementation)
 */
public class SequenceCalculator {
    private static final Logger log = LogManager.getLogger(SequenceCalculator.class);
    private static VisualDebugger DBG = new VisualDebugger("SequenceCalculator"); { DBG.enableDebug(false, false);}

    private static final double THRESHOLD = 2.0; // threshold an object can vary by a sidelength and is still considered similar


    /** All possible Types checked for */
    private static final ABType[] TYPES = new ABType[]{ ABType.Pig, ABType.Ice, ABType.Wood, ABType.Stone, ABType.TNT };

    private final List<FeedbackScene> sceneList;
    private final List<SequenceObject> unmovedObjects;
    private final List<SequenceObject> movedObjects;
    private final List<SequenceObject> destroyedObjects;
    private final int sceneAmount;

    /**
     * Initializes only with a required list of scenes. The amount has to be at least two to fulfill the class requirement.
     * @param sceneList List of scenes with objects from which to find sequences.
     * @throws NullPointerException If list of scenes is null.
     * @throws IllegalArgumentException If there is less than two scenes in the list.
     */
    public SequenceCalculator(List<FeedbackScene> sceneList){
        if(sceneList == null){
            throw new NullPointerException("FeedbackController: SceneList can not be null.");
        }
        if(sceneList.size() < 2){
            throw new IllegalArgumentException("FeedbackController: At least first and last scene of shot expected.");
        }
        this.sceneList = sceneList;
        this.unmovedObjects = new ArrayList<>();
        this.movedObjects = new ArrayList<>();
        this.destroyedObjects = new ArrayList<>();
        this.sceneAmount = sceneList.size();
    }

    /**
     * Starts the calculation of sequences from the given scenes. After an successful completion of this method {@link #getUnmovedObjects()}, 
     * {@link #getMovedObjects()} and {@link #getDestroyedObjects()} can be accessed with the estimated sequences from the
     * previously given scenes.
     * @return {code True} if calculation is successful, {@code false} otherwise.
     */
    public boolean calculate(){
        Timer timer = new Timer();
        boolean isSuccessful = true;
        List<ABObject> notedMovedObjects = new ArrayList<>();
        ComparisonStructure[]structures = new ComparisonStructure[sceneAmount];
        List<List<ABObject>> objectsOfSceneList = new ArrayList<>();
        List<List<SequenceObject>> notedUnmovedUntilScene = new ArrayList<>();
        for(FeedbackScene scene : sceneList){
            objectsOfSceneList.add(scene.getObjects());
        }
        for(int i=0; i < sceneAmount; i++){
            notedUnmovedUntilScene.add(new ArrayList<>());
        }
        /* Objects that did not move from the first to last scene are added to the [unmoved] list. Objects that did not move 
        until a certain scene and then got moved or destroyed are added to [notedMovedObjects] while their partially complete
        sequence is added to [notedUnmovedUntilScene]. */
        filterOutUnmovedObjects(objectsOfSceneList, notedUnmovedUntilScene, notedMovedObjects);
        /* From the [notedMovedObjects] an initial set of objects is created. An object that exists in the first scene should exist in 
        the following scenes or be destroyed. Additional objects that are new are considered errors in vision, so the amount of valid
        objects decreases over the scenes. Therefore clusters of similar objects are created from the initial set, propagated
        trough all scenes and managed in the array of [structures] which corresponds to the list of scenes. */
        createStructuresForComparing(objectsOfSceneList, notedUnmovedUntilScene, structures, notedMovedObjects);
        /* The [structures] are compared in order to each other to find a consistent sequence. For each two structures all equivalent
        clusters are being compared. If a single sequence has a correspondent object it is added, if not than it counts as hidden. If
        there are more than one sequences and objects the best fit has to estimated based on distance. */
        compareStructures(structures);
        /* The sequences are sorted in [moved] and [destroyed] depending on their last object. */
        sortInGroups(structures[sceneAmount-1]);

        /* debugging information */
        log.info("took " +timer.getElapsedTime()+"ms");
        if(DBG.visualOutputEnabled){
            SequenceCalculatorDebugging.drawSequencesOnImages(sceneList, unmovedObjects, movedObjects, destroyedObjects);
        }
        if(DBG.fileOutputEnabled){
            SequenceCalculatorDebugging.printSequences(unmovedObjects, movedObjects, destroyedObjects);
            SequenceCalculatorDebugging.printObjects(sceneList);
        }
        return isSuccessful;
    }

    private void filterOutUnmovedObjects(List<List<ABObject>> objectsOfSceneList, List<List<SequenceObject>> notedUnmovedUntilScene, 
    List<ABObject> notedMovedObjects) {
        /* create HashSets for a fast comparison */
        Set<ABObjectComparable> initialObjects = new TreeSet<>();
        Set<ABObjectComparable> notedObjects = new TreeSet<>();
        /* fill initialObjects set with quasi-identifier (x,y,angle) versions of the objects */
        for(ABObject object : objectsOfSceneList.get(0)){
            initialObjects.add(new ABObjectComparable(object));
        }
        /* compare every scene with the first one */
        for(int i = sceneAmount-1; i >= 0; i--){
            /* redundant unmoved objects that have to be removed to just count them once */
            List<ABObject> removedObjectsAfterIteration = new ArrayList<>();
            for(ABObject object : objectsOfSceneList.get(i)){
                ABObjectComparable objectAsComparable = new ABObjectComparable(object);
                if(notedObjects.contains(objectAsComparable)){
                    removedObjectsAfterIteration.add(object);
                }else{
                    if(i == 0){
                        notedUnmovedUntilScene.get(0).add(new SequenceObject(object));
                        notedMovedObjects.add(object);
                    }else if(initialObjects.contains(objectAsComparable)){
                        if(i == sceneAmount-1){
                            /* objects that didn't move from the first to last scene considered as finished sequence */
                            unmovedObjects.add(new SequenceObject(object));
                            notedObjects.add(objectAsComparable);
                            removedObjectsAfterIteration.add(object);
                        }else{
                            /* objects that didn't move until an arbitrary scene considered not as finished sequence */
                            notedUnmovedUntilScene.get(i).add(new SequenceObject(object));
                            notedObjects.add(objectAsComparable);
                            notedMovedObjects.add(object);
                            removedObjectsAfterIteration.add(object);
                        }
                    }
                }
            }
            objectsOfSceneList.get(i).removeAll(removedObjectsAfterIteration);
        }
    }

    private void createStructuresForComparing(List<List<ABObject>> objectsOfSceneList, List<List<SequenceObject>> notedUnmovedUntilScene, 
    ComparisonStructure[]structures, List<ABObject> notedMovedObjects){
        Map<ABType, List<Double>> splitValuesOfTypeList = new HashMap<>();
        for(ABType type : TYPES){
            splitValuesOfTypeList.put(type, new ArrayList<>());
            /* All moved objects from the initial scene are sorted into a specific list according to their type, sorted by maximum sidelength. */
            List<ABObject> currentTypeList = notedMovedObjects.stream().filter(object -> object.getType() == type)
            .sorted(Comparator.comparingDouble(ABObjectHelper::getMaxSidelength)).collect(Collectors.toList());

            if(!currentTypeList.isEmpty()){
                addSplitValuesToList(currentTypeList,splitValuesOfTypeList.get(type));
            }
        }
        if(DBG.fileOutputEnabled){
            SequenceCalculatorDebugging.printSplitValues(splitValuesOfTypeList);
        }
        for(int s=0;s<sceneAmount;s++){
            /* The same splitValues are used for each structure per scene but each scene adds their own objects.
            Therefore it is possible to compare the same clusters of objects for each scene. */
            structures[s] = new ComparisonStructure();
            for(ABType type : TYPES){
                structures[s].addSplitValues(type, splitValuesOfTypeList.get(type));
            }
            for(SequenceObject sequence : notedUnmovedUntilScene.get(s)){
                structures[s].addSequence(sequence);
            }
            if(s != 0){
                for(ABObject object : objectsOfSceneList.get(s)){
                    structures[s].addObject(object);
                }
            }
        }
    }

    /* Comparing difference in maximum sidelength to create an efficient data-structure. A split occurs when the difference between 
    two ordered objects is over a certain threshold, therefore clusters all objects together that have a certain maximum sidelength 
    similar to each other. Additional splits at the beginning and end are calculated to sort objects out that were not known 
    at start but wrongfully occured later. */
    private void addSplitValuesToList(List<ABObject> currentTypeList, List<Double> splitValues) {
        double lastValue = ABObjectHelper.getMaxSidelength(currentTypeList.get(0));
        double firstSplit = lastValue-THRESHOLD;
        splitValues.add((firstSplit > 0)?firstSplit:0);
        for(ABObject object : currentTypeList){
            double difference = ABObjectHelper.getMaxSidelength(object) - lastValue;
            if(difference > THRESHOLD*2){
                splitValues.add(lastValue + (difference)/2);
            }
            lastValue = ABObjectHelper.getMaxSidelength(object);
        }
        splitValues.add(lastValue+THRESHOLD);
    }

    private void compareStructures(ComparisonStructure[]structures){
        for(int s=0;s<sceneAmount-1;s++){
            /* for every scene */
            for(ABType t : TYPES){
                /* for every type */
                List<List<SequenceObject>> sequenceClusterList = structures[s].getSequenceTypeList(t);
                List<List<ABObject>> objectClusterList = structures[s+1].getObjectTypeList(t);
                for(int c=0;c < sequenceClusterList.size();c++){
                    /* for every cluster */
                    List<SequenceObject> sequenceList = sequenceClusterList.get(c);
                    List<ABObject> objectList = objectClusterList.get(c);
                    if(!sequenceList.isEmpty() && objectList.isEmpty()){
                        /* all objects are hidden in current comparison */
                        for(SequenceObject sequence : sequenceList){
                            sequence.addObject(null);
                            structures[s+1].addSequence(sequence);
                        }
                    }else if(sequenceList.size() == 1 && objectList.size() == 1){
                        /* equivalent object: high certainty that same object */
                        SequenceObject sequence = sequenceList.get(0);
                        ABObject object = objectList.get(0);
                        if(getMinSidelengthDifference(sequence, object) < THRESHOLD){
                            sequence.addObject(object);
                        }else{
                            sequence.addObject(null);
                        }
                        structures[s+1].addSequence(sequence);
                    }else if(sequenceList.size() >= 1 && objectList.size() >= 1){
                        /* multiple sequences and objects: most error-prone scenario */
                        List<SequenceObject> updatedSequences = findBestFit(sequenceList, objectList);
                        for(SequenceObject sequence : updatedSequences){
                            structures[s+1].addSequence(sequence);
                        }
                    }
                }
            }
        }
    }

    private class HeapElement{
        private final double distance;
        private final SequenceObject sequence;
        private final ABObject object;
        private HeapElement(SequenceObject sequence, ABObject object){
            this.sequence = sequence;
            this.object = object;
            this.distance = calculateVectorDistance(sequence.getLastObject(), object);
        }
        private double calculateVectorDistance(ABObject first, ABObject second) {
             /* euclidean distance */
            return Math.sqrt(Math.pow(first.getCenterX() - second.getCenterX(), 2)
                + Math.pow(first.getCenterY() - second.getCenterY(), 2));
        }
        public double getDistance(){
            return distance;
        }

        public SequenceObject getSequence() {
            return sequence;
        }

        public ABObject getObject() {
            return object;
        }
    }

    private double getMinSidelengthDifference(SequenceObject sequence, ABObject object){
        double sequenceMin = ABObjectHelper.getMinSidelength(sequence.getObjectList().get(0));
        double objectMin = ABObjectHelper.getMinSidelength(object);
        return (double) Math.floorDiv((long)sequenceMin, (long)objectMin);
    }

    private List<SequenceObject> findBestFit(List<SequenceObject> sequenceList, List<ABObject> objectList) {
        List<SequenceObject> updatedSequences = new ArrayList<>();
        int length = sequenceList.size()*objectList.size();
        /* Create minHeap to easily access the minimum distanced sequence and object elements. */
        PriorityQueue<HeapElement> minHeap = new PriorityQueue<HeapElement>(length,Comparator.comparing(HeapElement::getDistance));
        /* Create every possible arrangement and add to minHeap. */
        for(SequenceObject sequence : sequenceList){
            for(ABObject object : objectList){
                if(getMinSidelengthDifference(sequence, object) < THRESHOLD){
                    HeapElement heapElem = new HeapElement(sequence, object);
                    minHeap.add(heapElem);
                }
            }
        }
        /* Create sets for easy access. */
        Set<SequenceObject> notedSequences = new HashSet<SequenceObject>();
        Set<ABObject> notedObjects = new HashSet<ABObject>();
        HeapElement currentMin = null;
        while(!sequenceList.isEmpty() && !objectList.isEmpty()){
            /* Find a sequence and an object with minimum distance that are each not already used. */
            currentMin = minHeap.poll();
            if(currentMin == null){
                break;
            }
            SequenceObject sequence = currentMin.getSequence();
            ABObject object = currentMin.getObject();
            while(notedSequences.contains(sequence) || notedObjects.contains(object)){
                currentMin = minHeap.poll();
                if(currentMin == null){
                    break;
                }
                sequence = currentMin.getSequence();
                object = currentMin.getObject();
            }
            if(currentMin == null){
                break;
            }
            notedSequences.add(sequence);
            notedObjects.add(object);
            sequenceList.remove(sequence);
            objectList.remove(object);
            /* Prevent additional modification by copying. */
            SequenceObject sequenceCopy = getCopy(sequence);
            sequenceCopy.addObject(object);
            updatedSequences.add(sequenceCopy);
        }
        if(!sequenceList.isEmpty() && (objectList.isEmpty() || currentMin == null)){
            /* If not objects or heapElements left, but sequences then the objects count as hidden. */
            for(SequenceObject sequence : sequenceList){
                sequence.addObject(null);
                updatedSequences.add(sequence);
            }
        }
        return updatedSequences;
    }

    private SequenceObject getCopy(SequenceObject sequence){
        SequenceObject sequenceCopy = new SequenceObject();
        for(ABObject obj : sequence.getObjectList()){
            sequenceCopy.addObject(obj);
        }
        return sequenceCopy;
    }

    private void sortInGroups(ComparisonStructure comparisonElement) {
        for(ABType t : TYPES){
            for(List<SequenceObject> sequenceList : comparisonElement.getSequenceTypeList(t)){
                for(SequenceObject sequence : sequenceList){
                    if(sequence.getSequenceType() == SequenceType.DESTROYED){
                        destroyedObjects.add(sequence);
                    }else{
                        movedObjects.add(sequence);
                    }
                }
            }
        }
    }
    
    /**
     * The list of sequences whith objects that did not move from the first to last scene.
     * @return List of sequences.
     */
    public List<SequenceObject> getUnmovedObjects() {
        return unmovedObjects;
    }
    /**
     * The list of sequences whith objects did move over the first to the last scene.
     * @return List of sequences.
     */
    public List<SequenceObject> getMovedObjects() {
        return movedObjects;
    }
    /**
     * The list of sequences of which the last object is hidden or destroyed.
     * @return List of sequences.
     */
    public List<SequenceObject> getDestroyedObjects() {
        return destroyedObjects;
    }
}
