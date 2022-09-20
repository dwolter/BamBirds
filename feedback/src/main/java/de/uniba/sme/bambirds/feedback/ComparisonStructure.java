package de.uniba.sme.bambirds.feedback;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import de.uniba.sme.bambirds.common.objects.ab.ABObject;
import de.uniba.sme.bambirds.common.objects.ab.ABType;

/**
 * A structure to handle clustering of sequences({@link de.uniba.sme.bambirds.feedback.SequenceObject}) and objects 
 * ({@link de.uniba.sme.bambirds.common.objects.ab.ABObject}) for a specific type ({@link de.uniba.sme.bambirds.common.objects.ab.ABType}) 
 * given the values to split the clusters via {@link #addSplitValues(ABType, List)}.
 * 
 * @see de.uniba.sme.bambirds.feedback.SequenceObject
 * @see de.uniba.sme.bambirds.common.objects.ab.ABObject
 * @see de.uniba.sme.bambirds.common.objects.ab.ABType
 */
public class ComparisonStructure {
    /* ABTYPE Ids: Pig: 9, Ice: 10, Wood: 11, Stone: 12 */
    private static final int MIN_TYPE = 9;
    private static final int TYPE_AMOUNT = 4;
    private final Map<ABType, List<List<ABObject>>> objectTypeList;
    private final Map<ABType, List<List<SequenceObject>>> sequenceTypeList;
    private final Map<ABType, List<Double>> splitValuesOfTypeList;
    private final Map<ABType, Boolean> splitValueListGiven;

    public ComparisonStructure() {
        this.objectTypeList = new HashMap<>();
        this.sequenceTypeList = new HashMap<>();
        this.splitValuesOfTypeList = new HashMap<>();
        this.splitValueListGiven = new HashMap<>();
    }
    /**
     * Adds values for clustering to the structure. Before objects and sequences from a specific type can be sorted in their 
     * respective cluster (obtained by their maximum sidelength), the values for this clustering have to be given for the type. <br> <br>
     * 
     * Given the values this method evaluates the amount of clusters needed and initializes them as empty lists that
     * can now be filled via {@link #addObject(ABObject)} and {@link #addSequence(SequenceObject)}.
     * @param type The type of the sequences and objects that will be sorted in here. Should be one of [Pig], [Ice], [Wood], [Stone], [TNT]
     * @param splitValueList List of values that split added sequences and objects into clusters.
     * @throws NullPointerException If splitValueList is null.
     * @throws IllegalArgumentException If values are already initialized.
     */
    protected void addSplitValues(ABType type, List<Double> splitValueList) {
        if (splitValueList == null) {
            throw new NullPointerException("ComparisonElement: Values has to be not null.");
        }
        if (splitValueListGiven.getOrDefault(type, false)) {
            throw new IllegalArgumentException("ComparisonElement: Values are already initialized.");
        }
        splitValueListGiven.put(type, true);
        splitValuesOfTypeList.put(type, splitValueList);
        List<List<ABObject>> emptyObjectTypeList = new ArrayList<>();
        List<List<SequenceObject>> emptySequenceTypeList = new ArrayList<>();
        int clusterAmount = splitValueList.size() +1;
        for (int i = 0; i < clusterAmount; i++) {
            emptyObjectTypeList.add(new ArrayList<>());
            emptySequenceTypeList.add(new ArrayList<>());
        }
        sequenceTypeList.put(type, emptySequenceTypeList);
        objectTypeList.put(type, emptyObjectTypeList);
    }
    /**
     * Adds the object to the cluster it belongs to, according to type and given splitValues.
     * @param object Object to be added.
     * @throws NullPointerException If object is null.
     * @throws IllegalArgumentException If object has wrong type or {@link #addSplitValues(ABType, List)} was not called before for that type.
     */
    public void addObject(ABObject object) {
        int index = calculateIndexOfObject(object);
        if (objectTypeList.containsKey(object.getType())) {
            objectTypeList.get(object.getType()).get(index).add(object);
        } else {
            throw new IllegalArgumentException("Type not initialized yet");
        }
    }
    /**
     * Adds the sequence according to its last object to the cluster it belongs to, according to type and given splitValues.
     * @param sequence Sequence to be added.
     * @throws NullPointerException If last object of sequence is null.
     * @throws IllegalArgumentException If last object of sequence  has wrong type or {@link #addSplitValues(ABType, List)} was not called before for that type.
     */
    public void addSequence(SequenceObject sequence) {
        ABObject object = sequence.getLastObject();
        int index = calculateIndexOfObject(object);
        if (!sequenceTypeList.containsKey(object.getType())) {
            throw new IllegalArgumentException("Type not initialized yet");
        }
        sequenceTypeList.get(object.getType()).get(index).add(sequence);
    }

    private int calculateIndexOfObject(ABObject object) {
        if (object == null) {
            throw new NullPointerException("ComparisonElement: Nullpointer can not be evaluated.");
        }
        ABType type = object.getType();
        if (!splitValueListGiven.getOrDefault(type, false)) {
            throw new IllegalArgumentException(
                    "ComparisonElement: SplitValues have to be given before adding objects.");
        }
        List<Double> splits = splitValuesOfTypeList.get(type);
        double value = ABObjectHelper.getMaxSidelength(object);
        int left = 0;
        int right = splits.size();
        while (left != right) {
            int center = left + (int) (right - left) / 2;
            if (value < splits.get(center)) {
                // searched index is on left side
                right = center;
            } else {
                // searched index is on right side
                left = center + 1;
            }
        }
        return left;
    }
    /**
     * The list of all object clusters of a specific type.
     * @param type The type of the objects that are sorted in here. [Pig]:=0, [Ice]:=1, [Wood]:=2, [Stone]:=3
     * @return List of clusterLists with objects.
     */
    public List<List<ABObject>> getObjectTypeList(ABType type) {
        return objectTypeList.get(type);
    }
    /**
     * The list of all sequence clusters of a specific type.
     * @param type The type of the sequences that are sorted in here. [Pig]:=0, [Ice]:=1, [Wood]:=2, [Stone]:=3
     * @return List of clusterLists with sequences.
     */
    public List<List<SequenceObject>> getSequenceTypeList(ABType type) {
        return sequenceTypeList.get(type);
    }

    @Override
    public String toString() {
        return "ComparisonElement [objectTypeList=" + objectTypeList + ", splitValuesOfTypeList=" + splitValuesOfTypeList + ", splitValueListGiven="
                + splitValueListGiven + ", sequenceTypeList=" + sequenceTypeList + "]";
    }
}