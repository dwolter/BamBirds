package de.uniba.sme.bambirds.feedback;

import java.util.ArrayList;
import java.util.List;

import de.uniba.sme.bambirds.common.objects.ab.ABObject;

/**
 * Data-structure for managing a list of objects ({@link de.uniba.sme.bambirds.common.objects.ab.ABObject}) and infering a
 * sequenceType {@link de.uniba.sme.bambirds.feedback.SequenceType}.
 * 
 * @see de.uniba.sme.bambirds.common.objects.ab.ABObject
 * @see de.uniba.sme.bambirds.feedback.SequenceType
 */
public class SequenceObject{
    private int globalId;
    private SequenceType type;
    private List<ABObject> objectList;
    private List<ABObjectStatus> statusList;
    private ABObject lastObject;

    /**
     * Can be initiated with an object. Results in the same as adding the same object to a newly created instance.
     * @param object Object to be added as the first in the sequence.
     * @throws NullPointerException If object is null.
     */
    public SequenceObject(ABObject object) {
        if(object == null){
            throw new NullPointerException("SequenceObject: The first object can not be unknown."); 
        }
        this.type = SequenceType.UNMOVED;
        this.globalId = object.getId();
        this.statusList = new ArrayList<>();
        this.statusList.add(ABObjectStatus.KNOWN);
        this.objectList = new ArrayList<>();
        this.objectList.add(object);
        this.lastObject = object;
    }

    public SequenceObject(){
        this.type = SequenceType.MOVED;
        this.globalId = -1;
        this.statusList = new ArrayList<>();
        this.objectList = new ArrayList<>();
        this.lastObject = null;
    }
    /**
     * Adds an object to the end of the sequence. If only one object is added the type of the sequence is considered [unmoved],
     * if more than one object is added, it is [moved]. If the last object is null, the sequence type is [destroyed].
     * @param object Object to be added.
     * @throws NullPointerException If the first object to be added to the sequence is null.
     */
    public void addObject(ABObject object){
        ABObjectStatus status;
        if(object == null){
            if(objectList.isEmpty()){
                throw new NullPointerException("SequenceObject: The first object can not be unknown.");
            }else{
                status = ABObjectStatus.HIDDEN;
                type = SequenceType.DESTROYED;
            }
        }else{
            status = ABObjectStatus.KNOWN;
            if(objectList.isEmpty()){
                globalId = object.getId();
                type = SequenceType.UNMOVED;
            }else{
                type = SequenceType.MOVED;
            }
            lastObject = object;
        }
        objectList.add(object);
        statusList.add(status);
    }
    /**
     * Changes the object at a position to a new object. If the new object changes the end of the sequence to null, the 
     * sequence type is changed to [destroyed].
     * @param position The position at which the new object replaces the old one.
     * @param newObject New object to replace the old one.
     * @throws ArrayIndexOutOfBoundsException If the position value is not between zero and the last index of the list.
     * @throws NullPointerException If the first object in the sequence is changed to null.
     */
    public void changeObjectAtPosition(int position, ABObject newObject){
        ABObjectStatus newStatus;
        int lastArrayIndex = objectList.size()-1;
        if(position < 0 || position > lastArrayIndex){
            throw new ArrayIndexOutOfBoundsException("SequenceObject: Position is not accessible.");
        }
        if(newObject == null){
            newStatus = ABObjectStatus.HIDDEN;
            if(position == 0){
                throw new NullPointerException("SequenceObject: The first object can not be unknown.");
            }
            if(position == lastArrayIndex){
                type = SequenceType.DESTROYED;
            }
        }else{
            newStatus = ABObjectStatus.KNOWN;
            if(position == 0){
                globalId = newObject.getId();
            }
            if(position == lastArrayIndex){
                lastObject = newObject;
            }
        }
        objectList.set(position, newObject);
        statusList.set(position, newStatus);
    }

    public SequenceType getSequenceType(){
        return type;
    }

    public List<ABObject> getObjectList() {
        return objectList;
    }

    public List<ABObjectStatus> getStatusList() {
        return statusList;
    }

    public ABObject getLastObject(){
        return lastObject;
    }  

    public int getGlobalID(){
        return globalId;
    }

    @Override
    public String toString() {
        String lastObjectString = String.format("ABObject[id=%d, x=%.2f, y=%.2f, width=%.2f, height=%.2f, angle=%.2f, type=%s]",
        lastObject.getId(),lastObject.getCenterX(),lastObject.getCenterY(),lastObject.getWidth(),lastObject.getHeight(),lastObject.getAngle(), lastObject.getType());
        
        String objectListString = "\n";

        for(ABObject object : objectList){
            objectListString += ABObjectHelper.writeABObjectAsString(object);
        }

        return "SequenceObject [globalId=" + globalId + ", lastObject=" + lastObjectString + ", objectList=" + objectListString 
        + ", statusList=" + statusList + ", type=" + type + "]";
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + globalId;
        result = prime * result + ((lastObject == null) ? 0 : lastObject.hashCode());
        result = prime * result + ((objectList == null) ? 0 : objectList.hashCode());
        result = prime * result + ((statusList == null) ? 0 : statusList.hashCode());
        result = prime * result + ((type == null) ? 0 : type.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        SequenceObject other = (SequenceObject) obj;
        if (globalId != other.globalId)
            return false;
        if (lastObject == null) {
            if (other.lastObject != null)
                return false;
        } else if (!lastObject.equals(other.lastObject))
            return false;
        if (objectList == null) {
            if (other.objectList != null)
                return false;
        } else if (!objectList.equals(other.objectList))
            return false;
        if (statusList == null) {
            if (other.statusList != null)
                return false;
        } else if (!statusList.equals(other.statusList))
            return false;
        if (type != other.type)
            return false;
        return true;
    }
}