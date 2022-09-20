package de.uniba.sme.bambirds.feedback;
/**
 * Type of a sequence (@link de.uniba.sme.bambirds.feedback.SequenceObject). If only one object 
 * (@link de.uniba.sme.bambirds.common.objects.ab.ABObject) in a sequence it is [unmoved], if more it is [moved]. Is the last 
 * object null the type is [destroyed].
 * @see de.uniba.sme.bambirds.feedback.SequenceObject
 * @see de.uniba.sme.bambirds.common.objects.ab.ABObject
 */
public enum SequenceType {
    UNMOVED, MOVED, DESTROYED
}