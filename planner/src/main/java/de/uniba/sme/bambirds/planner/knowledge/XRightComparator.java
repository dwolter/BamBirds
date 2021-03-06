package de.uniba.sme.bambirds.planner.knowledge;

import java.util.Comparator;

import de.uniba.sme.bambirds.common.objects.ab.ABObject;

/**
 * <p>
 * Compare two {@link ABObject}s by their position on the x-axis using the
 * rightmost x-coordinate of the {@link ABObject}.
 * </p>
 * <p>
 * Note: {@link XComparator} uses the upper-left corner of the {@link ABObject}
 * and is thus not equivalent to this.
 * </p>
 */
public class XRightComparator implements Comparator<ABObject> {
	public int compare(ABObject a, ABObject b) {
		return Double.compare(b.getMaxX(), a.getMaxX());
	}
}
