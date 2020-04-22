package de.uniba.sme.bambirds.planner.knowledge;

import java.util.Comparator;

import de.uniba.sme.bambirds.common.objects.ab.ABObject;

/**
 * Compares {@link ABObject}s using the x-coordinates of the upper-left corner
 * of the {@link ABObject}. This enables sorting from left to right (or from
 * right to left respectively).
 */
public class XComparator implements Comparator<ABObject> {

	public int compare(ABObject a, ABObject b) {
		return Integer.compare(a.x, b.x);
	}
}
