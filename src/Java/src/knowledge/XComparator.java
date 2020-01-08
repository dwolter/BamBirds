package knowledge;

import ab.vision.ABObject;

import java.util.Comparator;

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
