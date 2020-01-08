package knowledge;

import ab.vision.ABObject;

import java.util.Comparator;

/**
 * Compares {@link ABObject}s using the y-coordinates of the upper-left corner of the {@link ABObject}. This enables sorting
 * from top to bottom (or from bottom to top respectively).
 */
public class YComparator implements Comparator<ABObject> {

	public int compare(ABObject a, ABObject b) {
		return Integer.compare(a.y, b.y);
	}
}
