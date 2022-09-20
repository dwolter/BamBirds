package de.uniba.sme.bambirds.planner.shot;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import de.uniba.sme.bambirds.common.database.AbstractScene;
import de.uniba.sme.bambirds.common.objects.ab.ABObject;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * Class for naively calculating the difference between two scenes and resulting
 */
public class SceneDifference {
	private static final Logger log = LogManager.getLogger();

	private AbstractScene beforeScene;
	private AbstractScene afterScene;

	private final List<ABObject> unchangedObjects = new ArrayList<>();
	private final List<ABObject> destroyedObjects = new ArrayList<>();
	private final List<ABObject> movedObjects = new ArrayList<>();

	public SceneDifference(AbstractScene beforeScene, AbstractScene afterScene) {
		this.beforeScene = beforeScene;
		this.afterScene = afterScene;
		calculate();
	}

	private void calculate() {

		// We are interested only in Blocks and Pigs for the cases
		List<ABObject> beforeObjects = beforeScene.getBlocks();
		beforeObjects.addAll(beforeScene.getPigs());

		List<ABObject> afterObjects = afterScene.getBlocks();
		afterObjects.addAll(afterScene.getPigs());

		for (ABObject before : beforeObjects) {
			boolean unchanged = false;
			ABObject toBeRemoved = null;
			for (ABObject after : afterObjects) {
				if (closeEnough(before, after)) {
					unchanged = true;
					// If an object is categorized as unchanged, it should be removed from both
					// lists and no more matches should be considered for this object
					toBeRemoved = after;
					break;
				}
			}
			if (unchanged) {
				unchangedObjects.add(before);
				if (toBeRemoved != null) {
					afterObjects.remove(toBeRemoved);
				}
			}
		}

		beforeObjects.removeAll(unchangedObjects);

		for (ABObject before : beforeObjects) {
			boolean destroyed = true;
			for (ABObject after : afterObjects) {
				if (closeEnoughWithOutPosition(before, after)) {
					// TODO: If translation is to be used, here would be the place
					destroyed = false;
				}
			}
			if (destroyed) {
				destroyedObjects.add(before);
			} else {
				movedObjects.add(before);
			}
		}
	}

	private boolean closeEnoughWithOutPosition(ABObject before, ABObject after) {
		if (before.getType() != after.getType()) {
			return false;
		}
		if (before.getShape() != after.getShape()) {
			return false;
		}

		double threshold = 10;
		if ((before.getArea() - after.getArea()) > threshold) {
			return false;
		}

		return true;

	}

	private boolean closeEnough(ABObject before, ABObject after) {
		if (before.getType() != after.getType()) {
			return false;
		}
		if (before.getShape() != after.getShape()) {
			return false;
		}

		double distance = Math.sqrt(Math.pow(before.getCenterX() - after.getCenterX(), 2) +
				Math.pow(before.getCenterY() - after.getCenterY(), 2));
		double threshold = 5;

		if (distance > threshold) {
			return false;
		}

		return true;
	}

	public List<ABObject> getUnchangedObjects() {
		return Collections.unmodifiableList(unchangedObjects);
	}

	public List<ABObject> getDestroyedObjects() {
		return Collections.unmodifiableList(destroyedObjects);
	}

	public List<ABObject> getMovedObjects() {
		return Collections.unmodifiableList(movedObjects);
	}

}
