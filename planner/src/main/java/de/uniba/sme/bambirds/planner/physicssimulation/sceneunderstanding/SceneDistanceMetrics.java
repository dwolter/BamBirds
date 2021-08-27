package de.uniba.sme.bambirds.planner.physicssimulation.sceneunderstanding;

import de.uniba.sme.bambirds.common.objects.ab.ABType;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.Scene;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.entities.SceneEntityBase;

public class SceneDistanceMetrics {

	public enum DistanceMetric {
		EUCLIDEAN_STANDARD, EUCLIDEAN_WEIGHTED
	}

	public static float calculateDistance(Scene scene1, Scene scene2, SceneDistanceMetrics.DistanceMetric metric) {
		switch (metric) {
		case EUCLIDEAN_STANDARD:
			return euclideanDistance(scene1, scene2);
		case EUCLIDEAN_WEIGHTED:
			return euclideanDistanceWeighted(scene1, scene2);

		default:
			return euclideanDistance(scene1, scene2);
		}
	}

	private static float euclideanDistanceWeighted(Scene scene1, Scene scene2) {
		float totalDistance = 0f;
		for (SceneEntityBase entity1 : scene1.getAllEntities()) {
			for (SceneEntityBase entity2 : scene2.getAllEntities()) {
				if (entity1.getGlobalID().equals(entity2.getGlobalID())) {
					float entityDistance = getDistance(entity1, entity2);
					entityDistance *= entityDistance; // the farther apart they are , the worse it is ( not linearly )
					totalDistance += entityDistance;
				}
			}
		}
		return totalDistance;
	}

	/**
	 * Calculates the eucledean distance of between identical objects in both scene based on their position and rotation
	 * (Assumes both scenes include the same object, objects that cannot be matched are ignored)
	 *
	 * @param scene1 snapshot of first scene
	 * @param scene2 snapshot of second scene
	 */
	private static float euclideanDistance(Scene scene1, Scene scene2) {
		float distance = 0f;
		for (SceneEntityBase entity1 : scene1.getAllEntities()) {
			for (SceneEntityBase entity2 : scene2.getAllEntities()) {

				if (entity1.getGlobalID().equals(entity2.getGlobalID())) {
					distance += getDistance(entity1, entity2);
				}
			}
		}
		return distance;
	}

	private static float getDistance(SceneEntityBase entity1, SceneEntityBase entity2) {
		float xPosSQ = (entity1.getCenterX() - entity2.getCenterX()) * (entity1.getCenterX() - entity2.getCenterX());
		float yPosSQ = (entity1.getCenterY() - entity2.getCenterY()) * (entity1.getCenterY() - entity2.getCenterY());
		float rotDif;
		if (entity1.getAngle() > entity2.getAngle()) {
			rotDif = entity1.getAngle() - entity2.getAngle();
		} else {
			rotDif = entity2.getAngle() - entity1.getAngle();
		}
		if (rotDif > Math.PI) {
			rotDif = ((float) (Math.PI * 2f)) - rotDif; 
		}
		float rotSQ = rotDif * rotDif;
		return (float) xPosSQ + yPosSQ + rotSQ;
	}
}