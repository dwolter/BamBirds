package de.uniba.sme.bambirds.common.objects;

import com.google.gson.annotations.SerializedName;

import de.uniba.sme.bambirds.common.database.AbstractScene;
import de.uniba.sme.bambirds.common.gson.JsonRequired;
import de.uniba.sme.bambirds.common.objects.ShotEffect.EffectType;
import de.uniba.sme.bambirds.common.objects.ab.ABType;
import de.uniba.sme.bambirds.common.utils.ShotHelper;

import java.awt.Point;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

public class Plan {

	@JsonRequired
	@SerializedName("target_object")
	private final String targetObject;
	@SerializedName("impact_angle")
	private final int impactAngle; // between +90 and -90
	private final String strategy;
	@JsonRequired
	private double confidence;
	@JsonRequired
	private final Shot shot;
	private final String[] reasons;
	@JsonRequired
	private final ThinkerType thinker; // the "source" of the Target
	private final String bird;

	public Plan(final String target, final int angle, final String strategy, final double confidence, final ThinkerType thinker) {
		this(null, target, angle, strategy, confidence, new String[]{}, null, thinker);
	}

	public Plan(final String target, final int angle, final String strategy, final double confidence, final String[] reasons, final ThinkerType thinker) {
		this(null, target, angle, strategy, confidence, reasons, null, thinker);
	}

	public Plan(final String target, final int angle, final String strategy, final double confidence, final Shot shot, final ThinkerType thinker) {
		this(null, target, angle, strategy, confidence, new String[]{}, shot, thinker);
	}

	public Plan(final String bird, final String target, final int angle, final String strategy, final double confidence, final String[] reasons, final Shot shot,
							final ThinkerType thinker) {
		this.bird = bird;
		this.targetObject = target;
		this.strategy = strategy;
		this.confidence = confidence;
		this.thinker = thinker;
		this.impactAngle = angle;
		this.reasons = reasons;
		this.shot = shot;
	}

	/**
	 * Gives the target to be hit by a shot.
	 *
	 * @return the ID of the targeted object as a String
	 */
	public String getTargetObject() {
		return targetObject;
	}

	public int getImpactAngle() {
		return impactAngle;
	}

	public double getReleaseAngle() {
		return Math.toDegrees(ShotHelper.releasePointToAngle(new Point(shot.getDragX(), shot.getDragY())));
	}

	/**
	 * @return a String indicating what strategy has been chosen or where the Target
	 * originated from (e.g. PhysicsSim).
	 */
	public String getStrategy() {
		return strategy;
	}

	public double getConfidence() {
		return confidence;
	}

	public void setConfidence(final double confidence) {
		this.confidence = confidence;
	}

	public ThinkerType getThinker() {
		return thinker;
	}

	public Shot getShot() {
		return shot;
	}

	public String[] getReasons() {
		return reasons;
	}

	private List<String> getReasonObjects(String reasonName, ABType objectType) {
		return Arrays.stream(reasons).filter((reason) -> {
			return reason.startsWith(reasonName) && ABType.fromObjectID(reason.replace(reasonName+"(", "")) == objectType;
		}).map((reason) -> 
			reason.replace(reasonName+"(", "").replace(")", "")
		).collect(Collectors.toList());
	}

	/**
	 * @return Pigs that are expected to be destroyed by this plan.
	 */
	public List<String> getDestroyedPigs() {
		return getReasonObjects("destroy", ABType.Pig);
	}

	/**
	 * @return All Pigs that are expected to be affected by this plan. Either destroyed or freed or any other way affected.
	 */
	public List<String> getAffectedPigs() {
		return getReasonObjects("affect", ABType.Pig);
	}

	/**
	 * Freed means that the pig was not able to be destroyed before but may be now.<br>
	 * Examples:
	 * <ul>
	 * <li>Pigs that were hidden behind a structure</li>
	 * </ul>
	 * <br>
	 * @return Pigs that are expected to be freed by this plan.
	 */
	public List<String> getFreedPigs() {
		return getReasonObjects("free", ABType.Pig);
	}

	/**
	 * Get all of the expected shot effects
	 * @param scene The scene to which this plan is applied
	 * @return
	 */
	public List<ShotEffect> getExpectedEffects(AbstractScene scene) {
		return Arrays.stream(reasons).map((reason) -> {
			
			String object = reason.substring(reason.indexOf("(") + 1,reason.length() - 1);
			String effect = reason.substring(0, reason.indexOf("("));
			EffectType type;
			switch (effect) {
				case "affect":
					type = EffectType.MOVE;
					break;
				case "free":
					type = EffectType.FREE;
				case "destroy":
				default:
					type  = EffectType.DESTROY;
					break;
			}

			return new ShotEffect(scene.findObjectWithID(object), type);
		}).collect(Collectors.toList());
	}

	public String getBird() {
		return bird;
	}

	public String prettyPrint() {
		String reasonString = (reasons == null || reasons.length == 0) ? "" : " Reasons: " + Arrays.toString(reasons);
		String shotInfoString = shot == null ? "" : " Shot: " + shot;
		return String.format("Plan: %7s @ angle %4.1f  strategy: %-20s Confidence: %1.4f%s%s", targetObject, getReleaseAngle(), strategy,
				confidence, reasonString, shotInfoString);
	}

	@Override
	public String toString() {
		String reasonString = (reasons == null || reasons.length == 0) ? "" : ":" + Arrays.toString(reasons);
		String shotInfoString = shot == null ? "" : ":" + shot;
		return String.format("(Plan %s:%s:%1.4f:%1.4f%s%s)", targetObject, strategy, getReleaseAngle(), confidence, reasonString,
				shotInfoString);
	}

	@Override
	public int hashCode() {
		int result = Objects.hash(targetObject, strategy, confidence, shot, thinker, bird);
		result = 31 * result + Arrays.hashCode(reasons);
		return result;
	}

	@Override
	public boolean equals(final Object obj) {
		if (obj instanceof Plan) {
			Plan p = (Plan) obj;
			return this.bird.equals(p.bird)
					&& this.confidence == p.confidence
					&& Arrays.equals(this.reasons, p.reasons)
					&& this.shot.equals(p.shot)
					&& this.strategy.equals(p.strategy)
					&& this.targetObject.equals(p.targetObject)
					&& this.thinker.equals(p.thinker);
		}
		return super.equals(obj);
	}

}
