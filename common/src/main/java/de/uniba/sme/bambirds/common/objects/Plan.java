package de.uniba.sme.bambirds.common.objects;

import de.uniba.sme.bambirds.common.gson.JsonRequired;

import java.util.Arrays;

import com.google.gson.annotations.SerializedName;

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

	public Plan(String target, int angle, String strategy, double confidence, ThinkerType thinker) {
		this(null, target, angle, strategy, confidence, new String[] {}, null, thinker);
	}

	public Plan(String target, int angle, String strategy, double confidence, String[] reasons, ThinkerType thinker) {
		this(null, target, angle, strategy, confidence, reasons, null, thinker);
	}

	public Plan(String target, int angle, String strategy, double confidence, Shot shot, ThinkerType thinker) {
		this(null, target, angle, strategy, confidence, new String[] {}, shot, thinker);
	}

	public Plan(String bird, String target, int angle, String strategy, double confidence, String[] reasons, Shot shot,
			ThinkerType thinker) {
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

	/**
	 * @return a String indicating what strategy has been chosen or where the Target
	 *         originated from (e.g. PhysicsSim).
	 */
	public String getStrategy() {
		return strategy;
	}

	public double getConfidence() {
		return confidence;
	}

	public void setConfidence(double confidence) {
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

  public String getBird() {
    return bird;
  }

	public String prettyPrint() {
		String reasonString = (reasons == null || reasons.length == 0) ? "" : " Reasons: " + Arrays.toString(reasons);
		String shotInfoString = shot == null ? "" : " Shot: " + shot.toString();
		return String.format("Plan: %7s @ angle %3s  strategy: %-20s Confidence: %1.4f%s%s", targetObject, impactAngle, strategy,
				confidence, reasonString, shotInfoString);
	}

	@Override
	public String toString() {
		String reasonString = (reasons == null || reasons.length == 0) ? "" : ":" + Arrays.toString(reasons);
		String shotInfoString = shot == null ? "" : ":" + shot.toString();
		return String.format("(Plan %s:%s:%d:%1.4f%s%s)", targetObject, strategy, impactAngle, confidence, reasonString,
				shotInfoString);
	}

	@Override
	public boolean equals(Object obj) {
		if (obj instanceof Plan) {
			Plan p = (Plan) obj;
			return this.bird.equals(p.bird) &&
				this.confidence == p.confidence &&
				this.impactAngle == p.impactAngle &&
				Arrays.equals(this.reasons, p.reasons) &&
				this.shot.equals(p.shot) &&
				this.strategy.equals(p.strategy) &&
				this.targetObject.equals(p.targetObject) &&
				this.thinker.equals(p.thinker);
		}
		return super.equals(obj);
	}

}
