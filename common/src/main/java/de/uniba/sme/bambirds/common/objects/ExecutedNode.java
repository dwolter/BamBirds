package de.uniba.sme.bambirds.common.objects;

import java.util.List;

import de.uniba.sme.bambirds.common.database.AbstractScene;
import de.uniba.sme.bambirds.common.database.Node;
import de.uniba.sme.bambirds.common.objects.ShotEffect.EffectType;
import de.uniba.sme.bambirds.common.objects.ab.ABObject;
import de.uniba.sme.bambirds.common.objects.ab.ABType;
import de.uniba.sme.bambirds.common.utils.MathUtil;

public class ExecutedNode implements Comparable<ExecutedNode> {
  
  private final Node node;
  private final List<ShotEffect> effects;

  public ExecutedNode(Node node, List<ShotEffect> effects) {
    this.node = node;
    this.effects = effects;
  }

  public Node getNode() {
    return node;
  }

  public List<ShotEffect> getEffects() {
    return effects;
  }

  /**
   * Calculate the similarity of another node to the executed node.
   * The score is calculated using the following metrics:
   * - Impact Angle
   * - Location of target object
   * - Bird type
   * - Expected Shot effects
   * 
   * @param node Another node
   * @return A score between 0 and 1 stating the similarity. 1 means identical, 0 means not at all similar
   */
  public double similarityTo(Node node) {
    Plan thisPlan = this.node.getPlan();
    Plan otherPlan = node.getPlan();

    // Compare impact angle

    double impactAngleSimilarity = 1 - Math.abs(thisPlan.getImpactAngle() - otherPlan.getImpactAngle()) / 180.0;

    AbstractScene thisBefore = this.node.getSceneBefore();
    AbstractScene otherBefore = node.getSceneBefore();

    // Compare target object

    ABObject thisTargetObject = thisBefore.findObjectWithID(thisPlan.getTargetObject());
    ABObject otherTargetObject = otherBefore.findObjectWithID(otherPlan.getTargetObject());

    double targetObjectSimilarity = 0;

    if (thisTargetObject != null && otherTargetObject != null && thisTargetObject.getType() == otherTargetObject.getType()) {
      int thisTargetObjectArea = thisTargetObject.getArea();
      int otherTargetObjectArea = otherTargetObject.getArea();
      double sizeSimilarity = valueSimilarity(otherTargetObjectArea, thisTargetObjectArea);
      if (thisTargetObject.intersects(otherTargetObject)) {
        targetObjectSimilarity = sizeSimilarity;
      } else {
        double distance = thisTargetObject.getCenter().distance(otherTargetObject.getCenter());
        targetObjectSimilarity = sizeSimilarity * Math.min(10, distance) / distance;
      }
    }

    // Compare current bird

    double birdSimilarity = 0;

    ABType thisBirdType = this.node.getBirdType();
    ABType otherBirdType = node.getBirdType();
    if (thisBirdType == otherBirdType) {
      birdSimilarity = 1;
    } else {
      switch (thisBirdType) {
        case RedBird:
          if (otherBirdType == ABType.BlueBird) {
            birdSimilarity = 0.8;
          }
          if (otherBirdType == ABType.YellowBird) {
            birdSimilarity = 0.5;
          }
          break;
        case BlueBird:
          if (otherBirdType == ABType.RedBird) {
            birdSimilarity = 0.7;
          }
          if (otherBirdType == ABType.YellowBird) {
            birdSimilarity = 0.4;
          }
          break;
        case YellowBird:
          if (otherBirdType == ABType.RedBird) {
            birdSimilarity = 0.3;
          }
          if (otherBirdType == ABType.BlueBird) {
            birdSimilarity = 0.3;
          }
          break;
        default:
          break;
      }
    }

    // Compare expected effects of plans

    List<ShotEffect> thisExpEffects = thisPlan.getExpectedEffects(thisBefore);
    List<ShotEffect> otherExpEffects = otherPlan.getExpectedEffects(otherBefore);

    long countThisDestroy = thisExpEffects.stream().filter((effect) -> effect.getType() == EffectType.DESTROY).count();
    long countThisMove = thisExpEffects.stream().filter((effect) -> effect.getType() == EffectType.MOVE).count();
    long countThisFree = thisExpEffects.stream().filter((effect) -> effect.getType() == EffectType.FREE).count();
    long countOtherDestroy = otherExpEffects.stream().filter((effect) -> effect.getType() == EffectType.DESTROY).count();
    long countOtherMove = otherExpEffects.stream().filter((effect) -> effect.getType() == EffectType.MOVE).count();
    long countOtherFree = otherExpEffects.stream().filter((effect) -> effect.getType() == EffectType.FREE).count();

    double expEffectsSimilarity = valueSimilarity(countThisDestroy, countOtherDestroy) * valueSimilarity(countThisMove, countOtherMove) * valueSimilarity(countThisFree, countOtherFree);

    // Compare applicability of actual effects

    List<ABObject> pigs = otherBefore.getPigs();

    double effectsApplicability = effects.stream()
      .filter((effect) -> effect.getObject().getType() == ABType.Pig)
      .mapToInt((effect) -> pigs.contains(effect.getObject()) ? 1 : 0)
      .average().orElse(1);


    return MathUtil.harmonicMean(impactAngleSimilarity, targetObjectSimilarity, birdSimilarity, expEffectsSimilarity, effectsApplicability);
  }

  private double valueSimilarity(double a, double b) {
    if (a == 0 && b == 0) {
      return 1;
    }
    if (a == 0 || b == 0) {
      return 0;
    }
    return a > b ? b/a : a/b;
  }

  @Override
  public boolean equals(Object obj) {
    if (obj instanceof ExecutedNode) {
      ExecutedNode other = (ExecutedNode) obj;
      return this.node.getBirdType() == other.getNode().getBirdType()
          && this.node.getShot().equals(other.getNode().getShot())
          && this.node.getPlan().getTargetObject().equals(other.getNode().getPlan().getTargetObject());
    }
    return false;
  }

  @Override
  public int compareTo(ExecutedNode executedNode) {
    if (equals(executedNode)) {
      return 0;
    }
    if (this.node.getBirdType() != null && executedNode.getNode().getBirdType() != null && this.node.getBirdType() != executedNode.getNode().getBirdType()) {
      return this.node.getBirdType().id() - executedNode.getNode().getBirdType().id();
    }
    if (this.node.getPlan().getImpactAngle() != executedNode.getNode().getPlan().getImpactAngle()) {
      return this.node.getPlan().getImpactAngle() - executedNode.getNode().getPlan().getImpactAngle();
    }
    if (this.node.getPlan().getConfidence() != executedNode.getNode().getPlan().getConfidence()) {
      double difference = this.node.getPlan().getConfidence() - executedNode.getNode().getPlan().getConfidence();
      return (int) Math.copySign(Math.ceil(Math.abs(difference)),difference);
    }
    // FIXME: Hopefully never get here, since otherwise TreeSet would be not comparable
    return 0;
  }

  @Override
  public String toString() {
    return "{Node:" + node.toString() + ", Effects: " + effects + "}";
  }
}
