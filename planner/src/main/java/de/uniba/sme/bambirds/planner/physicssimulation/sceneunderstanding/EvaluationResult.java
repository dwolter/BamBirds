package de.uniba.sme.bambirds.planner.physicssimulation.sceneunderstanding;

import de.uniba.sme.bambirds.common.objects.ab.ABType;
import de.uniba.sme.bambirds.planner.physicssimulation.physics.Simulation;

public class EvaluationResult{
	int AmountOfAccumulatedResults = 1;
	float GravityY;
	float Friction;
	float Restitution;
	float Density;
	float AngularDamping;
	float LinearDamping;

	public EvaluationResult(float gravityY, float friction, float restitution, float density, float angularDamping,
			float linearDamping) {
		GravityY = gravityY;
		Friction = friction;
		Restitution = restitution;
		Density = density;
		AngularDamping = angularDamping;
		LinearDamping = linearDamping;
	}


	public void addEvaluationResult(EvaluationResult evaluationResult){
		AmountOfAccumulatedResults += 1;
		GravityY += evaluationResult.GravityY;
		Friction += evaluationResult.Friction;
		Restitution += evaluationResult.Restitution;
		Density += evaluationResult.Density;
		AngularDamping += evaluationResult.AngularDamping;
		LinearDamping += evaluationResult.LinearDamping;
	}



	@Override public String toString() {
		return "EvaluationResult: "
				+ "\n\tSubResults:  "+ AmountOfAccumulatedResults
				+ "\n\tGravityY +/- " + Math.abs(GravityY / AmountOfAccumulatedResults)
				+ "\n\tFriction +/- "+Math.abs(Friction/ AmountOfAccumulatedResults)
				+ "\n\tRestitution +/- "+Math.abs(Restitution/ AmountOfAccumulatedResults)
				+ "\n\tDensity +- "+Math.abs(Density/ AmountOfAccumulatedResults)
				+ "\n\tAngularDamping +- "+ Math.abs(AngularDamping/ AmountOfAccumulatedResults)
				+ "\n\tLinearDamping +- " + Math.abs(LinearDamping/ AmountOfAccumulatedResults);
	}

	public static  EvaluationResult getDifference(Simulation mentalModelSimulation, Simulation realWorldSimulation){
		return new EvaluationResult(
				Math.abs(mentalModelSimulation.getSettings().getGravity().y - realWorldSimulation.getSettings().getGravity().y),
				Math.abs(mentalModelSimulation.getScene().getEntityPropertiesLUT().getEntityProperties(ABType.Wood).getFixtureFriction() - realWorldSimulation.getScene().getEntityPropertiesLUT().getEntityProperties(ABType.Wood).getFixtureFriction()),
				Math.abs(mentalModelSimulation.getScene().getEntityPropertiesLUT().getEntityProperties(ABType.Wood).getFixtureRestitution() - realWorldSimulation.getScene().getEntityPropertiesLUT().getEntityProperties(ABType.Wood).getFixtureRestitution()),
				Math.abs(mentalModelSimulation.getScene().getEntityPropertiesLUT().getEntityProperties(ABType.Wood).getFixtureDensity() - realWorldSimulation.getScene().getEntityPropertiesLUT().getEntityProperties(ABType.Wood).getFixtureDensity()),
				Math.abs(mentalModelSimulation.getScene().getEntityPropertiesLUT().getEntityProperties(ABType.Wood).getAngularDamping() - realWorldSimulation.getScene().getEntityPropertiesLUT().getEntityProperties(ABType.Wood).getAngularDamping()),
				Math.abs(mentalModelSimulation.getScene().getEntityPropertiesLUT().getEntityProperties(ABType.Wood).getLinearDamping() - realWorldSimulation.getScene().getEntityPropertiesLUT().getEntityProperties(ABType.Wood).getLinearDamping())
		);
	}

}
