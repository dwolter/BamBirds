package de.uniba.sme.bambirds.planner.physicssimulation.sceneunderstanding;

import java.util.*;
import java.util.stream.Collectors;

import de.uniba.sme.bambirds.common.objects.ab.ABType;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.entities.properties.EntityPropertiesLUT;
import de.uniba.sme.bambirds.planner.physicssimulation.sceneunderstanding.propertymodification.modifier.*;
import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;

import de.uniba.sme.bambirds.planner.physicssimulation.debugging.VisualSimulationDebugger;
import de.uniba.sme.bambirds.planner.physicssimulation.physics.Simulation;
import de.uniba.sme.bambirds.planner.physicssimulation.physics.SimulationSettings;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.Scene;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.SceneGenerator;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.entities.SceneCircle;
import de.uniba.sme.bambirds.planner.physicssimulation.sceneunderstanding.SceneDistanceMetrics.DistanceMetric;
import de.uniba.sme.bambirds.planner.physicssimulation.sceneunderstanding.utils.StepwisePropertyManipulator;


public class SceneUnderstandingMain {
	private static final Logger log = LogManager.getLogger(SceneUnderstandingMain.class);


	private static Random random = new Random();

    public static void main(String[] args) {
//		LogManager.getRootLogger().setLevel(Level.ALL);
		StartUnderstandingProcedure();
    }


	public static void StartUnderstandingProcedure(){
		 Hashtable<Scene, EvaluationResult> totalEvaluationResults = new Hashtable<>();

    	//=========================================================
    	//=============  SETTINGS =================================
		int numberOfRealWorldRandomisations = 2;

		int numberOfMentalWorldRandomisations = 2;

		DistanceMetric distanceMetric = DistanceMetric.EUCLIDEAN_WEIGHTED;
		int maxIterationsDepth  = 100;
		int modificationStepsPerIteration = 10;
		//=========================================================


		//=========================================================
    	//============ SCENE SETUP ================================
    	List<Scene> evaluationScenes = new LinkedList<>();

    	Scene scene1 = SceneGenerator.createSceneUnderstandingTestScene();
		SceneCircle bird1 = scene1.getBirds().get(0);
		//bird1.setLinearVelocityX(25f);
		bird1.setLinearVelocityX(40f);
		bird1.setLinearVelocityY(8f);
		bird1.getProperties().setFixtureDensity(10);
		evaluationScenes.add(scene1);

		Scene scene2 = SceneGenerator.createSceneUnderstandingTestScene2();
		SceneCircle bird2 = scene2.getBirds().get(0);
		//bird2.setLinearVelocityX(35f);
		bird2.setLinearVelocityX(60f);
		bird2.setLinearVelocityY(16f);
		bird2.getProperties().setFixtureDensity(10);
		evaluationScenes.add(scene2);
		//=========================================================


		for(Scene evaluationScene : evaluationScenes){

			for (int i = 0; i < numberOfRealWorldRandomisations; i++) {
				SimulationSettings realWorldSimSettings = new SimulationSettings();
				realWorldSimSettings.setMaxRuntimeMillis(20000); //limit virtual simulation time
				Scene realWorldScene = new Scene(evaluationScene);
				realWorldScene.logAllEntities();
				Simulation randomizedRealWorldSim = new Simulation("Real World", realWorldScene, realWorldSimSettings);

				//randomizedRealWorldSim.getSettings().getGravity().y = randomizeFloat(randomizedRealWorldSim.getSettings().getGravity().y,defaultRealWorldGravityMaxOffset,true);

				EntityPropertiesLUT initialRealEntityProperties = randomizedRealWorldSim.getScene().getEntityPropertiesLUT();

				//Sensible Restitution/Friction Values: [0, 1]
				//see https://www.iforce2d.net/b2dtut/fixtures
				initialRealEntityProperties.getEntityProperties(ABType.Wood).setFixtureFriction(randomFloatInRange(0f, 1f));
				initialRealEntityProperties.getEntityProperties(ABType.Wood).setFixtureRestitution(randomFloatInRange(0f, 1f));

				//Density must be non-negative
				initialRealEntityProperties.getEntityProperties(ABType.Wood).setFixtureDensity(randomFloatInRange(0f, 3f));

				//Should be between [0, 0.1], however in theory [0, infinity]
				//https://box2d.org/documentation/md__d_1__git_hub_box2d_docs_dynamics.html
				initialRealEntityProperties.getEntityProperties(ABType.Wood).setAngularDamping(randomFloatInRange(0f, 0.1f));
				initialRealEntityProperties.getEntityProperties(ABType.Wood).setLinearDamping(randomFloatInRange(0f, 0.1f));

				randomizedRealWorldSim.run();
				randomizedRealWorldSim.addToVisualSimulationDebugger(true);

				for (int j = 0; j < numberOfMentalWorldRandomisations; j++) {
					SimulationSettings initialMentalModelSettings = new SimulationSettings();
					initialMentalModelSettings.setMaxRuntimeMillis(20000);

					//initialMentalModelSettings.setGravity(new Vec2(0f, randomizeFloat(initialMentalModelSettings.getGravity().y,defaultMentalWorldGravityMaxOffset,true)));
					EntityPropertiesLUT initialMentalEntityProperties = new EntityPropertiesLUT();

					//Use same restrictions as real world
					initialMentalEntityProperties.getEntityProperties(ABType.Wood).setFixtureFriction(randomFloatInRange(0f, 1f));
					initialMentalEntityProperties.getEntityProperties(ABType.Wood).setFixtureRestitution(randomFloatInRange(0f, 1f));

					initialMentalEntityProperties.getEntityProperties(ABType.Wood).setFixtureDensity(randomFloatInRange(0f, 3f));

					initialMentalEntityProperties.getEntityProperties(ABType.Wood).setAngularDamping(randomFloatInRange(0f, 0.1f));
					initialMentalEntityProperties.getEntityProperties(ABType.Wood).setLinearDamping(randomFloatInRange(0f, 0.1f));


					Simulation randomizedInitialMentalModel = new Simulation("Initial Mental Model ", new Scene(realWorldScene), initialMentalModelSettings);
					randomizedInitialMentalModel.getScene().setEntityPropertiesLUT(initialMentalEntityProperties);
					randomizedInitialMentalModel.run();
					randomizedInitialMentalModel.addToVisualSimulationDebugger(true);

					List<StepwisePropertyManipulator> propertyManipulators = new LinkedList<>();
					//propertyManipulators.add(new StepwisePropertyManipulator(new VerticalGravityModifier(), 0.1f, randomizedInitialMentalModel,-50,50,0f));
					propertyManipulators.add(new StepwisePropertyManipulator(new FrictionModifier(ABType.Wood), 0.01f, randomizedInitialMentalModel,0,1,0f));
					propertyManipulators.add(new StepwisePropertyManipulator(new RestitutionModifier(ABType.Wood), 0.01f, randomizedInitialMentalModel,0,1,0f));

					propertyManipulators.add(new StepwisePropertyManipulator(new DensityModifier(ABType.Wood), 0.01f, randomizedInitialMentalModel,0,3,0f));

					propertyManipulators.add(new StepwisePropertyManipulator(new LinearDampingModifier(ABType.Wood), 0.001f, randomizedInitialMentalModel,0,0.1f,0f));
					propertyManipulators.add(new StepwisePropertyManipulator(new AngularDampingModifier(ABType.Wood), 0.001f, randomizedInitialMentalModel,0,0.1f,0f));


					GreedyIncrementalConverger sceneUnderstanding = new GreedyIncrementalConverger(randomizedInitialMentalModel, randomizedRealWorldSim, propertyManipulators, distanceMetric, maxIterationsDepth, modificationStepsPerIteration);

					Simulation finalMentalModel = sceneUnderstanding.convergeMentalModel();
					finalMentalModel.run();
					sceneUnderstanding.printResult();
					EvaluationResult paritalEvaluationResult = EvaluationResult.getDifference(finalMentalModel,randomizedRealWorldSim);

					if(totalEvaluationResults.containsKey(evaluationScene) == false){
						totalEvaluationResults.put(evaluationScene,paritalEvaluationResult);
					}else{
						totalEvaluationResults.get(evaluationScene).addEvaluationResult(paritalEvaluationResult);
					}
				}
				System.out.println(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>");
				System.out.println(">> DONE WITH RANDOM REAL WORLD (" + (i+1) + "/" + numberOfRealWorldRandomisations + ") >>");
				System.out.println(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>");
			}
			System.out.println(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>");
			System.out.println(">>>>>>>>>>>>> DONE WITH SCENE >>>>>>>>>>>>>>>>>>");
			System.out.println(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>");

		}
		// PRINT FINAL RESULT:
		System.out.println(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>");
		System.out.println(">>>>>>>>>>>>>> FINAL RESULT >>>>>>>>>>>>>>>>>>>>");
		System.out.println(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>");
		System.out.println("");
		System.out.println("Number of Realworld-Randomisations: "+ numberOfRealWorldRandomisations);
		System.out.println("Number of Mental-Scene-Randomisations: "+ numberOfMentalWorldRandomisations);
		System.out.println("--------------------------");
		for (int i = 0; i < totalEvaluationResults.keySet().size(); i++) {
			Scene scene = totalEvaluationResults.keySet().stream().collect(Collectors.toList()).get(i);
			EvaluationResult evaluationResult = totalEvaluationResults.get(scene);
			System.out.println("Scene Number: "  + i);
			System.out.println(evaluationResult.toString());
			System.out.println("");
			System.out.println("--------------------------");
		}
		VisualSimulationDebugger.visualize();

	}

	public static float randomFloatInRange(float minValue, float maxValue) {
		float randF = random.nextFloat();
		float randomInRange = minValue + randF * (maxValue - minValue);
		return randomInRange;
	}


	public static float randomizeFloat(float input, float maxRandomOffset, boolean allowNegative){
    	Random random = new Random();

    	float result = input + maxRandomOffset * (random.nextFloat()*2-1f);
    	if(allowNegative == false && result < 0){
    		result = Math.abs(result);
		}
    	return result;
	}

}

