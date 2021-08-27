package de.uniba.sme.bambirds.planner.physicssimulation.debugging.evaluation;

import java.util.LinkedList;
import java.util.List;

import de.uniba.sme.bambirds.planner.physicssimulation.debugging.VisualSimulationDebugger;
import de.uniba.sme.bambirds.planner.physicssimulation.exceptions.PredicateGenerationException;
import de.uniba.sme.bambirds.planner.physicssimulation.physics.SimulationSettings;
import de.uniba.sme.bambirds.planner.physicssimulation.physics.modification.stability.NaturalWorldStabilizer;
import de.uniba.sme.bambirds.planner.physicssimulation.predicategeneration.IsStableScenePredicateGenerator;
import de.uniba.sme.bambirds.planner.physicssimulation.predicategeneration.IsReachablePredicateGenerator;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.Scene;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.SceneGenerator;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.modification.randomisation.PositionRandomizer;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.modification.randomisation.RotationRandomizer;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.modification.randomisation.SizeRandomiser;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.modification.scaling.GlobalScale;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.modification.smoothing.AngleSmoother;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.modification.smoothing.GlobalPolygonSmoother;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.modification.smoothing.LocalPolygonSmoother;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.modification.translation.BlockAlignment;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.preprocessing.CustomPreProcessor;

import de.uniba.sme.bambirds.planner.predicates.Predicate;
import org.apache.log4j.Level;
import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;

public class EvaluationIsReachablePredicateGenerator {
	private static final Logger log = LogManager.getLogger(EvaluationIsReachablePredicateGenerator.class);

	private static final int number_of_runs = 500;
	private static final boolean printGroundTruth = true;
	private static final boolean createVisualDebugger = false;

	private static final float shootingAngleMin = 0f;
	private static final float shootingAngleMax = 90f;
	private static final float shootingAngleStepSize = 5f;
	private static final float shootingStrengthMin = 0.1f;
	private static final float shootingStrengthMax = 1f;
	private static final float shootingStrengthStepSize = 0.1f;

	public static void main(String[] args) {
			LogManager.getRootLogger().setLevel(Level.ALL);
			LogManager.getRootLogger().setLevel(Level.ERROR);


			Scene baseScene = SceneGenerator.createIsReachableExampleScene();
//			Scene baseScene = SceneGenerator.createSupportEvaluationScene();

			List<Predicate> groundTruth = getGroundTruth(baseScene);
//			for (int i = 0; i <= 3; i++) {
//				CustomPreProcessor randomizer = new CustomPreProcessor();
//				randomizer.addModifier(new PositionRandomizer(i , i));
//				System.out.println("Position (max derivation " + i + "):");
//				evalPrecisionAndRecall(baseScene, groundTruth, randomizer);
//			}

			System.out.println("####################################");
//
//			for (int i = 0; i <= 3; i++) {
//				CustomPreProcessor randomizer = new CustomPreProcessor();
//				randomizer.addModifier(new SizeRandomiser(i));
//				System.out.println("Size (max derivation " + i + "):");
//				evalPrecisionAndRecall(baseScene, groundTruth, randomizer);
//			}
//
//			System.out.println("####################################");
//
//			for (int i = 0; i <= 15; i++) {
//				CustomPreProcessor randomizer = new CustomPreProcessor();
//				randomizer.addModifier(new RotationRandomizer(i));
//				System.out.println("Rotation (max derivation " + i + "):");
//				evalPrecisionAndRecall(baseScene, groundTruth, randomizer);
//			}

			System.out.println("####################################");

			for (int i = 0; i <= 3; i++) {
				CustomPreProcessor randomizer = new CustomPreProcessor();
				randomizer.addModifier(new PositionRandomizer(i, i));
				randomizer.addModifier(new SizeRandomiser(i));
				randomizer.addModifier(new RotationRandomizer(i));
				System.out.println("Position/Size/Rotation (max derivation " + i + "):");
				evalPrecisionAndRecall(baseScene, groundTruth, randomizer);
			}
			if(createVisualDebugger){
				VisualSimulationDebugger.setDrawCoordinateSystem(true);
				VisualSimulationDebugger.visualize();
			}


		}
		private static void ApplySceneRandomizationThenPreprocessing(Scene inputScene, CustomPreProcessor randomizer){
			new GlobalScale(1/0.085f).apply(inputScene); // first lets scale to dimensions like when receiving data from the computervisionmodule

			randomizer.apply(inputScene);
			if(createVisualDebugger){
				VisualSimulationDebugger.addScene("Randomized",inputScene, new SimulationSettings());
			}
//			//Apply Similar preprocessing like in DefaultPreprocessor
			new AngleSmoother(10f).apply(inputScene);
			new LocalPolygonSmoother(4).apply(inputScene);
			new GlobalPolygonSmoother(4).apply(inputScene);
			new BlockAlignment(2f).apply(inputScene);
			new GlobalScale(0.085f).apply(inputScene); // now after preprocessing scale it back

		}


		private static List<Predicate> getGroundTruth(Scene baseScene) {
			Scene tempScene = new Scene(baseScene);
			ApplySceneRandomizationThenPreprocessing(tempScene, new CustomPreProcessor());
			IsReachablePredicateGenerator predGen = new IsReachablePredicateGenerator(
					tempScene,
					false,
					shootingAngleMin,
					shootingAngleMax,
					shootingAngleStepSize,
					shootingStrengthMin,
					shootingStrengthMax,
					shootingStrengthStepSize);

			List<Predicate> groundTruth = new LinkedList<>();
			try {
				groundTruth = predGen.call();
			} catch (PredicateGenerationException e) {
				e.printStackTrace();
			}
			return groundTruth;
		}

		private static float calcPrecision(List<Predicate> result, List<Predicate> groundTruth) {
			float true_pos = 0f;
			float false_pos = 0f;

			for (Predicate prediction: result) {
				if (groundTruth.contains(prediction)) {
					true_pos += 1f;
				} else {
					false_pos += 1f;
				}
			}
			float precision = true_pos / (true_pos + false_pos);
			return precision;
		}

		private static float calcRecall(List<Predicate> result, List<Predicate> groundTruth) {
			float true_pos = 0f;
			float false_neg = 0f;

			for (Predicate gt: groundTruth) {
				if (result.contains(gt)) {
					true_pos += 1f;
				} else {
					false_neg += 1f;
				}
			}
			float recall = true_pos / (true_pos + false_neg);
			return recall;
		}

		private static void evalPrecisionAndRecall(Scene baseScene, List<Predicate> groundTruth, CustomPreProcessor randomiser) {

//			DefaultPreprocessor preprocessor = new DefaultPreprocessor();

			float sum_precision = 0f;
			float sum_recall = 0f;

			for (int n = 0; n < number_of_runs; n++) {

				Scene tempScene = new Scene(baseScene);
				// randomisation of Input, simulation Vision inaccuracies
				// standard scene processing
				ApplySceneRandomizationThenPreprocessing(tempScene, randomiser);

				try {
					boolean isSceneStable = !( new IsStableScenePredicateGenerator(new Scene(tempScene), new NaturalWorldStabilizer(50, 20000f), 1f).call().isEmpty());
					if (isSceneStable == false) {
						//not a stable scene --> skip, but try again to get n runs
						log.debug("n: "+n + " Scene is not stable" );
						n -= 1;
						continue;
					}
				} catch (Exception e) {
					//same here
					log.debug("n: "+n + " Exception" );

					n -= 1;
					continue;
				}
				log.debug("n: "+ n + " Predicate Generator Starting");
//				IsReachablePredicateGenerator predGen = new IsReachablePredicateGenerator(tempScene, false);
				IsReachablePredicateGenerator predGen = new IsReachablePredicateGenerator(
						tempScene,
						false,
						shootingAngleMin,
						shootingAngleMax,
						shootingAngleStepSize,
						shootingStrengthMin,
						shootingStrengthMax,
						shootingStrengthStepSize);

				try {
					List<Predicate> result = predGen.call();
					if(createVisualDebugger){
						predGen.addSimulationsToVisualSimulationDebugger("N:  "+n + " ");
					}
					sum_precision += calcPrecision(result, groundTruth);
					sum_recall += calcRecall(result, groundTruth);

				} catch (PredicateGenerationException e) {
					System.err.println(e.getMessage());
				}
			}

			float avg_precision = sum_precision / (float) number_of_runs;
			float avg_recall = sum_recall / (float) number_of_runs;

			System.out.println("\tNumber of Runs:\t " + number_of_runs);
			System.out.println("\tAvg. Precision:\t " + avg_precision);
			System.out.println("\tAvg. Recall:\t " + avg_recall);
			if(printGroundTruth){
				System.out.println("\tGround Truth: ");
				for(Predicate predicate : groundTruth){
					System.out.println("\t\t" + predicate);
				}
			}
			System.out.println("");
		}


	}

