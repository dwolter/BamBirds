package de.uniba.sme.bambirds.planner.physicssimulation.debugging.evaluation;

import java.util.LinkedList;
import java.util.List;

import de.uniba.sme.bambirds.planner.physicssimulation.exceptions.PredicateGenerationException;
import de.uniba.sme.bambirds.planner.physicssimulation.physics.modification.stability.NaturalWorldStabilizer;
import de.uniba.sme.bambirds.planner.physicssimulation.predicategeneration.IsStableScenePredicateGenerator;
import de.uniba.sme.bambirds.planner.physicssimulation.predicategeneration.SupportPredicateGenerator;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.Scene;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.SceneGenerator;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.modification.randomisation.PositionRandomizer;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.modification.randomisation.RotationRandomizer;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.modification.randomisation.SizeRandomiser;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.preprocessing.CustomPreProcessor;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.preprocessing.DefaultPreprocessor;
import de.uniba.sme.bambirds.planner.predicates.Predicate;

public class EvaluationSupportPredicateGenerator {

    public static void main(String[] args) {

        Scene baseScene = SceneGenerator.createStabilityEvaluationScene();

        List<Predicate> groundTruth = getGroundTruth(baseScene);

        for (int i = 0; i <= 3; i++) {
            CustomPreProcessor randomizer = new CustomPreProcessor();
            randomizer.addModifier(new PositionRandomizer(i, i));
            System.out.println("Position (max derivation " + i + "):");
            evalPrecisionAndRecall(baseScene, groundTruth, randomizer);
        }

        System.out.println("####################################");

        for (int i = 0; i <= 3; i++) {
            CustomPreProcessor randomizer = new CustomPreProcessor();
            randomizer.addModifier(new SizeRandomiser(i));
            System.out.println("Size (max derivation " + i + "):");
            evalPrecisionAndRecall(baseScene, groundTruth, randomizer);
        }

        System.out.println("####################################");

        for (int i = 0; i <= 3; i++) {
            CustomPreProcessor randomizer = new CustomPreProcessor();
            randomizer.addModifier(new RotationRandomizer(i));
            System.out.println("Rotation (max derivation " + i + "):");
            evalPrecisionAndRecall(baseScene, groundTruth, randomizer);
        }

        System.out.println("####################################");

        for (int i = 3; i <= 3; i++) {
            CustomPreProcessor randomizer = new CustomPreProcessor();
            randomizer.addModifier(new PositionRandomizer(i, i));
            randomizer.addModifier(new SizeRandomiser(i));
            randomizer.addModifier(new RotationRandomizer(i));
            System.out.println("Position/Size/Rotation (max derivation " + i + "):");
            evalPrecisionAndRecall(baseScene, groundTruth, randomizer);
        }

    }

    private static List<Predicate> getGroundTruth(Scene baseScene) {
        Scene tmp = new Scene(baseScene);
        DefaultPreprocessor preprocessor = new DefaultPreprocessor();
        preprocessor.apply(tmp);

        SupportPredicateGenerator predGen = new SupportPredicateGenerator(tmp);

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
        return true_pos / (true_pos + false_pos);
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
        return true_pos / (true_pos + false_neg);
    }

    private static void evalPrecisionAndRecall(Scene baseScene, List<Predicate> groundTruth, CustomPreProcessor randomiser) {
        int number_of_runs = 1000;

        DefaultPreprocessor preprocessor = new DefaultPreprocessor();

        float sum_precision = 0f;
        float sum_recall = 0f;

        for (int n = 0; n < number_of_runs; n++) {
            
            Scene tmpScene = new Scene(baseScene);
            // randomisation of Input, simulation Vision inaccuracies
            randomiser.apply(tmpScene);
            // standard scene processing
            preprocessor.apply(tmpScene);

            try {
                if (new IsStableScenePredicateGenerator(new Scene(tmpScene), new NaturalWorldStabilizer(50, 20000f), 1f).call().isEmpty()) {
                    //not a stable scene --> skip, but try again to get n runs
                    n -= 1;
                    continue;
                }
            } catch (Exception e) {
                //same here
                n -= 1;
                continue;
            }

            SupportPredicateGenerator predGen = new SupportPredicateGenerator(tmpScene);

            try {
                List<Predicate> result = predGen.call();
                sum_precision += calcPrecision(result, groundTruth);
                sum_recall += calcRecall(result, groundTruth);

            } catch (PredicateGenerationException e) {
                System.err.println(e.getMessage());
            }
        }

        float avg_precision = sum_precision / (float) number_of_runs;
        float avg_recall = sum_recall / (float) number_of_runs;

        System.out.println("\tAvg. Precision:\t " + avg_precision);
        System.out.println("\tAvg. Recall:\t " + avg_recall);

        }


    }