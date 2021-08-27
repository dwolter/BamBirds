package de.uniba.sme.bambirds.planner.physicssimulation.debugging.evaluation;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;

import de.uniba.sme.bambirds.planner.physicssimulation.exceptions.PredicateGenerationException;
import de.uniba.sme.bambirds.planner.physicssimulation.physics.modification.IWorldModifier;
import de.uniba.sme.bambirds.planner.physicssimulation.physics.modification.stability.NaturalWorldStabilizer;
import de.uniba.sme.bambirds.planner.physicssimulation.predicategeneration.ConfidencePredicateGenerator;
import de.uniba.sme.bambirds.planner.physicssimulation.predicategeneration.IsStableScenePredicateGenerator;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.Scene;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.SceneGenerator;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.modification.ISceneModifier;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.modification.randomisation.PositionRandomizer;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.modification.randomisation.RotationRandomizer;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.modification.randomisation.SizeRandomiser;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.preprocessing.DefaultPreprocessor;
import de.uniba.sme.bambirds.planner.predicates.Predicate;

public class EvaluationFrameworkStability {

    public static void main(String[] args) {

        Scene baseScene = SceneGenerator.createStabilityEvaluationScene();
        //examinePositionStability(baseScene);
//        examineSizeStability(baseScene);
//        examinePositionPlusSizeStability(baseScene);
        int defaultNumberOfRuns = 1000;
        int defaultMaxRuntimeInMillis = 20000;
//
        examineSomethingForStabilityConfidence("Position Randomization",baseScene,defaultMaxRuntimeInMillis,defaultNumberOfRuns,new PositionRandomizer(1,1));
        examineSomethingForStabilityConfidence("Position Randomization",baseScene,defaultMaxRuntimeInMillis,defaultNumberOfRuns,new PositionRandomizer(2,2));
        examineSomethingForStabilityConfidence("Position Randomization",baseScene,defaultMaxRuntimeInMillis,defaultNumberOfRuns,new PositionRandomizer(3,3));
        examineSomethingForStabilityConfidence("Position Randomization",baseScene,defaultMaxRuntimeInMillis,defaultNumberOfRuns,new PositionRandomizer(4,4));
        examineSomethingForStabilityConfidence("Position Randomization",baseScene,defaultMaxRuntimeInMillis,defaultNumberOfRuns,new PositionRandomizer(5,5));

        examineSomethingForStabilityConfidence("Scale Randomization",baseScene,defaultMaxRuntimeInMillis,defaultNumberOfRuns,new SizeRandomiser(1));
        examineSomethingForStabilityConfidence("Scale Randomization",baseScene,defaultMaxRuntimeInMillis,defaultNumberOfRuns,new SizeRandomiser(2));
        examineSomethingForStabilityConfidence("Scale Randomization",baseScene,defaultMaxRuntimeInMillis,defaultNumberOfRuns,new SizeRandomiser(3));
        examineSomethingForStabilityConfidence("Scale Randomization",baseScene,defaultMaxRuntimeInMillis,defaultNumberOfRuns,new SizeRandomiser(4));
        examineSomethingForStabilityConfidence("Scale Randomization",baseScene,defaultMaxRuntimeInMillis,defaultNumberOfRuns,new SizeRandomiser(5));


        examineSomethingForStabilityConfidence("Rotation Randomization",baseScene,defaultMaxRuntimeInMillis,defaultNumberOfRuns,new RotationRandomizer(1f));
        examineSomethingForStabilityConfidence("Rotation Randomization",baseScene,defaultMaxRuntimeInMillis,defaultNumberOfRuns,new RotationRandomizer(2f));
        examineSomethingForStabilityConfidence("Rotation Randomization",baseScene,defaultMaxRuntimeInMillis,defaultNumberOfRuns,new RotationRandomizer(3f));
        examineSomethingForStabilityConfidence("Rotation Randomization",baseScene,defaultMaxRuntimeInMillis,defaultNumberOfRuns,new RotationRandomizer(4f));
        examineSomethingForStabilityConfidence("Rotation Randomization",baseScene,defaultMaxRuntimeInMillis,defaultNumberOfRuns,new RotationRandomizer(5f));


        examineSomethingForStabilityConfidence("Position + Size Randomization",baseScene,defaultMaxRuntimeInMillis,defaultNumberOfRuns,Arrays.asList(new PositionRandomizer(1,1), new SizeRandomiser(1)));
        examineSomethingForStabilityConfidence("Position + Size Randomization",baseScene,defaultMaxRuntimeInMillis,defaultNumberOfRuns,Arrays.asList(new PositionRandomizer(2,2), new SizeRandomiser(2)));
        examineSomethingForStabilityConfidence("Position + Size Randomization",baseScene,defaultMaxRuntimeInMillis,defaultNumberOfRuns,Arrays.asList(new PositionRandomizer(3,3), new SizeRandomiser(3)));
        examineSomethingForStabilityConfidence("Position + Size Randomization",baseScene,defaultMaxRuntimeInMillis,defaultNumberOfRuns,Arrays.asList(new PositionRandomizer(4,4), new SizeRandomiser(4)));
//
//
        examineSomethingForStabilityConfidence("Position + Angle Randomization",baseScene,defaultMaxRuntimeInMillis,defaultNumberOfRuns,Arrays.asList(new PositionRandomizer(1,1), new RotationRandomizer(2f)));
        examineSomethingForStabilityConfidence("Position + Angle Randomization",baseScene,defaultMaxRuntimeInMillis,defaultNumberOfRuns,Arrays.asList(new PositionRandomizer(2,2), new RotationRandomizer(4f)));
        examineSomethingForStabilityConfidence("Position + Angle Randomization",baseScene,defaultMaxRuntimeInMillis,defaultNumberOfRuns,Arrays.asList(new PositionRandomizer(3,3), new RotationRandomizer(6f)));
        examineSomethingForStabilityConfidence("Position + Angle Randomization",baseScene,defaultMaxRuntimeInMillis,defaultNumberOfRuns,Arrays.asList(new PositionRandomizer(4,4), new RotationRandomizer(8f)));


        examineSomethingForStabilityConfidence("Size + Angle Randomization",baseScene,defaultMaxRuntimeInMillis,defaultNumberOfRuns,Arrays.asList(new SizeRandomiser(1), new RotationRandomizer(2f)));
        examineSomethingForStabilityConfidence("Size + Angle Randomization",baseScene,defaultMaxRuntimeInMillis,defaultNumberOfRuns,Arrays.asList(new SizeRandomiser(2), new RotationRandomizer(4f)));
        examineSomethingForStabilityConfidence("Size + Angle Randomization",baseScene,defaultMaxRuntimeInMillis,defaultNumberOfRuns,Arrays.asList(new SizeRandomiser(3), new RotationRandomizer(6f)));
        examineSomethingForStabilityConfidence("Size + Angle Randomization",baseScene,defaultMaxRuntimeInMillis,defaultNumberOfRuns,Arrays.asList(new SizeRandomiser(4), new RotationRandomizer(8f)));


        examineSomethingForStabilityConfidence("Position + Size + Angle Randomization",baseScene,defaultMaxRuntimeInMillis,defaultNumberOfRuns,Arrays.asList(new PositionRandomizer(1,1), new SizeRandomiser(1), new RotationRandomizer(1f)));
        examineSomethingForStabilityConfidence("Position + Size + Angle Randomization",baseScene,defaultMaxRuntimeInMillis,defaultNumberOfRuns,Arrays.asList(new PositionRandomizer(2,2), new SizeRandomiser(2), new RotationRandomizer(2f)));
        examineSomethingForStabilityConfidence("Position + Size + Angle Randomization",baseScene,defaultMaxRuntimeInMillis,defaultNumberOfRuns,Arrays.asList(new PositionRandomizer(3,3), new SizeRandomiser(3), new RotationRandomizer(3f)));
        examineSomethingForStabilityConfidence("Position + Size + Angle Randomization",baseScene,defaultMaxRuntimeInMillis,defaultNumberOfRuns,Arrays.asList(new PositionRandomizer(4,4), new SizeRandomiser(4), new RotationRandomizer(4f)));
        examineSomethingForStabilityConfidence("Position + Size + Angle Randomization",baseScene,defaultMaxRuntimeInMillis,defaultNumberOfRuns,Arrays.asList(new PositionRandomizer(5,5), new SizeRandomiser(5), new RotationRandomizer(5f)));

    }

    private static void examinePositionStability(Scene baseScene) {
        int max_position_radomisation = 5;
        int number_of_runs = 10;

        IWorldModifier stabilizer = new NaturalWorldStabilizer(50, 20000);
        DefaultPreprocessor preprocessor = new DefaultPreprocessor();

        for (int i = 0; i <= max_position_radomisation; i++) {
            System.out.println("Starting Position Stability Analysis for i = " + i);
            List<List<Predicate>> allPredicates = new LinkedList<>();
            ISceneModifier modifier = new PositionRandomizer(i, i);

            for (int n = 0; n < number_of_runs; n++) {

                Scene tmpScene = new Scene(baseScene);
                // randomisation of Input, simulation Vision inaccuracies
                modifier.apply(tmpScene);
                // standard scene processing
                preprocessor.apply(tmpScene);

                IsStableScenePredicateGenerator predGen = new IsStableScenePredicateGenerator(tmpScene, stabilizer, 1f);

                try {
                    allPredicates.add(predGen.call());
                } catch (PredicateGenerationException e) {
                    System.err.println(e.getMessage());
                    // add empty list as substitutef
                }

                System.out.println("Calculating Confidence");
                ConfidencePredicateGenerator confGen = new ConfidencePredicateGenerator(allPredicates);
                try {
                    List<Predicate> result = confGen.call();
                    System.out.println("Position Randomisation with max offset " + i + " (pixels), n = " + number_of_runs);
                    System.out.println(result);
                    System.out.println("");
                } catch (PredicateGenerationException e) {
                    System.err.println("Confidence generation failed");
                }
            }
        }
    }

        private static void examineSizeStability (Scene baseScene){
            int max_position_radomisation = 5;
            int number_of_runs = 10;

            IWorldModifier stabilizer = new NaturalWorldStabilizer(50, 20000);
            DefaultPreprocessor preprocessor = new DefaultPreprocessor();

            for (int i = 0; i <= max_position_radomisation; i++) {
                System.out.println("Starting Size Stability Analysis for i = " + i);
                List<List<Predicate>> allPredicates = new LinkedList<>();
                ISceneModifier modifier = new SizeRandomiser(i);

                for (int n = 0; n < number_of_runs; n++) {

                    Scene tmpScene = new Scene(baseScene);
                    // randomisation of Input, simulation Vision inaccuracies
                    modifier.apply(tmpScene);
                    // standard scene processing
                    preprocessor.apply(tmpScene);

                    IsStableScenePredicateGenerator predGen = new IsStableScenePredicateGenerator(tmpScene, stabilizer,
                            1f);

                    try {
                        allPredicates.add(predGen.call());
                    } catch (PredicateGenerationException e) {
                        System.err.println(e.getMessage());
                        // add empty list as substitute
                        allPredicates.add(new LinkedList<Predicate>());
                    }
                }

                System.out.println("Calculating Confidence");
                ConfidencePredicateGenerator confGen = new ConfidencePredicateGenerator(allPredicates);
                try {
                    List<Predicate> result = confGen.call();
                    System.out.println("Position Randomisation with max offset " + i + " (pixels), n = " + number_of_runs);
                    System.out.println(result);
                    System.out.println("");
                } catch (PredicateGenerationException e) {
                    System.err.println("Confidence generation failed");
                }
            }
        }

    private  static void examineSomethingForStabilityConfidence(String examinationName, Scene baseScene, float maxVirtualRuntimeMillis, int number_of_runs,ISceneModifier sceneModifier) {
        examineSomethingForStabilityConfidence(examinationName,baseScene,maxVirtualRuntimeMillis,number_of_runs,Arrays.asList(sceneModifier));
    }
    private  static void examineSomethingForStabilityConfidence(String examinationName, Scene baseScene, float maxVirtualRuntimeMillis, int number_of_runs,List<ISceneModifier> sceneModifiers){



        IWorldModifier stabilizer = new NaturalWorldStabilizer(100000, maxVirtualRuntimeMillis);
        DefaultPreprocessor preprocessor = new DefaultPreprocessor();
//            System.out.println("Stability Analysis for : " +examinationName  + " | runs= " + number_of_runs + " | maxVirtualRuntime= " + maxVirtualRuntimeMillis );
            List<List<Predicate>> allPredicates = new LinkedList<>();
//        System.out.println("Next Simulation Progress in %: " );

        for (int n = 0; n < number_of_runs; n++) {
//                if(n % 10 == 0) System.out.print((100 * n/(float)number_of_runs)+" % | ");
                Scene tmpScene = new Scene(baseScene);
                // randomisation of Input, simulation Vision inaccuracies
                for (ISceneModifier sceneModifier : sceneModifiers) {
                    sceneModifier.apply(tmpScene);
                }
                // standard scene processing
                preprocessor.apply(tmpScene);

                IsStableScenePredicateGenerator predGen = new IsStableScenePredicateGenerator(tmpScene, stabilizer, 1f);

                try {
                    allPredicates.add(predGen.call());
                } catch (PredicateGenerationException e) {
                    System.err.println(e.getMessage());
                    // add empty list as substitutef
                }
            }
//            System.out.println();
            ConfidencePredicateGenerator confGen = new ConfidencePredicateGenerator(allPredicates);
            try {
                List<Predicate> result = confGen.call();

                String outputString = "runs=" + number_of_runs + " >> ";
                 outputString += "Stability Analysis for : " + examinationName;
                outputString += ": ";
                for(ISceneModifier sceneModifier : sceneModifiers){
                    outputString += " | " +sceneModifier.toString();
                }

                outputString +=  " | Result: " + result;
//                System.out.print("Stability Analysis for : " +examinationName  + " | runs= " + number_of_runs + " | maxVirtualRuntime= " + maxVirtualRuntimeMillis );
//                System.out.print(" | Result:");
                System.out.print(outputString);
                System.out.println("");
            } catch (PredicateGenerationException e) {
                System.err.println("Confidence generation failed");
            }
//        }



    }



        private static void examinePositionPlusSizeStability (Scene baseScene){
            int max_position_radomisation = 5;
            int number_of_runs = 10;

            IWorldModifier stabilizer = new NaturalWorldStabilizer(50, 20000);
            DefaultPreprocessor preprocessor = new DefaultPreprocessor();

            for (int i = 0; i <= max_position_radomisation; i++) {
                System.out.println("Starting Position+Size Analysis for i = " + i);
                List<List<Predicate>> allPredicates = new LinkedList<>();
                ISceneModifier modifier1 = new PositionRandomizer(i, i);
                ISceneModifier modifier2 = new SizeRandomiser(i);

                for (int n = 0; n < number_of_runs; n++) {

                    Scene tmpScene = new Scene(baseScene);
                    // randomisation of Input, simulation Vision inaccuracies
                    modifier1.apply(tmpScene);
                    modifier2.apply(tmpScene);
                    // standard scene processing
                    preprocessor.apply(tmpScene);

                    IsStableScenePredicateGenerator predGen = new IsStableScenePredicateGenerator(tmpScene, stabilizer,
                            1f);

                    try {
                        allPredicates.add(predGen.call());
                    } catch (PredicateGenerationException e) {
                        System.err.println(e.getMessage());
                        // add empty list as substitute
                        allPredicates.add(new LinkedList<Predicate>());
                    }
                }

                System.out.println("Calculating Confidence");
                ConfidencePredicateGenerator confGen = new ConfidencePredicateGenerator(allPredicates);
                try {
                    List<Predicate> result = confGen.call();
                    System.out.println("Position Randomisation with max offset " + i + " (pixels), n = " + number_of_runs);
                    System.out.println(result);
                    System.out.println("");
                } catch (PredicateGenerationException e) {
                    System.err.println("Confidence generation failed");
                }
            }
        }
    }
