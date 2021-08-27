package de.uniba.sme.bambirds.planner.physicssimulation.sceneunderstanding;

import java.util.List;
import java.util.stream.Collectors;

import de.uniba.sme.bambirds.planner.physicssimulation.physics.Simulation;
import de.uniba.sme.bambirds.planner.physicssimulation.physics.SimulationSettings;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.Scene;
import de.uniba.sme.bambirds.planner.physicssimulation.sceneunderstanding.SceneDistanceMetrics.DistanceMetric;
import de.uniba.sme.bambirds.planner.physicssimulation.sceneunderstanding.modificationtree.ModificationTree;
import de.uniba.sme.bambirds.planner.physicssimulation.sceneunderstanding.modificationtree.ModificationTreeNode;
import de.uniba.sme.bambirds.planner.physicssimulation.sceneunderstanding.propertymodification.PropertyState;
import de.uniba.sme.bambirds.planner.physicssimulation.sceneunderstanding.utils.StepwisePropertyManipulator;

public class GreedyIncrementalConverger implements SceneUnderStanding {


    private Simulation realWorld;

    private Simulation initialMentalModel;

    private Simulation finalMentalModel;

    private Scene finalRefScene;

    private int maxIterationDepth;
    private int modificationStepsPerIteration;

    private ModificationTree modificationTree;

    float currentDistance = Float.MAX_VALUE;


    private List<StepwisePropertyManipulator> propertyManipulators;

    private DistanceMetric metric;

    private Scene initialScene;
    private SimulationSettings initialSettings;


    public Simulation getFinalMentalmodel() {
        return finalMentalModel;
    }

    public GreedyIncrementalConverger(Simulation initialMentalModel,
                                        Simulation realWorld,
                                        List<StepwisePropertyManipulator> propertyManipulators,
                                        DistanceMetric metric,
                                        int maxIterationDepth,
                                        int modificationStepsPerIteration) {
        this.initialMentalModel = initialMentalModel;
        initialScene = initialMentalModel.getScene();
        initialSettings = initialMentalModel.getSettings();

        this.realWorld = realWorld;
        finalRefScene = realWorld.getFinalSnapshot();

        this.maxIterationDepth = maxIterationDepth;
        this.modificationStepsPerIteration = modificationStepsPerIteration;

        this.propertyManipulators = propertyManipulators;

        this.metric = metric;

        modificationTree = new ModificationTree();
    }

    @Override
    public Simulation convergeMentalModel() {
        modificationTree = new ModificationTree();
        ModificationTreeNode currentNode = modificationTree.getRoot();

        currentDistance = SceneDistanceMetrics.calculateDistance(finalRefScene,
                                        initialMentalModel.getFinalSnapshot(), metric);

        modificationTree.setRootValue(currentDistance);

        for (int i = 1; i <= maxIterationDepth; i++) {

            System.out.println("====================================");
            System.out.println("Iteration " + i +":");
            System.out.println("\tCurrent Distance:\t" + currentDistance);

            for (StepwisePropertyManipulator manipulator: propertyManipulators) {

                Simulation refSim = new Simulation("ref", new Scene(initialScene), initialSettings.getCopy());
                currentNode.applyEntireBranch(refSim);
                manipulator.setCurrentValueAccordingTo(refSim);

                float initialValue = manipulator.getCurrentPropertyState().getValue();

                manipulator.decrementNtimes((modificationStepsPerIteration/2)+1);
                for (int j = -modificationStepsPerIteration/2; j < modificationStepsPerIteration/2+1; j++) {
                    manipulator.increment();

                    //noinspection PointlessBooleanExpression
                    if (manipulator.getCurrentPropertyState().getValue() == initialValue ) {
                        continue;
                    }
                    if(manipulator.isBelowLimitRange()){
                    	continue;
					}
                    if(manipulator.isAboveLimitRange()){
                    	break;
					}

                    Simulation tmp = new Simulation("tmp", new Scene(initialScene), initialSettings.getCopy());
                    currentNode.applyEntireBranch(tmp);

                    PropertyState newState = manipulator.getCurrentPropertyState();
                    newState.applyTo(tmp);

                    runAndWaitUntilFinished(tmp);

                    float distance = SceneDistanceMetrics.calculateDistance(finalRefScene, tmp.getFinalSnapshot(), metric);
                    currentNode.addChild(newState, distance);
                }

            }

            ModificationTreeNode nextNodeToExpand = currentNode.getChildWithLowestValue();
            if (currentDistance <= nextNodeToExpand.getValue()) {
                finalMentalModel = new Simulation("Final Mental Model", new Scene(initialScene), initialSettings.getCopy());
                currentNode.applyEntireBranch(finalMentalModel);
                runAndWaitUntilFinished(finalMentalModel);
                finalMentalModel.addToVisualSimulationDebugger(true);

                System.out.println("\tNo Property changed (no improvement found)");
                System.out.println("\tNext Best Distance in current iteration:\t" + nextNodeToExpand.getValue());
                currentNode.printBranchModificationStack();
                return finalMentalModel;
            } else {
                System.out.println("\tProperty Changed to: \t" + nextNodeToExpand.getPropertyState());
                System.out.println("\tNew Distance:\t\t" + nextNodeToExpand.getValue());
                currentNode = nextNodeToExpand;
                currentDistance = currentNode.getValue();


                //for visuals
                Simulation theChosenOne = new Simulation(currentNode.getPropertyState().toString() + ": " + currentDistance, new Scene(initialScene), initialSettings.getCopy());
                currentNode.applyEntireBranch(theChosenOne);
                runAndWaitUntilFinished(theChosenOne);
//                VisualSimulationDebugger.addScene(theChosenOne.getName() + "final",theChosenOne.getFinalSnapshot(),theChosenOne.getSettings());
                theChosenOne.addToVisualSimulationDebugger(false);

                System.out.println("<<<< RESULT AFTER >>>>");
                for(StepwisePropertyManipulator stepwisePropertyManipulator : propertyManipulators){
                    System.out.println(stepwisePropertyManipulator.getCurrentPropertyState().getModifier() + " : " + stepwisePropertyManipulator.getCurrentPropertyState().getModifier().getCurrentPropertyValue(theChosenOne));
                }
                System.out.println("<<<< ------ >>>>");
            }

        }
        //Max iteration (depth) reached
        finalMentalModel = new Simulation("Final Mental Model", new Scene(initialScene), initialSettings.getCopy());
        currentNode.applyEntireBranch(finalMentalModel);
        runAndWaitUntilFinished(finalMentalModel);
        finalMentalModel.addToVisualSimulationDebugger(true);
        currentNode.printBranchModificationStack();
        return finalMentalModel;
    }

    public  void printResult(){
        System.out.println("<<<< RESULT >>>>");
        for(StepwisePropertyManipulator stepwisePropertyManipulator : propertyManipulators){
            System.out.println(stepwisePropertyManipulator.getCurrentPropertyState().getModifier() + " : " + stepwisePropertyManipulator.getCurrentPropertyState().getModifier().getCurrentPropertyValue(finalMentalModel));
        }
        System.out.println("----------------");
        System.out.println("DISTANCE: " + currentDistance);
        System.out.println("<<<< ------ >>>>");
    }

    private void runAndWaitUntilFinished(Simulation simulation) {
        simulation.run();
        while (!simulation.isFinsihed()) {
            try {
                Thread.sleep(10);
            } catch (InterruptedException e) {
                //ignore
            }
        }
    }

}