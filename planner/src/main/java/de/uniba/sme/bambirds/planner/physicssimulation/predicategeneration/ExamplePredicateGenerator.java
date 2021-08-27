package de.uniba.sme.bambirds.planner.physicssimulation.predicategeneration;

import java.util.LinkedList;
import java.util.List;

import de.uniba.sme.bambirds.planner.physicssimulation.exceptions.PredicateGenerationException;
import de.uniba.sme.bambirds.planner.physicssimulation.physics.Simulation;
import de.uniba.sme.bambirds.planner.physicssimulation.physics.SimulationExecutor;
import de.uniba.sme.bambirds.planner.physicssimulation.physics.modification.manipulation.ExampleWorldModifier;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.Scene;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.modification.ISceneModifier;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.modification.insertion.ExampleSceneModifier;
import de.uniba.sme.bambirds.planner.predicates.IPredicateGenerator;
import de.uniba.sme.bambirds.planner.predicates.Predicate;

public class ExamplePredicateGenerator implements IPredicateGenerator {

    private Scene scene;

    public ExamplePredicateGenerator(Scene sceneFromVison) {
        scene = sceneFromVison;
    }

    @Override
    public List<Predicate> call() throws PredicateGenerationException {
        
        // 1) Scene Pre Processing
        ISceneModifier myModifier = new ExampleSceneModifier();
        myModifier.apply(scene);
        
        // 2) Execution of Simulation
        SimulationExecutor executor = SimulationExecutor.getInstance();

        Simulation mySimulation = new Simulation("MySimulation", scene);
        mySimulation.addPreparationPhaseModifier(new ExampleWorldModifier());
        mySimulation.addPreStepModifier(new ExampleWorldModifier());
        mySimulation.addPostStepModifier(new ExampleWorldModifier());
        mySimulation.addEndPhaseModifier(new ExampleWorldModifier());

        executor.runSimulation(mySimulation);

        // 3) Evaluation
        boolean simulating = true;
        List<Predicate> predicates = new LinkedList<>();
        do {
            try{
                Thread.sleep(50);
            } catch (InterruptedException ignore) {

            }
            if (mySimulation.isFinsihed()) {
                if(mySimulation.isSuccessful()) {
                    predicates = doSomeEvaluation(mySimulation);
                } else {
                    // Something went wrong - what to do?
                }
            }
        } while (simulating);
        return predicates;
    }

    @Override
    public void addSimulationsToVisualSimulationDebugger(String testbedNamePrefix) {
        // Let's ignore that
    }

    private List<Predicate> doSomeEvaluation(Simulation simulation) {
        return new LinkedList<>();
    }
    
}