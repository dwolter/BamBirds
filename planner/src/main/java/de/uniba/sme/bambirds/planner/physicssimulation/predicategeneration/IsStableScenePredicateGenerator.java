package de.uniba.sme.bambirds.planner.physicssimulation.predicategeneration;

import java.util.LinkedList;
import java.util.List;

import de.uniba.sme.bambirds.planner.physicssimulation.debugging.VisualSimulationDebugger;
import de.uniba.sme.bambirds.planner.physicssimulation.exceptions.PredicateGenerationException;
import de.uniba.sme.bambirds.planner.physicssimulation.physics.Simulation;
import de.uniba.sme.bambirds.planner.physicssimulation.physics.SimulationExecutor;
import de.uniba.sme.bambirds.planner.physicssimulation.physics.modification.IWorldModifier;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.Scene;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.entities.SceneEntityBase;

import de.uniba.sme.bambirds.planner.predicates.IPredicateGenerator;
import de.uniba.sme.bambirds.planner.predicates.Predicate;
import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;

/**
 * Returns the predicate "isStable" if the given Scene after the application of the 
 * IWorldModifier results in a similar JBox2D world representation compared to the original input.
 * (given the predefined margins - only checks position margins)
 * 
 * I.e. does the Scene initialisazies properly oder do the strzuctures fall apart? 
 * (for example due to falsee input from the vision)
 */
public class IsStableScenePredicateGenerator implements IPredicateGenerator {

    private static final Logger log = LogManager.getLogger(IsStableScenePredicateGenerator.class);

    private SimulationExecutor executor;
    private Simulation simulation;

    private Scene initialScene;
    private IWorldModifier worldStabilizer;

    private float positionMargin;

    public IsStableScenePredicateGenerator(Scene initialScene, IWorldModifier worldStabilizer, float positionMargin) {
        executor = SimulationExecutor.getInstance();
        this.initialScene = initialScene;
        this.worldStabilizer = worldStabilizer;
        this.positionMargin = positionMargin;
	}


	@Override
	public List<Predicate> call() throws PredicateGenerationException {

        List<Predicate> predicates = new LinkedList<>();
        
        simulation = new Simulation("Stability Check", initialScene);
        simulation.addPreparationPhaseModifier(this.worldStabilizer);
        executor.runSimulation(simulation);
        boolean isFinsihed = simulation.isFinsihed();
        while (!isFinsihed) {
            try {
                Thread.sleep(10);
            } catch (InterruptedException e) {
            }
            isFinsihed = simulation.isFinsihed();
        }
        if (simulation.isSuccessful()) {
            Scene finalScene = simulation.getFinalSnapshot();
            if (isSimilarToInitialScene(finalScene)) {
                predicates.add(new Predicate("isStable"));
            }
        }
        return predicates;
	}

	@Override
	public void addSimulationsToVisualSimulationDebugger(String testbedNamePrefix) {
        VisualSimulationDebugger.addScene(testbedNamePrefix + simulation.getName()+ " Before "   , simulation.getInitialSnapshot(), simulation.getSettings());
        VisualSimulationDebugger.addScene(testbedNamePrefix + simulation.getName()+ " After  "   , simulation.getFinalSnapshot(), simulation.getSettings());
    }
    
    private boolean isSimilarToInitialScene(Scene finalScene) {
        for (SceneEntityBase initialEntity: initialScene.getAllBlocks()) {
            for (SceneEntityBase finalEntity: finalScene.getAllBlocks()) {
                if (initialEntity.getGlobalID().equals(finalEntity.getGlobalID())) {
                    float xOffset = Math.abs(initialEntity.getCenterX() - finalEntity.getCenterX());
                    float yOffset = Math.abs(initialEntity.getCenterY() - finalEntity.getCenterY());

                    if (xOffset > positionMargin || yOffset > positionMargin) {
                        return false;
                    }
                }
            }
        }
        return true;
    }



    
}
