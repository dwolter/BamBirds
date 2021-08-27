package de.uniba.sme.bambirds.planner.physicssimulation.predicategeneration;

import java.util.*;

import de.uniba.sme.bambirds.planner.predicates.IPredicateGenerator;
import de.uniba.sme.bambirds.planner.predicates.Predicate;
import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;
    

import de.uniba.sme.bambirds.planner.physicssimulation.debugging.VisualSimulationDebugger;
import de.uniba.sme.bambirds.planner.physicssimulation.exceptions.NoStableSceneException;
import de.uniba.sme.bambirds.planner.physicssimulation.exceptions.PredicateGenerationException;
import de.uniba.sme.bambirds.planner.physicssimulation.physics.Simulation;
import de.uniba.sme.bambirds.planner.physicssimulation.physics.SimulationExecutor;
import de.uniba.sme.bambirds.planner.physicssimulation.physics.modification.manipulation.BlockRemover;
import de.uniba.sme.bambirds.planner.physicssimulation.physics.modification.stability.AdvancedStabilizerV2;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.Scene;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.entities.SceneEntityBase;


/**Generates Predicates in Form of supports(A,B) > meaning that the absence of the block-object with the globalID "A" in its current position leads to a significant change of the position and/or angle of the block-object with the globalID "B". */
public class SupportPredicateGenerator implements IPredicateGenerator {

    private static final Logger log = LogManager.getLogger(SupportPredicateGenerator.class);

    /** The amount of Simulations to run for EACH possible pair of objects */
    private int numOfSimulation = 1;

    private Scene initialScene;
    private Scene stableScene;

    private List<String> blockIDs = new LinkedList<>();

    private Set<Predicate> predicates = new HashSet<>();
    
    private ArrayList<Simulation> allSimulations = new ArrayList<Simulation>(); 

    private SimulationExecutor executor;


    /**when an objects angle is being rotated more than this angle ( in radians! ) during the simulation, then the object counts as "supported" by the supporter */
    private float angleMargin = 0.2618f; // ~ 15°
    /**when an objects position is being displaced more than this length during the simulation, then the object counts as "supported" by the supporter */
    private float positionMargin = 1.0f; // 


    /**
     * This Predicate Generator attempts to identify sets of entities in a scene
     * which act as supporters and supported. Affecting a Supporter-Entity should
     * have significant physical impact on its Supported entitities with regards to their angle or position.
     * 
     * @param scene The Input-Scene
     */
    public SupportPredicateGenerator(Scene scene) {
        super();
        this.initialScene = scene;
        executor = SimulationExecutor.getInstance();

        // TODO: Maybe filter relevant blocks to reduce number of simulations
        for (SceneEntityBase entity : scene.getAllBlocks()) {
            blockIDs.add(entity.getGlobalID());
        }
    }

    @Override
    public List<Predicate> call() throws PredicateGenerationException {

        long initialTimeMillis = System.currentTimeMillis();

        try {
            stableScene = createStableInitialScene();
            log.debug("Scene Stabilized Succesfully");

            for (String blockID : blockIDs) {
                for (int i = 0; i < numOfSimulation; i++) {
                    Simulation simulation = new Simulation(blockID , stableScene);

                    simulation.addPreparationPhaseModifier(new BlockRemover(blockID));

                    allSimulations.add(simulation);
                }
            }
            // log.debug("Running Simulation: Support Predicate for Block " + blockID);
            executor.runSimulations(allSimulations);

            boolean simulating;
            do {
                try {
                    Thread.sleep(50);
                } catch (InterruptedException e) {
                    log.warn("Got interrupted while generating predicates. Returning empty result");
                    executor.cancel();
                    return new ArrayList<>();
                }
                simulating = false;
                for (Simulation simulation : allSimulations) {
                    if (simulation.isFinsihed()) {
                        if (simulation.isSuccessful()) {
                            evaluate(simulation.getFinalSnapshot(), simulation.getName());
                            log.debug("finished simulation: " + simulation.getName());

                        } else {
                            // ignore?
                        }
                    } else {
                        simulating = true;
                    }
                }
            } while (simulating);
            // log.debug("Finished Simulation: Support Predicate for Block " + blockID);
        
        } catch (NoStableSceneException e) {
            throw new PredicateGenerationException("Could not Produce any Predicates", e);
            // log.debug("Could not Produce any Predicates because scene was not stable: " + e);
        }
        
        long duration = System.currentTimeMillis() - initialTimeMillis;
        log.debug("Finished Simulation for \"Support Predicate\" in " + duration + " (ms).");
        
        return new ArrayList<>(predicates);
    }

    
    @Override
    public void addSimulationsToVisualSimulationDebugger(String testbedNamePrefix){
        for(Simulation simulation : getAllSimulations()){
            for (int i = 0; i < simulation.getAllSnapShots().size(); i++) {
                Scene sceneSnapshot = simulation.getAllSnapShots().get(i);
                VisualSimulationDebugger.addScene(testbedNamePrefix + simulation.getName()+ " : "+i+" " + sceneSnapshot.getLastAppliedModifierName() , sceneSnapshot, simulation.getSettings());
            }
        }
    }


    //**perform some logic to extract knowledge based on the generated scene-data */
    private void evaluate(Scene finalScene, String blockID) {

        for (SceneEntityBase entity : finalScene.getAllBlocks()) {
            SceneEntityBase before = stableScene.getAllEntitiesMap().get(entity.getId());
            if (hasMoved(before, entity)) {
                Predicate predicate = createSupportsPredicate(blockID, entity.getGlobalID());

                predicates.add(predicate);
            }
        }
    }

    /**
     * create the final predicate string
     * @param id1 the name of the supporter
     * @param id2 the name supported
     * @return the created predicate-string
     */
    private Predicate createSupportsPredicate(String id1, String id2) {
        return new Predicate("supports", id1, id2);
    }


    /**
     * determines if an Entity has changed its rotation or its position enough to be qualified as suported for the supports-predicate
     */
    private boolean hasMoved(SceneEntityBase entityBeforeSimulation, SceneEntityBase entityAfterSimulation) {
        float maxRotation;
        float minRotation;
        if (entityBeforeSimulation.getAngle() + getAngleMargin() > ((float) Math.PI * 2f)) {
            maxRotation = (entityBeforeSimulation.getAngle() + getAngleMargin()) - ((float) Math.PI * 2f); 
        } else {
            maxRotation = entityBeforeSimulation.getAngle() + getAngleMargin();
        }

        if (entityBeforeSimulation.getAngle() - getAngleMargin() < 0) {
            minRotation = ((float) Math.PI * 2f) - (Math.abs(entityBeforeSimulation.getAngle() - getAngleMargin()));
        } else {
            minRotation = entityBeforeSimulation.getAngle() - getAngleMargin();
        }
        boolean overRotated;
        if (minRotation < maxRotation) {
            overRotated =   !(  entityAfterSimulation.getAngle() < maxRotation
                            &&  entityAfterSimulation.getAngle() > minRotation);
        } else {
            overRotated =   !( (entityAfterSimulation.getAngle() < maxRotation
                            &&  entityAfterSimulation.getAngle() >= 0f)
                            || (entityAfterSimulation.getAngle() > minRotation
                            && entityAfterSimulation.getAngle() <= 2f * (float) Math.PI));
        }
 
        return  overRotated
            ||  (entityAfterSimulation.getCenterY() < entityBeforeSimulation.getCenterY() - getPositionMargin())
            ||  (entityAfterSimulation.getCenterY() > entityBeforeSimulation.getCenterY() + getPositionMargin())
            ||  (entityAfterSimulation.getCenterX() < entityBeforeSimulation.getCenterX() - getPositionMargin())
            ||  (entityAfterSimulation.getCenterX() > entityBeforeSimulation.getCenterX() + getPositionMargin());
    }

    /**
     *  Try to Stabilize the initial Scene
     * @throws NoStableSceneException
     */
    private Scene createStableInitialScene() throws NoStableSceneException {
        log.debug("createStableScene()");

        Simulation sim = new Simulation("Stability Check", initialScene);
        sim.addPreparationPhaseModifier(new AdvancedStabilizerV2());
        
        executor.runSimulation(sim);
        boolean isFinsihed = false;
        while (!isFinsihed) {
            try {
                Thread.sleep(10);
            } catch (InterruptedException e) {
            }
            isFinsihed = sim.isFinsihed();
        }
        if (sim.isSuccessful()) {
            log.debug("sim.succesful = true");
            return sim.getFinalSnapshot();
        } else {
            throw new NoStableSceneException("Could not create stable pysical scene for simulation");
        }
    }

	public ArrayList<Simulation> getAllSimulations() {
		return allSimulations;
	}

	public void setAllSimulations(ArrayList<Simulation> collectedSimulations) {
		this.allSimulations = collectedSimulations;
	}

	public float getAngleMargin() {
		return angleMargin;
	}

	public void setAngleMargin(float angleMargin) {
		this.angleMargin = angleMargin;
	}

	public float getPositionMargin() {
		return positionMargin;
	}

	public void setPositionMargin(float positionMargin) {
		this.positionMargin = positionMargin;
	}

}