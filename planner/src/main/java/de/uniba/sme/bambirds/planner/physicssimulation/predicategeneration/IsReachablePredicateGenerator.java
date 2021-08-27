package de.uniba.sme.bambirds.planner.physicssimulation.predicategeneration;


import java.util.*;

import de.uniba.sme.bambirds.planner.predicates.IPredicateGenerator;
import de.uniba.sme.bambirds.planner.predicates.Predicate;
import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;

import de.uniba.sme.bambirds.common.objects.ab.ABType;
import de.uniba.sme.bambirds.planner.physicssimulation.debugging.VisualSimulationDebugger;
import de.uniba.sme.bambirds.planner.physicssimulation.exceptions.NoStableSceneException;
import de.uniba.sme.bambirds.planner.physicssimulation.exceptions.PredicateGenerationException;
import de.uniba.sme.bambirds.planner.physicssimulation.physics.Simulation;
import de.uniba.sme.bambirds.planner.physicssimulation.physics.SimulationExecutor;
import de.uniba.sme.bambirds.planner.physicssimulation.physics.modification.shooting.Shooter;
import de.uniba.sme.bambirds.planner.physicssimulation.physics.modification.stability.AdvancedStabilizerV2;
import de.uniba.sme.bambirds.planner.physicssimulation.physics.modification.tracking.CollisionInfo;
import de.uniba.sme.bambirds.planner.physicssimulation.physics.modification.tracking.CollisionTracker;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.Scene;

public class IsReachablePredicateGenerator implements IPredicateGenerator {
    private static final Logger log = LogManager.getLogger(IsReachablePredicateGenerator.class);
	
	/** The amount of Simulations to run for EACH shot */
	private int numOfSimulation = 1;
	
	private Scene initialScene;
    private Scene stableScene;

    private float shootingAngleMin = 0f;
    private float shootingAngleMax = 90f;
    private float shootingAngleStepSize = 5f;
    private float shootingStrengthMin = 0.1f;
    private float shootingStrengthMax = 1f;
    private float shootingStrengthStepSize = 0.1f;

    private Set<Predicate> predicates = new HashSet<>();
    
    private ArrayList<Simulation> allSimulations = new ArrayList<Simulation>(); 

    private SimulationExecutor executor;

	private boolean generateHighDetailResultPredicates = false;

	public IsReachablePredicateGenerator(Scene scene, boolean generateHighDetailResultPredicates) {
		super();
		this.initialScene = scene;
		this.generateHighDetailResultPredicates = generateHighDetailResultPredicates;
		executor = SimulationExecutor.getInstance();
	}

	public IsReachablePredicateGenerator(Scene scene, boolean generateHighDetailResultPredicates, float shootingAngleMin, float shootingAngleMax, float shootingAngleStepSize,
			float shootingStrengthMin, float shootingStrengthMax, float shootingStrengthStepSize
			) {
		super();
		this.initialScene = scene;
		this.generateHighDetailResultPredicates = generateHighDetailResultPredicates;
		this.shootingAngleMin = shootingAngleMin;
		this.shootingAngleMax = shootingAngleMax;
		this.shootingAngleStepSize = shootingAngleStepSize;
		this.shootingStrengthMin = shootingStrengthMin;
		this.shootingStrengthMax = shootingStrengthMax;
		this.shootingStrengthStepSize = shootingStrengthStepSize;
		this.generateHighDetailResultPredicates = generateHighDetailResultPredicates;
		executor = SimulationExecutor.getInstance();
	}

	@Override
	public List<Predicate> call() throws PredicateGenerationException {
		log.debug(" getPredicates()");

        long initialTimeMillis = System.currentTimeMillis();

        try {
            stableScene = createStableInitialScene();
            log.debug("Scene Stabilized Successfully");
			
//            for (float shootingAngle = 0f; shootingAngle <= 90f; shootingAngle += 1f) {
            for (float shootingAngle = shootingAngleMin; shootingAngle <= shootingAngleMax; shootingAngle += shootingAngleStepSize) {

//				for(float shootingStrenght = 0.1f; shootingStrenght <= 1f; shootingStrenght += 0.1f)
				for(float shootingStrenght = shootingStrengthMin; shootingStrenght <= shootingStrengthMax; shootingStrenght += shootingStrengthStepSize)

					for (int i = 0; i < numOfSimulation; i++) {

                        String simulationName = "Angle="+String.format(Locale.US,"%.2f",shootingAngle )+", "+ "Strength=" +String.format(Locale.US,"%.2f", shootingStrenght);
                        Simulation simulation = new Simulation(simulationName, stableScene);

                        simulation.addPreparationPhaseModifier(new Shooter(ABType.RedBird,"TestshotRedbird", shootingAngle, shootingStrenght, 10000f));
                        simulation.addPreStepModifier(new CollisionTracker("TestshotRedbird"));

						allSimulations.add(simulation);
					}
            }
            executor.runSimulations(allSimulations);

          


            //run simulations in threads ---------------------------------------------------------------------
            boolean simulating;
            do {
                try {
                    Thread.sleep(50);
                } catch (InterruptedException e) {
                    // Ignore
                }
                simulating = false;
                for (Simulation simulation : allSimulations) {
                    if (simulation.isFinsihed()) {
                        if (simulation.isSuccessful()) {
                            // evaluate(simulation.getFinalSnapshot(), simulation.getName());
                            // log.debug("finished simulation: " + simulation.getName());

                        } else {
                            // TODO handle failure
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
        //finished simulations in threads ---------------------------------------------------------------------
        
        log.debug("generating");
        // TODO > GENERATE PREDICATES OUT OF THE COLLECTED DATA 
        for (Simulation simulation : allSimulations) {
            
            CollisionTracker collisionTracker = (CollisionTracker) simulation.getPreStepModifiers().get(0);

            if(generateHighDetailResultPredicates){
            	for (CollisionInfo collisionInfo : collisionTracker.getTrackedCollisions()) {
					Predicate predicate = new Predicate("isReachable",  collisionInfo.selfGlobalID,
                            collisionInfo.otherGlobalID, simulation.getName(),
                            "impact{impactPosition:"+collisionInfo.position + ", " +"impactImpulse:"+
                                    collisionInfo.impulse + ", " + "frame:"+ collisionInfo.frame+"}");
					predicates.add(predicate);
				}
			}else{
				for (CollisionInfo collisionInfo : collisionTracker.getTrackedCollisions()) {
//					String predicate = "isReachable("+ collisionInfo.selfGlobalID+ ", " + collisionInfo.otherGlobalID + ")";
					Predicate predicate = new Predicate("isReachable", collisionInfo.selfGlobalID, collisionInfo.otherGlobalID, simulation.getName());
                    predicates.add(predicate);
				}
			}
        }

        long duration = System.currentTimeMillis() - initialTimeMillis;
        log.debug("Finished Simulation for \"isReachable-Predicate\" in " + duration + " (ms).");
        
        return new ArrayList<>(predicates);
    
	}

	@Override
	public void addSimulationsToVisualSimulationDebugger(String testbedNamePrefix) {
		for(Simulation simulation : getAllSimulations()){
            for (int i = 0; i < simulation.getAllSnapShots().size(); i++) {
                Scene sceneSnapshot = simulation.getAllSnapShots().get(i);
                VisualSimulationDebugger.addScene(testbedNamePrefix + simulation.getName()+ " : "+i+" " + sceneSnapshot.getLastAppliedModifierName() , sceneSnapshot, simulation.getSettings());
            }
        }
	}



	/**
     *  Try to Stabilize the initial Scene
     * @throws NoStableSceneException
     */
    private Scene createStableInitialScene() throws NoStableSceneException {
        log.debug("createStableScene()");

        Simulation sim = new Simulation("Stabilization Phase", initialScene);
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
    
}
