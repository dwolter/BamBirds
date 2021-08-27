package de.uniba.sme.bambirds.planner.physicssimulation.physics;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

import org.jbox2d.dynamics.Body;
import org.jbox2d.dynamics.BodyType;
import org.jbox2d.dynamics.World;

import de.uniba.sme.bambirds.planner.physicssimulation.JWorld;
import de.uniba.sme.bambirds.planner.physicssimulation.debugging.VisualSimulationDebugger;
import de.uniba.sme.bambirds.planner.physicssimulation.exceptions.ModificationException;
import de.uniba.sme.bambirds.planner.physicssimulation.physics.modification.IWorldModifier;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.Scene;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.entities.SceneEntity;

import org.apache.log4j.*;
import org.apache.logging.log4j.core.lookup.SystemPropertiesLookup;



/** Is a Container for a everything related to setting up and executing a Physics-Simulation.
 * 
 *  It will run a Simulation-World as a box2d Simulation, apply the settings,
stabilize the Simulation, manipulate it by applying any modifiers and then let it simulate for a limitied time ( which is defined in the SimulationSsettings-Object).
During Execution it will make World-Snapshots (by creating Scene-Objects from the world) at specific timepoints ( before start, after application of modifiers, after stabilization and at the end). These World-Snapshots can then later be accessed for reasoning-purposes/ loading it into the VisualSimulationDebugger or whatever else you may need to do.
*/
public class Simulation implements Runnable {

    private static final Logger log = LogManager.getLogger(Simulation.class);

    private String name;

    private JWorld world;

    private SimulationSettings settings;

    private Scene scene;

    private List<IWorldModifier> preparationPhaseModifiers = new LinkedList<>();
    private List<IWorldModifier> preStepModifiers = new LinkedList<>();
    private List<IWorldModifier> postStepModifiers = new LinkedList<>();
    private List<IWorldModifier> endPhaseModifiers = new LinkedList<>();

    private List<Scene> preparationPhaseSnapshots = new LinkedList<>();
    private List<Scene> simulationPhaseSnapshots = new LinkedList<>();
    private List<Scene> endPhaseSnapshots = new LinkedList<>();

    private Scene initialSnapshot;
    private Scene finalSnapshot;

    private boolean isFinished = false;
    private boolean isSuccessful = false;

    static {
        // log.setLevel(LogManager.getRootLogger().getLevel());
    }


    /**
     * Constitutes one specific Setup of a Simulation, that can then be executed. It
     * is meant to capture snapshots of the simulation during its execution.
     * Afterwards these snapshots can be accessed and evaluated for further
     * reasoning
     * 
     * @param scene
     */
    public Simulation(String name, Scene scene) {
        settings = new SimulationSettings();
        world = new JWorld(settings.getGravity(), scene.slingShotCenterPosition);
        world.isAllowSleep();
        this.name = name;

        //Copy the scene before we potentially make manipulations of the scene
        // (e.g. applying damage & destroying objects)
        this.scene = new Scene(scene);
    }

        /**
     * Constitutes one specific Setup of a Simulation, that can then be executed. It
     * is meant to capture snapshots of the simulation during its execution.
     * Afterwards these snapshots can be accessed and evaluated for further
     * reasoning
     * 
     * @param scene
     */
    public Simulation(String name, Scene scene, SimulationSettings settings) {
        this.settings = settings;
        world = new JWorld(settings.getGravity(), scene.slingShotCenterPosition);

        world.isAllowSleep();
        this.name = name;

        //Copy the scene before we potentially make manipulations of the scene
        // (e.g. applying damage & destroying objects)
        this.scene = new Scene(scene);
    }


    public void addPreparationPhaseModifier(IWorldModifier modifier) {
        preparationPhaseModifiers.add(modifier);
    }

    public void addPreStepModifier(IWorldModifier modifier) {
        preStepModifiers.add(modifier);
    }

    public void addPostStepModifier(IWorldModifier modifier) {
        postStepModifiers.add(modifier);
    }

    public void addEndPhaseModifier(IWorldModifier modifier) {
        endPhaseModifiers.add(modifier);
    }

    @Override
    public void run() {
        log.debug(name + ": Starting Simulation Process.");

        scene.populateWorld(world);

        log.debug(name + ": Creating Initial Snapshot");
        initialSnapshot = new Scene(world, scene.getEntityPropertiesLUT().getCopy());
        try {

            log.debug(name + ": Running Preparation Phase");
            runPreparationPhase();
    
            log.debug(name + ": Running Simulation Phase.");
            runSimulationPhase();
    
            log.debug(name + ": Running End Phase.");
            runEndPhase();

            isSuccessful = true;
        } catch (ModificationException e) {
            isSuccessful = false;
        }

        finalSnapshot = new Scene(world, scene.getEntityPropertiesLUT().getCopy());
        finalSnapshot.setLastAppliedModifierName("FINAL");
        isFinished = true;
        log.debug(name + ": Done.");
    }
    
    private void runPreparationPhase() throws ModificationException {
        for (IWorldModifier worldmodifier : preparationPhaseModifiers) {
            log.debug(name + ": Applying " + worldmodifier.getName());
            worldmodifier.apply(world,this, settings);
            
            Scene sceneSnapshot = new Scene(world,scene.getEntityPropertiesLUT().getCopy());
            sceneSnapshot.setLastAppliedModifierName(worldmodifier.getName()); 
            this.preparationPhaseSnapshots.add(sceneSnapshot);
        }
    }


    private void runSimulationPhase() throws ModificationException {
        int stepCounter = 0;
        long startMillis = System.currentTimeMillis();
        float virtualTimePassedInMillis = 0;

        world.setContinuousPhysics(true);
        world.setGravity(settings.getGravity());
        world.setWarmStarting(true);
        world.setSubStepping(false);

        //Do one step to apply changes, seems important
        world.step(settings.getDeltaTime(), settings.getVelocityIterations(), settings.getPositionIterations());

        while(!isStable()) {
            for (IWorldModifier modifier : preStepModifiers) {
                modifier.apply(world,this, settings);
            }
    
            world.step(settings.getDeltaTime(), settings.getVelocityIterations(), settings.getPositionIterations());
    
            for (IWorldModifier modifier : postStepModifiers) {
                modifier.apply(world,this, settings);
            }

            virtualTimePassedInMillis += settings.getDeltaTime() * 1000;
            stepCounter += 1;

            if (virtualTimePassedInMillis > settings.getMaxRuntimeVirtualMillis()) {
                //Cancel Simulation
                log.debug(name + ": Simulation FAILED > Simulation Steps executed = " + stepCounter + " ; realtime passed = "+ (System.currentTimeMillis() - startMillis )+ " ; virtual passed =  " + virtualTimePassedInMillis );
                isFinished = true;
                isSuccessful = false;
                finalSnapshot = new Scene(world,scene.getEntityPropertiesLUT().getCopy());
                return;
            }

            if (settings.isDoIntermediateSnapshots() && (stepCounter % settings.getIntermediateSnapshotDelta() == 0)) {
                simulationPhaseSnapshots.add(new Scene(world,scene.getEntityPropertiesLUT().getCopy()));
            }
        }

        log.debug(name + ": Simulation Phase Data:");
        log.debug(name + ": MaxRuntimeMillis= " + settings.getMaxRuntimeVirtualMillis());
        log.debug(name + ": Deltatime= " + settings.getDeltaTime());
        log.debug(name + ": Simulation Steps executed = " + stepCounter + "; realtime passed = "+ (System.currentTimeMillis() - startMillis )+ " ; virtual passed =  " + virtualTimePassedInMillis );
    }

    private void runEndPhase() throws ModificationException {
        for (IWorldModifier worldmodifier : endPhaseModifiers) {
            log.debug(name + ": Applying " + worldmodifier.getName());
            worldmodifier.apply(world,this, settings);
            
            Scene sceneSnapshot = new Scene(world,scene.getEntityPropertiesLUT().getCopy());
            sceneSnapshot.setLastAppliedModifierName(worldmodifier.getName()); 
            endPhaseSnapshots.add(sceneSnapshot);
            
        }
    }


    /**
     * We consider a Scene as stable (here) iff no dynamic body is awake anymore.
     */
    private boolean isStable() {
        Body body = world.getBodyList();
        while (body != null) {
            if (body.getType() == BodyType.DYNAMIC &&  body.isAwake()) {
                return false;
            }
            body = body.getNext();
        }
        return true;
    }

    public void addToVisualSimulationDebugger(boolean includeIntermediateScenes){
        VisualSimulationDebugger.addScene(name + " (initial)", this.getInitialSnapshot(), this.getSettings());
        
        if (includeIntermediateScenes) {
           log.debug("postmod Snapshots size :" + this.getPreparationPhaseSnapshots().size());
            for (int i = 0; i < this.getPreparationPhaseSnapshots().size(); i++) {
                VisualSimulationDebugger.addScene(name + " (post: " + this.getPreperationPhaseModifiers().get(i).getName() + ")", this.getPreparationPhaseSnapshots().get(i), this.getSettings());
            }
            VisualSimulationDebugger.addScene(name + " (final)", this.getFinalSnapshot(), this.getSettings());
        }
    }


    public Scene getInitialSnapshot() {
        return initialSnapshot;
    }

    public Scene getFinalSnapshot() {
        return finalSnapshot;
    }

    public List<Scene> getSimulationPhaseSnapshots() {
        return simulationPhaseSnapshots;
    }

    public List<Scene> getPreparationPhaseSnapshots() {
        return preparationPhaseSnapshots;
    }

    public List<Scene> getEndPhaseSnapshots() {
        return endPhaseSnapshots;
    }
    
    public List<Scene> getAllSnapShots(){
        List<Scene> allSnapShots = new LinkedList<>();
        allSnapShots.add(getInitialSnapshot());
        allSnapShots.addAll(getPreparationPhaseSnapshots());
        allSnapShots.addAll(getSimulationPhaseSnapshots());
        allSnapShots.addAll(getEndPhaseSnapshots());
        allSnapShots.add(getFinalSnapshot());
        return allSnapShots;
    }


    /**
     * Indicates the Simulation has terminated.
     * 
     * @return wheter simulation finished running
     */
    public boolean isFinsihed() {
        return isFinished;
    }

    /**
     * If the Simulation did come to a stable state within the defined timelimits, it is seen as succesful
     */
    public boolean isSuccessful() {
        return isSuccessful;
    }

    /**
     * A descriptive name for the simulation helps to identify it later when having many of them.
     */
	public String getName() {
		return name;
	}

    
    /**
     * A descriptive name for the simulation helps to identify it later when having many of them.
     */
	public void setName(String name) {
		this.name = name;
	}

	public SimulationSettings getSettings() {
		return settings;
	}

    /**
     * Every Simulation is associated with exactly one initial Scene
     */
	public Scene getScene() {
		return scene;
	}

    /**
     * The world is the jbox2d-instantiation of the Simulations' Scene.
     */
	public World getWorld() {
		return world;
    }

    public List<IWorldModifier> getPreperationPhaseModifiers() {
        return preparationPhaseModifiers;
    }

    public void setPreparationPhaseModifiers(List<IWorldModifier> startModifiers) {
        this.preparationPhaseModifiers = startModifiers;
    }

	public List<IWorldModifier> getPreStepModifiers() {
		return preStepModifiers;
	}

	public void setPreStepModifiers(List<IWorldModifier> worldEachStepModifiers) {
		this.preStepModifiers = worldEachStepModifiers;
    }

    public List<IWorldModifier> getPostStepModifiers() {
        return postStepModifiers;
    }

    public void setPostStepModifiers(List<IWorldModifier> postStepModifiers) {
        this.postStepModifiers = postStepModifiers;
    }

    public List<IWorldModifier> getEndPhaseModifiers() {
        return endPhaseModifiers;
    }

    public void setEndPhaseModifiers(List<IWorldModifier> endModifiers) {
        this.endPhaseModifiers = endModifiers;
    }
        
}