package de.uniba.sme.bambirds.planner.physicssimulation.physics;

import org.jbox2d.common.Vec2;
import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;


/**The SimulationSettings is a an object provided to a Simulation-Constructor. It stores important Settings which can greatly affect the Runtime of a Simulation. Also you define here how often a Snapshot will be created during the Simulations Runtime. */
public class SimulationSettings {
    private static final Logger log = LogManager.getLogger(SimulationSettings.class);

    // Typically we use a time step of 1/60 of a second
    // # (60Hz) and 6 velocity/2 position iterations. This provides a high quality
    // # simulation in most game scenarios. Dive into the docs or into the web to read more about this.

    
    /** after empiric evaluation, we found out that the angry-birds-game uses -10 as a gravitation-force */
    private Vec2 gravity = new Vec2(0, -10f);

    //** How many physics-steps per simulated virtual-second/game-second. Default is 60*/
    private float deltaTime = 1.0f / 60.0f;


    private int positionIterations = 10;
    // private int positionIterations = 3;
    
    private int velocityIterations = 10;
    // private int velocityIterations = 8;

    //* Do you wish to create snapshots of intermediate States while the Simulation is running ?*/
    private boolean doIntermediateSnapshots = false;

    /**After how much Simulationsteps do you wish to create Snapshots*/
    private int intermediateSnapshotDelta = 20;

    /**How much virtual milliseconds to simulate as upperlimit. If the Simulation is not stable until then, it will be aborted (stops simulating), so we do not waste valuable processor-time. If the simulation is stable in shorter time, it will stop earlier ( without wasting processing time ).*/
    // private long maxRuntimeVirtualMillis = 20000;
    private long maxRuntimeVirtualMillis = 40000;


    public SimulationSettings() {

    }

    public  SimulationSettings getCopy(){
        SimulationSettings copy = new SimulationSettings();
        copy.gravity = new Vec2(gravity.x,gravity.y);
        copy.deltaTime = deltaTime;
        copy.positionIterations = positionIterations;
        copy.velocityIterations = velocityIterations;
        copy.doIntermediateSnapshots = doIntermediateSnapshots;
        copy.intermediateSnapshotDelta = intermediateSnapshotDelta;
        copy.maxRuntimeVirtualMillis = maxRuntimeVirtualMillis;
        return copy;
    }

    public Vec2 getGravity() {
        return gravity;
    }

    public void setGravity(Vec2 gravity) {
        this.gravity = gravity;
    }

    public float getDeltaTime() {
        return deltaTime;
    }

    public void setDeltaTime(float deltaTime) {
        this.deltaTime = deltaTime;
    }

    public int getVelocityIterations() {
        return velocityIterations;
    }

    public void setVelocityIterations(int velocityIterations) {
        this.velocityIterations = velocityIterations;
    }

    public int getPositionIterations() {
        return positionIterations;
    }

    public void setPositionIterations(int positionIterations) {
        this.positionIterations = positionIterations;
    }

    public boolean isDoIntermediateSnapshots() {
        return doIntermediateSnapshots;
    }

    public void setDoIntermediateSnapshots(boolean doIntermediateSnapshots) {
        this.doIntermediateSnapshots = doIntermediateSnapshots;
    }

    public int getIntermediateSnapshotDelta() {
        return intermediateSnapshotDelta;
    }

    public void setIntermediateSnapshotDelta(int intermediateSnapshotDelta) {
        this.intermediateSnapshotDelta = intermediateSnapshotDelta;
    }

    public long getMaxRuntimeVirtualMillis() {
        return maxRuntimeVirtualMillis;
    }

    public void setMaxRuntimeMillis(long maxRuntimeMillis) {
        this.maxRuntimeVirtualMillis = maxRuntimeMillis;
    }
    
}