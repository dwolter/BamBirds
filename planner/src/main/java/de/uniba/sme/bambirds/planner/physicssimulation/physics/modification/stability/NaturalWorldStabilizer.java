package de.uniba.sme.bambirds.planner.physicssimulation.physics.modification.stability;

import de.uniba.sme.bambirds.planner.physicssimulation.physics.Simulation;
import org.jbox2d.dynamics.Body;
import org.jbox2d.dynamics.BodyType;
import org.jbox2d.dynamics.World;

import de.uniba.sme.bambirds.planner.physicssimulation.JWorld;
import de.uniba.sme.bambirds.planner.physicssimulation.exceptions.ModificationException;
import de.uniba.sme.bambirds.planner.physicssimulation.physics.SimulationSettings;
import de.uniba.sme.bambirds.planner.physicssimulation.physics.modification.IWorldModifier;
import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;


/**
 * This Stabilizer  simply simulates the physics for a while and tries to 
 * wait until all objects in the world stopped moving. 
 * Depending on the Level this may lead to good results but also to bad results.
 * */
public class NaturalWorldStabilizer implements IWorldModifier {

    private static final Logger log = LogManager.getLogger(NaturalWorldStabilizer.class);

    private long maxRuntimeMillis;
    private float maxRuntimeVirtualMillis;

    public NaturalWorldStabilizer() {
        maxRuntimeMillis = Long.MAX_VALUE;
        maxRuntimeVirtualMillis = Long.MAX_VALUE;
    }

    public NaturalWorldStabilizer(long maxRuntimeMillis, float maxRuntimeVirtualMillis) {
        this.maxRuntimeMillis = maxRuntimeMillis;
        this.maxRuntimeVirtualMillis = maxRuntimeVirtualMillis;
    }

    
    @Override
    public void apply(JWorld world, Simulation simulation, SimulationSettings settings) throws ModificationException {
        long startTime = System.currentTimeMillis();
        float virtualTimePassed = 0;
        int stepCount = 0;
        world.step(settings.getDeltaTime(), settings.getVelocityIterations(), settings.getVelocityIterations());
        stepCount++;
        virtualTimePassed += settings.getDeltaTime();
        while(!isStable(world)) {
            world.step(settings.getDeltaTime(), settings.getVelocityIterations(), settings.getVelocityIterations());
            virtualTimePassed += settings.getDeltaTime() * 1000;
            stepCount++;
            long currentTime = System.currentTimeMillis();
            if (currentTime - startTime > maxRuntimeMillis || virtualTimePassed > maxRuntimeVirtualMillis) {
                log.debug("ABORTED Stabilization after Steps: " + stepCount + " | Virtual Time passed = " + virtualTimePassed + " deltatime = " + settings.getDeltaTime());
                throw new ModificationException("Time for creating natural stability exceeded.");
            }
        }
        log.debug("SUCCESS Stabilization after Steps: " + stepCount);
    }


    private boolean isStable(World world) {
        Body body = world.getBodyList();
        while (body != null) {
            if ( body.m_type == BodyType.DYNAMIC && body.isAwake() ) {
                return false;
            }
            body = body.getNext();
        }
        return true;
    }

    @Override
    public String getName() {
        return "Natural Stabilizer";
    }
    
}