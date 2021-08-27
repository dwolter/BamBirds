package de.uniba.sme.bambirds.planner.physicssimulation.physics.modification.stability;

import java.util.HashMap;

import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;
import org.jbox2d.common.Vec2;
import org.jbox2d.dynamics.Body;
import org.jbox2d.dynamics.BodyType;
import org.jbox2d.dynamics.World;
import org.jbox2d.common.MathUtils;

import de.uniba.sme.bambirds.common.objects.ab.ABType;
import de.uniba.sme.bambirds.planner.physicssimulation.JWorld;
import de.uniba.sme.bambirds.planner.physicssimulation.exceptions.ModificationException;
import de.uniba.sme.bambirds.planner.physicssimulation.physics.Simulation;
import de.uniba.sme.bambirds.planner.physicssimulation.physics.SimulationSettings;
import de.uniba.sme.bambirds.planner.physicssimulation.physics.modification.IWorldModifier;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.entities.SceneEntityBase;

/**Next attempt to make a better Stabilizer.
 * In the main phase it fixes the rotations of the objects in the world. Then it lets the physics engine execute, alternatively moving the objects only in y-direction, then only in x-direction, then repeating this. Finally it will simulate in both x-and y-direction together, and then it will turn off fixed-rotation again and simulate further.
 * ( There is still enough room for improvement for future endeavors )   */
public class AdvancedStabilizerV2 implements IWorldModifier {
    private static final Logger log = LogManager.getLogger(AdvancedStabilizerV2.class);


    public AdvancedStabilizerV2() {
    }


    @Override
    public void apply(JWorld world, Simulation simulation, SimulationSettings settings) throws ModificationException {

        int iterationsPerCycle = 200;

        HashMap<String, Vec2> lastPositions = new HashMap<>();
        HashMap<String, Float> lastAngles = new HashMap<>();


        Body body = world.getBodyList();
        while (body != null) {
            body.setFixedRotation(true);
            SceneEntityBase sceneEntityBase = (SceneEntityBase)body.getUserData();
            if(sceneEntityBase.getAbType() == ABType.Background || sceneEntityBase.getAbType() == ABType.Ground || sceneEntityBase.getAbType() == ABType.Hill){
                body = body.getNext();
                continue;
            }

            lastPositions.put(sceneEntityBase.getGlobalID(), body.getPosition());
            lastAngles.put(sceneEntityBase.getGlobalID(), body.getAngle());
            body = body.getNext();
        }
        // Uts necessary to make a step  step to apply the physics-changes
        world.step(0,0,0);

        for (int i = 0; i < iterationsPerCycle; i++) {
            body = world.getBodyList();
            while (body != null) {
                SceneEntityBase sceneEntityBase = (SceneEntityBase)body.getUserData();
                if(sceneEntityBase.getAbType() == ABType.Background || sceneEntityBase.getAbType() == ABType.Ground || sceneEntityBase.getAbType() == ABType.Hill){
                    body = body.getNext();
                    continue;
                }
                // let physics engine change only Y-Position of objects
                body.setTransform(new Vec2(lastPositions.get(sceneEntityBase.getGlobalID()).x, body.getPosition().y), body.getAngle());
                lastPositions.put(sceneEntityBase.getGlobalID(), body.getPosition());
                lastAngles.put(   sceneEntityBase.getGlobalID(), body.getAngle());

                body = body.getNext();
            }

            world.step(settings.getDeltaTime(), settings.getVelocityIterations(), settings.getPositionIterations());
        }

        for (int i = 0; i < iterationsPerCycle; i++) {
            body = world.getBodyList();
            while (body != null) {
                SceneEntityBase sceneEntityBase = (SceneEntityBase)body.getUserData();
                if(sceneEntityBase.getAbType() == ABType.Background || sceneEntityBase.getAbType() == ABType.Ground || sceneEntityBase.getAbType() == ABType.Hill){
                    body = body.getNext();
                    continue;
                }
                // let physics engine change only X-Position of objects
                body.setTransform(new Vec2(body.getPosition().x, lastPositions.get( sceneEntityBase.getGlobalID()).y), body.getAngle());
                lastPositions.put(sceneEntityBase.getGlobalID(), body.getPosition());
                lastAngles.put(   sceneEntityBase.getGlobalID(), body.getAngle());

                body = body.getNext();
            }

            world.step(settings.getDeltaTime(), settings.getVelocityIterations(), settings.getPositionIterations());
        }


        for (int i = 0; i < iterationsPerCycle; i++) {
            body = world.getBodyList();
            while (body != null) {
                SceneEntityBase sceneEntityBase = (SceneEntityBase)body.getUserData();
                if(sceneEntityBase.getAbType() == ABType.Background || sceneEntityBase.getAbType() == ABType.Ground || sceneEntityBase.getAbType() == ABType.Hill){
                    body = body.getNext();
                    continue;
                }
                // let physics engine change only Y-Position of objects
                body.setTransform(new Vec2(lastPositions.get(sceneEntityBase.getGlobalID()).x, body.getPosition().y), body.getAngle());
                lastPositions.put(sceneEntityBase.getGlobalID(), body.getPosition());
                lastAngles.put(   sceneEntityBase.getGlobalID(), body.getAngle());

                body = body.getNext();
            }
            world.step(settings.getDeltaTime(), settings.getVelocityIterations(), settings.getPositionIterations());
        }


        for (int i = 0; i < iterationsPerCycle; i++) {
            body = world.getBodyList();
            while (body != null) {
                SceneEntityBase sceneEntityBase = (SceneEntityBase)body.getUserData();
                if(sceneEntityBase.getAbType() == ABType.Background || sceneEntityBase.getAbType() == ABType.Ground || sceneEntityBase.getAbType() == ABType.Hill){
                    body = body.getNext();
                    continue;
                }
                // let physics engine change only X-Position of objects
                body.setTransform(new Vec2(body.getPosition().x, lastPositions.get( sceneEntityBase.getGlobalID()).y), body.getAngle());
                lastPositions.put(sceneEntityBase.getGlobalID(), body.getPosition());
                lastAngles.put(   sceneEntityBase.getGlobalID(), body.getAngle());

                body = body.getNext();
            }

            world.step(settings.getDeltaTime(), settings.getVelocityIterations(), settings.getPositionIterations());
        }


        for (int i = 0; i < 1000; i++) {
            world.step(settings.getDeltaTime(), settings.getVelocityIterations(), settings.getPositionIterations());
        }


        body = world.getBodyList();
        while (body != null) {
            body.setFixedRotation(false);
            body = body.getNext();
        }

        for (int i = 0; i < 1000; i++) {
            world.step(settings.getDeltaTime(), settings.getVelocityIterations(), settings.getPositionIterations());
        }

        body = world.getBodyList();
        while (body != null) {
            SceneEntityBase sceneEntityBase = (SceneEntityBase) body.getUserData();
            if(sceneEntityBase.getAbType() == ABType.Background || sceneEntityBase.getAbType() == ABType.Ground || sceneEntityBase.getAbType() == ABType.Hill || sceneEntityBase.getAbType().isBird() || body.getPosition().equals(new Vec2(0,0))){
                body = body.getNext();
                continue;
            }
            float distance = MathUtils.distance(body.getPosition(), new Vec2(sceneEntityBase.getCenterX(), sceneEntityBase.getCenterY()));
            if (distance > 2) {
                throw new ModificationException("Final Body was to far away from the initial position");
            }
            body = body.getNext();
        }


        log.debug("AdvancedStabilizerV2 FINISHED Succesfully");
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
        return "AdvancedStabilizerV2";
    }

}

