package de.uniba.sme.bambirds.planner.physicssimulation.physics.modification.stability;

import de.uniba.sme.bambirds.planner.physicssimulation.physics.Simulation;

import org.jbox2d.dynamics.Body;
import org.jbox2d.dynamics.BodyType;
import org.jbox2d.dynamics.World;

import de.uniba.sme.bambirds.planner.physicssimulation.JWorld;
import de.uniba.sme.bambirds.planner.physicssimulation.exceptions.ModificationException;
import de.uniba.sme.bambirds.planner.physicssimulation.physics.SimulationSettings;
import de.uniba.sme.bambirds.planner.physicssimulation.physics.modification.IWorldModifier;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.entities.SceneEntityBase;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.entities.SceneRectangle;

import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;


public class AdvancedStabilizerV1 implements IWorldModifier {
    private static final Logger log = LogManager.getLogger(AdvancedStabilizerV1.class);



    public AdvancedStabilizerV1() {

    }


    @Override
    public void apply(JWorld world, Simulation simulation, SimulationSettings settings) throws ModificationException {

        Body body = world.getBodyList();
        while (body != null) {
            body.setFixedRotation(true);
            if(body.getUserData() instanceof SceneRectangle){
                SceneRectangle sceneRectangle = (SceneRectangle)body.getUserData();
                if(sceneRectangle != null){
                    log.debug("Body: " + sceneRectangle.getGlobalID() + " width: " + sceneRectangle.getWidth() + " | height: " + sceneRectangle.getHeight());
                }
            }
            body = body.getNext();
        }
        world.step(0,0,0);
        
      
        for (int i = 0; i < 1000; i++) {
            world.step(settings.getDeltaTime(), settings.getVelocityIterations(), settings.getPositionIterations());
        }
        
        
        body = world.getBodyList();
        while (body != null) {
            body.setFixedRotation(false);
            body.setAngularDamping(100000f);
            body = body.getNext();
        }
        for (int i = 0; i < 1000; i++) {
            world.step(settings.getDeltaTime(), settings.getVelocityIterations(), settings.getPositionIterations());
        }
        

        body = world.getBodyList();
        while (body != null) {
            body.setAngularDamping(0.1f);
            SceneEntityBase sceneEntityBase = (SceneEntityBase)body.getUserData();
            
            // apply initial entityproperties to body again 
            simulation.getScene().getEntityPropertiesLUT().getEntityProperties(sceneEntityBase.getAbType()).applyValuesToBody(body);
            body = body.getNext();
        }
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
        return "AdvancedStabilizerV1";
    }
}