package de.uniba.sme.bambirds.planner.physicssimulation.debugging;
import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;

import org.jbox2d.collision.shapes.PolygonShape;
import org.jbox2d.common.Vec2;
import org.jbox2d.dynamics.Body;
import org.jbox2d.dynamics.BodyDef;
import org.jbox2d.dynamics.BodyType;
import org.jbox2d.dynamics.World;





/** This is a simple example of building and running a simulation using Box2D without using the VisualDebugger. Whe just simulate a world and let it run for a while, while we track some objects' states in the console. 
we create a large ground box and a small dynamic box.*/
public class JBox2DDebugNoGUIWorldTest {

    private static final Logger log = LogManager.getLogger(JBox2DDebugNoGUIWorldTest.class);
    
    public static void main(String[] args) {
    // 
        // # default gravity is (0,-10) and doSleep is True
        World world = new World(new Vec2(0,-10));

         // # create ground>>>>>>>>>>>>>>>>>>>>>>>>>>>
         {
            PolygonShape shape = new PolygonShape();
            shape.setAsBox(100.0f,1f);
            BodyDef bodyDef = new BodyDef();
            bodyDef.type = BodyType.STATIC;
            bodyDef.position.set(0, -1);
            bodyDef.allowSleep = true;
            
            Body body  = world.createBody(bodyDef);
            body.createFixture(shape, 0f);
            
        }
        // # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>



        
        PolygonShape polygonShape = new PolygonShape();
        polygonShape.setAsBox(0.5f, 0.5f);
        
        BodyDef bodyDef = new BodyDef();
        bodyDef.type = BodyType.DYNAMIC;

        bodyDef.position.set(0, 2);
        bodyDef.angle = (float) (0.01);
        bodyDef.allowSleep = true;
        Body body = world.createBody(bodyDef);

        // # And add  fixture onto it (with a nonzero density, so it will move)
        body.createFixture(polygonShape, 5.0f);
        // body.applyForce(new Vec2(-10000 * , 0), new Vec2());

        
        // # Prepare for simulation. Typically we use a time step of 1/60 of a second
        // # (60Hz) and 6 velocity/2 position iterations. This provides a high quality
        // # simulation in most game scenarios.
        float timeStep = 1.0f / 60f;
        int velocity_iterations_per_step = 6; 
        int position_iterations_per_step = 2;

        // # This is our little game loop.
        for(int step = 0; step < 180; step++){
            
            
            // # Instruct the world to perform a single step of simulation. It is
            // # generally best to keep the time step and iterations fixed.
            world.step(timeStep, velocity_iterations_per_step, position_iterations_per_step);
            
            // # Clear applied body forces. We didn't apply any forces, but you should
            // # know about this function.
            world.clearForces();
            
            // # Now print the position and angle of the body.
            log.debug("Step: " + step + " | Body-Position: " + body.getPosition() +  " | Body-Angle: " + body.getAngle() );
        }
        }
    }


