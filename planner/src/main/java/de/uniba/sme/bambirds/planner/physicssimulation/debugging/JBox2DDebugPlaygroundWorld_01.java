package de.uniba.sme.bambirds.planner.physicssimulation.debugging;

import org.jbox2d.testbed.framework.TestbedTest;
import org.jbox2d.collision.shapes.PolygonShape;
import org.jbox2d.common.Vec2;
import org.jbox2d.dynamics.Body;
import org.jbox2d.dynamics.BodyDef;
import org.jbox2d.dynamics.BodyType;
import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;



/** An Example of another implementation of a TestbedTest-subclass. We dont use it for the Angry-Birds-Agent, but looking at it and playing around with it will let you grasp faster what does what */
public class JBox2DDebugPlaygroundWorld_01 extends TestbedTest{
    private static final Logger log = LogManager.getLogger(JBox2DDebugPlaygroundWorld_01.class);

    public JBox2DDebugPlaygroundWorld_01() {

    }

    @Override
    public void initTest(boolean deserialized) {

        
        // set camera 
        this.setCamera(new Vec2(0,10), 10f);
        
        // # create ground>>>>>>>>>>>>>>>>>>>>>>>>>>>
        {
            PolygonShape shape = new PolygonShape();
            shape.setAsBox(30.0f, 0.1f);
            BodyDef bodyDef = new BodyDef();
            bodyDef.type = BodyType.STATIC;
            bodyDef.position.set(0, 0);
            bodyDef.allowSleep = true;
            
            Body body = getWorld().createBody(bodyDef);
            body.createFixture(shape, 0f);

            
        }
        // # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
        
        
        for (int i = 0; i < 3; i++) {
            PolygonShape shape = new PolygonShape();
            Vec2[] verts = new Vec2[4];
            verts[0] = new Vec2(-5,10);
            verts[1] = new Vec2(0, 15);
            verts[2] = new Vec2(5,10);

            shape.set(verts, 3);
          
            BodyDef bodyDef = new BodyDef();
            bodyDef.type = BodyType.STATIC;

            bodyDef.position.set(0, 2 +i* 2);
            bodyDef.allowSleep = true;
            Body body = getWorld().createBody(bodyDef);
            body.createFixture(shape, 5.0f);
            log.debug("Body :" + i + " | Pos= " + body.getPosition());
          }
    }

    @Override
    public String getTestName() {
        return "SUPRA ARMAGEDDON TEST";
    }


}