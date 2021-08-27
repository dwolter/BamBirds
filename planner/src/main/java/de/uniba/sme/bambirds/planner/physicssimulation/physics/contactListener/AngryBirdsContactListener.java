/*******************************************************************************
 * Copyright (c) 2013, Daniel Murphy
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 * 	* Redistributions of source code must retain the above copyright notice,
 * 	  this list of conditions and the following disclaimer.
 * 	* Redistributions in binary form must reproduce the above copyright notice,
 * 	  this list of conditions and the following disclaimer in the documentation
 * 	  and/or other materials provided with the distribution.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 ******************************************************************************/
package de.uniba.sme.bambirds.planner.physicssimulation.physics.contactListener;

import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;
import org.jbox2d.callbacks.ContactImpulse;
import org.jbox2d.callbacks.ContactListener;
import org.jbox2d.collision.Manifold;
import org.jbox2d.dynamics.Body;
import org.jbox2d.dynamics.contacts.Contact;

import de.uniba.sme.bambirds.common.objects.ab.ABType;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.entities.SceneEntityBase;



/**
 * Implement this class to get contact information. You can use these results for
 * things like sounds and game logic. You can also get contact results by
 * traversing the contact lists after the time step. However, you might miss
 * some contacts because continuous physics leads to sub-stepping.
 * Additionally you may receive multiple callbacks for the same contact in a
 * single time step.
 * You should strive to make your callbacks efficient because there may be
 * many callbacks per time step.
 * @warning You cannot create/destroy Box2D entities inside these callbacks.
 * @author Daniel Murphy
 *
 */
public class AngryBirdsContactListener implements ContactListener{
    
    
    
    private static final Logger log = LogManager.getLogger(AngryBirdsContactListener.class);
    
    /**
	 * Called when two fixtures begin to touch.
	 * @param contact
	 */
    @Override
    public void beginContact(Contact contact) {
        SceneEntityBase entity1 = ((SceneEntityBase) contact.m_fixtureA.getBody().getUserData());
        SceneEntityBase entity2 = ((SceneEntityBase) contact.m_fixtureB.getBody().getUserData());
        // log.debug("Contact begin: " + entity1.getGlobalID() + " >> " + entity2.getGlobalID() + " istouching: " + contact.isTouching()  );
    }

    /**
	 * Called when two fixtures cease to touch.
	 * @param contact
	 */
    @Override
    public void endContact(Contact contact) {
        SceneEntityBase entity1 = (SceneEntityBase) contact.m_fixtureA.getBody().getUserData();
        SceneEntityBase entity2 = (SceneEntityBase) contact.m_fixtureB.getBody().getUserData();

        entity1.getInContactWith().remove(entity2.getGlobalID());
        entity2.getInContactWith().remove(entity1.getGlobalID());
        // log.debug("Contact END: " + entity1.getGlobalID() + " XX "+ entity2.getGlobalID());
    }


    /**
	 * This is called after a contact is updated. This allows you to inspect a
	 * contact before it goes to the solver. If you are careful, you can modify the
	 * contact manifold (e.g. disable contact).
	 * A copy of the old manifold is provided so that you can detect changes.
	 * Note: this is called only for awake bodies.
	 * Note: this is called even when the number of contact points is zero.
	 * Note: this is not called for sensors.
	 * Note: if you set the number of contact points to zero, you will not
	 * get an EndContact callback. However, you may get a BeginContact callback
	 * the next step.
	 * Note: the oldManifold parameter is pooled, so it will be the same object for every callback
	 * for each thread.
	 * @param contact
	 * @param oldManifold
	 */
    @Override
    public void preSolve(Contact arg0, Manifold arg1) {

    }


    /**
	 * This lets you inspect a contact after the solver is finished. This is useful
	 * for inspecting impulses.
	 * Note: the contact manifold does not include time of impact impulses, which can be
	 * arbitrarily large if the sub-step is small. Hence the impulse is provided explicitly
	 * in a separate data structure.
	 * Note: this is only called for contacts that are touching, solid, and awake.
	 * @param contact
	 * @param impulse this is usually a pooled variable, so it will be modified after
	 * this call
	 */
    @Override
    public void postSolve(Contact contact, ContactImpulse impulse) {
        /* Note:
            > NormalImpulse is the magnitude of the correcting impulse applied to push the two bodies apart when they collide.
            This is in the direction of the contact normal.
            > TangentImpulse is the magnitude of the impulse applied to simulate friction between the two colliding fixtures,
            and this is perpendicular to the contact normal.
            > You can put these together to get the overall impulse applied.
        */

        if( impulse.normalImpulses.length > 0){
            float totalImpulseStrength = 0f;
            for (int i = 0; i < impulse.normalImpulses.length; i++) {
                totalImpulseStrength += Math.abs(impulse.normalImpulses[i]);
            }
            
            // if(totalImpulseStrength > 1.8f){
            if(totalImpulseStrength > 0f){
                boolean doPrint = false;
                Body body1 = contact.m_fixtureA.getBody();
                Body body2 = contact.m_fixtureB.getBody();
                SceneEntityBase entity1 = null;
                SceneEntityBase entity2 = null;
                if(body1.getUserData() != null &&  (SceneEntityBase) body2.getUserData() != null){
                    entity1 = (SceneEntityBase) body1.getUserData();
                    entity2 = (SceneEntityBase) body2.getUserData();
                    
                    if(entity1.getInContactWith().contains(entity2.getGlobalID())==false && entity2.getInContactWith().contains(entity1.getGlobalID())==false ){
                        entity1.getInContactWith().add(entity2.getGlobalID());
                        entity2.getInContactWith().add(entity1.getGlobalID());
                    
                        if(entity1.getAbType() == ABType.Ice || entity1.getAbType() == ABType.Stone || entity1.getAbType() == ABType.Wood || entity1.getAbType() == ABType.Pig){
                            entity1.addDamage(totalImpulseStrength);
                            doPrint = true;
                        }
                        
                        if(entity2.getAbType() == ABType.Ice || entity2.getAbType() == ABType.Stone || entity2.getAbType() == ABType.Wood || entity2.getAbType() == ABType.Pig){
                            entity2.addDamage(totalImpulseStrength);
                            doPrint = true;
                       }

                       
                       if(doPrint && entity1 != null && entity2 != null){
                           //uncomment if you want to log impulses
                        //    log.debug("Impulse: "  + entity1.getGlobalID() + "("+(entity1.getTotalDamage())+")" + " [ "+entity1.getProperties().getMaximumDamagePerMass() * body1.getMass()+" ] " + " >> " +entity2.getGlobalID() + "("+(entity1.getTotalDamage())+")" +  " [ "+entity2.getProperties().getMaximumDamagePerMass() * body2.getMass()+" ] "+ " : " + totalImpulseStrength + " #impulses= " + impulse.normalImpulses.length);
                       }
                      
                    }
                }
            }
        }
    }
}
