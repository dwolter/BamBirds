package de.uniba.sme.bambirds.planner.physicssimulation.sceneunderstanding.propertymodification.modifier;

import de.uniba.sme.bambirds.common.objects.ab.ABType;
import de.uniba.sme.bambirds.planner.physicssimulation.physics.Simulation;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.entities.SceneEntityBase;
import de.uniba.sme.bambirds.planner.physicssimulation.sceneunderstanding.propertymodification.PropertyModifier;
import org.jbox2d.common.Vec2;
import org.jbox2d.dynamics.Body;

import java.util.function.BiConsumer;

public class FrictionModifier implements PropertyModifier {

	final ABType objectABType;

	public  FrictionModifier(ABType objectABType){
		this.objectABType = objectABType;
	}

	@Override public BiConsumer<Simulation, Float> getFunction() {
		return (sim, value) -> {
			sim.getScene().getEntityPropertiesLUT().getEntityProperties(objectABType).setFixtureFriction(value);

			for(SceneEntityBase sceneEntityBase : sim.getScene().getAllEntities()) {
				if(sceneEntityBase.getAbType() == objectABType) {
					sceneEntityBase.getProperties().setFixtureFriction(value);
				}
			}
			if(sim.getWorld() != null){
				//if for some reason the world has already been instantiated, we apply the changes directly to the world objects
				Body body = sim.getWorld().getBodyList();
				while(body != null){
					SceneEntityBase sceneEntityBase = ((SceneEntityBase) body.getUserData());
					sceneEntityBase.getProperties().setFixtureFriction(value);
					sceneEntityBase.getProperties().applyValuesToBody(body);
					body = body.getNext();
				}
			}
		};
	}

	@Override public float getCurrentPropertyValue(Simulation simulation) {
		return  simulation.getScene().getEntityPropertiesLUT().getEntityProperties(objectABType).getFixtureFriction();
	}

	@Override public MODIFIER_TYPE getModifierType() {
		return MODIFIER_TYPE.FRICTION;
	}

	@Override public String toString() {
		return "Friction Modifier - Type: "+ objectABType.toString();
	}
}
