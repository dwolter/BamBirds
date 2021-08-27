package de.uniba.sme.bambirds.planner.physicssimulation.sceneunderstanding.propertymodification.modifier;

import de.uniba.sme.bambirds.common.objects.ab.ABType;
import de.uniba.sme.bambirds.planner.physicssimulation.physics.Simulation;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.entities.SceneEntityBase;
import de.uniba.sme.bambirds.planner.physicssimulation.sceneunderstanding.propertymodification.PropertyModifier;
import org.jbox2d.dynamics.Body;

import java.util.function.BiConsumer;

public class AngularDampingModifier implements PropertyModifier {
	final ABType objectABType;

	public AngularDampingModifier(ABType objectABType){
		this.objectABType = objectABType;
	}

	@Override public BiConsumer<Simulation, Float> getFunction() {

		return (sim, value) -> {

			sim.getScene().getEntityPropertiesLUT().getEntityProperties(objectABType).setAngularDamping(value);

			for(SceneEntityBase sceneEntityBase : sim.getScene().getAllEntities()) {
				if(sceneEntityBase.getAbType() == objectABType) {
					sceneEntityBase.getProperties().setAngularDamping(value);
				}
			}
			if(sim.getWorld() != null){
				//if for some reason the world has already been instantiated, we apply the changes directly to the world objects
				Body body = sim.getWorld().getBodyList();
				while(body != null){
					SceneEntityBase sceneEntityBase = ((SceneEntityBase) body.getUserData());
					sceneEntityBase.getProperties().setAngularDamping(value);
					sceneEntityBase.getProperties().applyValuesToBody(body);
					body = body.getNext();
				}
			}
		};
	}

	@Override public float getCurrentPropertyValue(Simulation simulation) {
		return  simulation.getScene().getEntityPropertiesLUT().getEntityProperties(objectABType).getAngularDamping();
	}

	@Override public PropertyModifier.MODIFIER_TYPE getModifierType() {
		return PropertyModifier.MODIFIER_TYPE.LINEAR_DAMPING;
	}

	@Override public String toString() {
		return "AngularDamping Modifier - Type: "+ objectABType.toString();
	}
}
