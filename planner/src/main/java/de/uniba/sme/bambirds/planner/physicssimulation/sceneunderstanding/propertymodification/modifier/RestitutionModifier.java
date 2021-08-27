package de.uniba.sme.bambirds.planner.physicssimulation.sceneunderstanding.propertymodification.modifier;

import de.uniba.sme.bambirds.common.objects.ab.ABType;
import de.uniba.sme.bambirds.planner.physicssimulation.physics.Simulation;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.entities.SceneEntityBase;
import de.uniba.sme.bambirds.planner.physicssimulation.sceneunderstanding.propertymodification.PropertyModifier;
import org.jbox2d.dynamics.Body;

import java.util.function.BiConsumer;

public class RestitutionModifier implements PropertyModifier{

	final ABType objectABType;

	public RestitutionModifier(ABType objectABType){
		this.objectABType = objectABType;
	}

	@Override public BiConsumer<Simulation, Float> getFunction() {
		return (sim, value) -> {

			sim.getScene().getEntityPropertiesLUT().getEntityProperties(objectABType).setFixtureRestitution(value);

			for(SceneEntityBase sceneEntityBase : sim.getScene().getAllEntities()) {
				if(sceneEntityBase.getAbType() == objectABType) {
					sceneEntityBase.getProperties().setFixtureRestitution(value);
				}
			}
			if(sim.getWorld() != null){
				//if for some reason the world has already been instantiated, we apply the changes directly to the world objects
				Body body = sim.getWorld().getBodyList();
				while(body != null){
					SceneEntityBase sceneEntityBase = ((SceneEntityBase) body.getUserData());
					sceneEntityBase.getProperties().setFixtureRestitution(value);
					sceneEntityBase.getProperties().applyValuesToBody(body);
					body = body.getNext();
				}
			}
		};
	}

	@Override public float getCurrentPropertyValue(Simulation simulation) {
		return  simulation.getScene().getEntityPropertiesLUT().getEntityProperties(objectABType).getFixtureRestitution();
	}

	@Override public PropertyModifier.MODIFIER_TYPE getModifierType() {
		return MODIFIER_TYPE.RESTITUTION;
	}

	@Override public String toString() {
		return "Fixture Restitution Modifier - Type: "+ objectABType.toString();
	}
}
