package de.uniba.sme.bambirds.planner.shot;

import de.uniba.sme.bambirds.common.database.Node;
import de.uniba.sme.bambirds.common.objects.ExecutedNode;
import de.uniba.sme.bambirds.common.objects.ShotEffect;
import de.uniba.sme.bambirds.common.objects.ab.ABObject;
import de.uniba.sme.bambirds.common.objects.ab.ABType;

import java.util.ArrayList;
import java.util.List;

/**
 * Used for Calculating the effects of a shot and generating a corresponding {@link ExecutedNode}
 */
public class EffectCalculator {

	private static boolean isTarget(ABObject object) {
		return object.getType() == ABType.Pig || object.getType() == ABType.TNT;
	}

	public static ExecutedNode analyzeEffects(SceneDifference difference, Node node) {
		List<ShotEffect> effects = new ArrayList<>();

		difference.getDestroyedObjects().stream().filter(EffectCalculator::isTarget).forEach(object -> effects.add(new ShotEffect(object, ShotEffect.EffectType.DESTROY)));
		difference.getMovedObjects().stream().filter(EffectCalculator::isTarget).forEach(object -> effects.add(new ShotEffect(object, ShotEffect.EffectType.MOVE)));

		// TODO: Find way to determine if some objects were freed

		return new ExecutedNode(node, effects);
	}
}
