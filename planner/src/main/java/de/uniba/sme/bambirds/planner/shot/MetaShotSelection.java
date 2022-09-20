package de.uniba.sme.bambirds.planner.shot;

import de.uniba.sme.bambirds.common.database.Node;
import de.uniba.sme.bambirds.common.objects.Plan;
import de.uniba.sme.bambirds.common.objects.Shot;


import java.util.List;

/**
 *
 * @author uni
 */
public interface MetaShotSelection {

    Plan getChosenTarget();

    boolean generateAndEvaluateShotOptions(List<Plan> plans);

    boolean generateAndEvaluateShotOptions();

    Node mostPromisingNode();

    List<Node> getAvailableTargets();

    void printAvailableTargets();

    boolean isFallbackToDemoShot();
}
