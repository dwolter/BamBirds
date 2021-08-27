package de.uniba.sme.bambirds.planner.physicssimulation.physics.modification.manipulation;

import de.uniba.sme.bambirds.planner.physicssimulation.physics.Simulation;
import org.jbox2d.dynamics.Body;
import org.jbox2d.dynamics.BodyType;
import org.jbox2d.dynamics.World;

import de.uniba.sme.bambirds.planner.physicssimulation.JWorld;
import de.uniba.sme.bambirds.planner.physicssimulation.exceptions.ModificationException;
import de.uniba.sme.bambirds.planner.physicssimulation.physics.SimulationSettings;
import de.uniba.sme.bambirds.planner.physicssimulation.physics.modification.IWorldModifier;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.entities.SceneEntityBase;
import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;


/**Delete Objects from a Physicssimulation by providing their globalID */
public class BlockRemover implements IWorldModifier {
    private static final Logger log = LogManager.getLogger(BlockRemover.class);

    
    private String blockID;

    /**
    * Delete Objects from a Physicssimulation by providing their globalID 
     * @param blockID the globalID of the object that shall be deleted from the simulation
     */
    public BlockRemover(String blockID) {
        this.blockID = blockID;
    }

    @Override
    public void apply(JWorld world, Simulation simulation, SimulationSettings settings) throws ModificationException {
        Body body = world.getBodyList();
        while (body != null) {
            if (body.m_userData instanceof SceneEntityBase) {
                SceneEntityBase entity = (SceneEntityBase) body.m_userData;
                if (entity.getGlobalID().equals(blockID)) {
                    world.destroyBody(body);
                    log.debug("BlockRemover: Removed Block: " + blockID);
                    return;
                }
            }
            body = body.getNext();
        }
        throw new ModificationException("Block not found (" + blockID + ")");
    }

    @Override
    public String getName() {
        return "Block Remover (" + blockID +")";
    }
    
}