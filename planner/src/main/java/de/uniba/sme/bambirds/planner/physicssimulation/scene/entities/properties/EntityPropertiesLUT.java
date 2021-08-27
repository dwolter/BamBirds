package de.uniba.sme.bambirds.planner.physicssimulation.scene.entities.properties;

import java.io.Serializable;
import java.util.Hashtable;

import de.uniba.sme.bambirds.common.objects.ab.ABType;
import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;

/**Convenience-Class which provides a comfortable method to retrieve a matching Entity-Properties-Object for a given ABType */
public class EntityPropertiesLUT implements Serializable {

    private static final long serialVersionUID = 1L;

    private static final Logger log = LogManager.getLogger(EntityPropertiesLUT.class);

    Hashtable<ABType, EntityProperties> lookUpTable;


    public  EntityPropertiesLUT(){
        // Define defaultProperties
        lookUpTable = new Hashtable<>();
            lookUpTable.put(ABType.Background, new BackgroundProperties());
            lookUpTable.put(ABType.BlackBird, new BlackBirdProperties());
            lookUpTable.put(ABType.BlueBird, new BlueBirdProperties());
            lookUpTable.put(ABType.Duck, new DuckProperites());
            lookUpTable.put(ABType.Edge, new EdgeProperties());
            lookUpTable.put(ABType.Ground, new GroundProperties());
            lookUpTable.put(ABType.Hill, new IceProperties());
            lookUpTable.put(ABType.Ice, new IceProperties());
            lookUpTable.put(ABType.Pig, new PigProperties());
            lookUpTable.put(ABType.RedBird, new RedBirdProerties());
            lookUpTable.put(ABType.Sling, new SlingProperties());
            lookUpTable.put(ABType.Stone, new StoneProperties());
            lookUpTable.put(ABType.TNT, new TNTProperties());
            lookUpTable.put(ABType.Unknown, new UnknownProperties());
            lookUpTable.put(ABType.Watermelon, new WatermelonProperties());
            lookUpTable.put(ABType.WhiteBird, new WhiteBirdProperties());
            lookUpTable.put(ABType.Wood, new WoodProperties());
            lookUpTable.put(ABType.YellowBird, new YellowBirdProperties());

    }


    /**
     * Retrieve a matching Entity-Property-Object for a requested ABType
     * @param requestedPropertyType
     * @return The matching EntityProperty if there was found one , if not found it will return null 
     */
    public EntityProperties getEntityProperties(ABType requestedPropertyType){
        EntityProperties requestedProperties = null;
        if(lookUpTable.containsKey(requestedPropertyType) == false){
            log.debug("Warning - Could not find requested Properties of type: " + requestedPropertyType.toString());
        }else{
            requestedProperties = lookUpTable.get(requestedPropertyType);
        }
        return requestedProperties;
    }

    public EntityPropertiesLUT getCopy(){
        EntityPropertiesLUT copy = new EntityPropertiesLUT();

        lookUpTable.get(ABType.Background).copyValuesTo(copy.lookUpTable.get(ABType.Background));
        lookUpTable.get(ABType.BlackBird).copyValuesTo(copy.lookUpTable.get(ABType.BlackBird));
        lookUpTable.get(ABType.BlueBird).copyValuesTo(copy.lookUpTable.get(ABType.BlueBird));
        lookUpTable.get(ABType.Duck).copyValuesTo(copy.lookUpTable.get(ABType.Duck));
        lookUpTable.get(ABType.Edge).copyValuesTo(copy.lookUpTable.get(ABType.Edge));
        lookUpTable.get(ABType.Ground).copyValuesTo(copy.lookUpTable.get(ABType.Ground));
        lookUpTable.get(ABType.Hill).copyValuesTo(copy.lookUpTable.get(ABType.Hill));
        lookUpTable.get(ABType.Ice).copyValuesTo(copy.lookUpTable.get(ABType.Ice));
        lookUpTable.get(ABType.Pig).copyValuesTo(copy.lookUpTable.get(ABType.Pig));
        lookUpTable.get(ABType.RedBird).copyValuesTo(copy.lookUpTable.get(ABType.RedBird));
        lookUpTable.get(ABType.Sling).copyValuesTo(copy.lookUpTable.get(ABType.Sling));
        lookUpTable.get(ABType.Stone).copyValuesTo(copy.lookUpTable.get(ABType.Stone));
        lookUpTable.get(ABType.TNT).copyValuesTo(copy.lookUpTable.get(ABType.TNT));
        lookUpTable.get(ABType.Unknown).copyValuesTo(copy.lookUpTable.get(ABType.Unknown));
        lookUpTable.get(ABType.Watermelon).copyValuesTo(copy.lookUpTable.get(ABType.Watermelon));
        lookUpTable.get(ABType.WhiteBird).copyValuesTo(copy.lookUpTable.get(ABType.WhiteBird));
        lookUpTable.get(ABType.Wood).copyValuesTo(copy.lookUpTable.get(ABType.Wood));
        lookUpTable.get(ABType.YellowBird).copyValuesTo(copy.lookUpTable.get(ABType.YellowBird));
        return copy;
    }
}