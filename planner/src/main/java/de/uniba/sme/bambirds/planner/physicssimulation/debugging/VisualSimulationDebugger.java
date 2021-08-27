package de.uniba.sme.bambirds.planner.physicssimulation.debugging;

import javax.swing.JFrame;

import org.jbox2d.testbed.framework.TestbedFrame;
import org.jbox2d.testbed.framework.TestbedModel;
import org.jbox2d.testbed.framework.TestbedPanel;
import org.jbox2d.testbed.framework.TestbedController.UpdateBehavior;
import org.jbox2d.testbed.framework.j2d.TestPanelJ2D;

import de.uniba.sme.bambirds.planner.physicssimulation.physics.SimulationSettings;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.Scene;
import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;

/** We use this class to conveniently be able to initialize, set up and populate the Debuggung-GUI, by using static-method calls from where any place we want */
public class VisualSimulationDebugger {
    private static final Logger log = LogManager.getLogger(VisualSimulationDebugger.class);


    //---------------------------
    // Settings - you can adjust the settings here or at runtime through setter-methods
    static boolean drawCoordinateSystem = true;

    //---------------------------


    static TestbedModel model ;
    static{
        model = new TestbedModel();
        model.addCategory("Debug Tests");

        // we always start in a paused state
        model.getSettings().pause = true;
    }

    public static void addScene(String name, Scene scene, SimulationSettings settings){
        JBox2DDebugTestbedWorld testBedtest = new JBox2DDebugTestbedWorld(name, scene, settings);
        testBedtest.setDrawCoordinateSystem(drawCoordinateSystem);
        testBedtest.setTitle(name);
        model.addTest(testBedtest); 
    } 

    public static void visualize(){
        // model.addTest(new JBox2DDebugPlaygroundWorld_01());

        TestbedPanel panel = new TestPanelJ2D(model);
        JFrame testbed = new TestbedFrame(model, panel, UpdateBehavior.UPDATE_CALLED);
        testbed.setVisible(true);
        testbed.setBounds(0, 0, 1920, 1080);
        testbed.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
}

	public static boolean isDrawCoordinateSystem() {
		return drawCoordinateSystem;
	}

	public static void setDrawCoordinateSystem(boolean drawCoordinateSystem) {
		VisualSimulationDebugger.drawCoordinateSystem = drawCoordinateSystem;
	}

}