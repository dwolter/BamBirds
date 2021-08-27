package de.uniba.sme.bambirds.planner.physicssimulation.physics;

import java.util.List;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadPoolExecutor;
import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;



/**The SimulationExecutor makes sure will make sure that multiple Simulations are executed in multiple Threads to improve the performance.*/
public class SimulationExecutor {
    private static final Logger log = LogManager.getLogger(SimulationExecutor.class);



    private static SimulationExecutor simulationExecutor;

    private ThreadPoolExecutor executor;

    private SimulationExecutor() {
        executor = (ThreadPoolExecutor) Executors.newCachedThreadPool();
    }

    private static synchronized void initializeInstance(){
        SimulationExecutor.simulationExecutor = new SimulationExecutor();
    }

    public static SimulationExecutor getInstance() {
        if (simulationExecutor == null) {
            initializeInstance();
        }
        return simulationExecutor;
    }

    public void runSimulations(List<Simulation> simulations) {
        for (Simulation simulation : simulations) {
            runSimulation(simulation);
        }
    }

    public void runSimulation(Simulation simulation) {
        executor.execute(simulation);
    }

    public void cancel() {
        executor.shutdownNow();
    }
}