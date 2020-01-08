package planner;

import main.SWIConnector;
import meta.Strategy;
import meta.StrategyConsumer;

import java.util.List;

public class PrologPlanner implements Strategy {

	private SWIConnector connector;
	private String knowledgeBaseFilename;
	private PrologPlanParser parser;
	private boolean finished = false;

	public PrologPlanner(SWIConnector connector, String knowledgeBaseFilename, PrologPlanParser parser) {
		this.connector = connector;
		this.knowledgeBaseFilename = knowledgeBaseFilename;
		this.parser = parser;
	}

	@Override
	public void plan(StrategyConsumer consumer, long timeOut) {
		// TODO: Try to not receive all plans at once; get them as soon as they can be returned and post them to consumer
		connector.sendCommand("'" + knowledgeBaseFilename + "'.");
		String result = connector.getResult(timeOut);
		List<Target> targets = parser.parsePlans(result);
		consumer.post(targets);
		finished = true;
	}

    public List<Target> planSynchronously(long timeOut) {
        connector.sendCommand("'" + knowledgeBaseFilename + "'.");
        String result = connector.getResult(timeOut);
        return parser.parsePlans(result);
    }

	@Override
	public void kill() {
		if (!finished) {
			connector.getResult(0);
		}
	}
}
