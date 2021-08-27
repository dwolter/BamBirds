package de.uniba.sme.bambirds.common.objects;

import de.uniba.sme.bambirds.common.Strategy;

/**
 * Enum of all Thinkers (s. {@link Strategy}) to differentiate
 * {@link Plan}s by their source so they can be handled accordingly.
 */
public enum ThinkerType {
	OLDPLANNER, NEWPLANNER, DEMO
}
