package de.uniba.sme.bambirds.common.utils;

import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;

/**
 * A simple future that just returns a value of the type <V>.
 *
 * @param <V> Type of value to return
 */
public class SimpleFuture<V> implements Future<V> {

	private final V v;

	/**
	 * Create a new SimpleFuture that returns v on get calls.
	 *
	 * @param v the value to return on get calls
	 */
	public SimpleFuture(V v) {
		this.v = v;
	}

	@Override
	public boolean cancel(final boolean mayInterruptIfRunning) {
		return false;
	}

	@Override
	public V get() {
		return v;
	}

	@Override
	public V get(final long timeout, final TimeUnit unit) {
		return v;
	}

	@Override
	public boolean isCancelled() {
		return false;
	}

	@Override
	public boolean isDone() {
		return true;
	}

}
