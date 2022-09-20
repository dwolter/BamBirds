package de.uniba.sme.bambirds.feedback;

import de.uniba.sme.bambirds.common.objects.ab.ABObject;
import de.uniba.sme.bambirds.common.objects.ab.ABType;
import de.uniba.sme.bambirds.common.utils.MathUtil;

public class ABObjectComparable implements Comparable<ABObjectComparable> {

	private final ABType type;
	private final double centerX;
	private final double centerY;
	private final double angle;

	public ABObjectComparable(ABObject object) {
		this.type = object.getType();
		this.centerX = object.getCenterX();
		this.centerY = object.getCenterY();
		this.angle = object.getAngle();
	}

	@Override
	public int hashCode() {
		return super.hashCode();
	}

	@Override
	public boolean equals(Object o) {
		if (o instanceof ABObjectComparable) {
			ABObjectComparable obj = (ABObjectComparable) o;
			return this.type == obj.type
					&& MathUtil.isClose(this.centerX, obj.centerX, 1)
					&& MathUtil.isClose(this.centerY, obj.centerY, 1)
					&& MathUtil.isClose(this.angle, obj.angle, 0.1);
		}
		return false;
	}

	@Override
	public int compareTo(ABObjectComparable o) {
		if (equals(o)) {
			return 0;
		}
		if (this.type != o.type) {
			return this.type.id() - o.type.id();
		}
		if (this.centerX != o.centerX) {
			return (int) (this.centerX - o.centerX);
		}
		if (this.centerY != o.centerY) {
			return (int) (this.centerY - o.centerY);
		}
		if (this.angle != o.angle) {
			return (int) (this.angle - o.angle);
		}
		return 0;
	}
}
