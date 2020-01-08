package helper;

import static ab.vision.ABType.*;

public class Constants {
	static public boolean    DEBUG_ENABLED = true;
	static public boolean    USE_NEW_SLING_DETECTION = true;
	static public boolean    PERFORMANCE_MEASUREMENT_ENABLED = false; // for testing
	static public boolean    LEVEL_SELECTION_FIRST_ROUND_ITERATIVE = false; // for testing
	static public double[]   lowAngleChange;
	static public double[]   lowAngleVelocity;
	static public double[]   highAngleBegin    = new double[10];
	static public double[][] highAngleChange   = new double[10][3];
	static public double[][] highAngleVelocity = new double[10][3];
	static public double[]  yellowBirdVelocity = new double[]{ -0.2339, 0.3093, 3.7051 };
	// yellowBirdVelocity: -0.4315 * Math.pow(x, 3) - 0.4029 * x * x + 0.3315 * x + 3.7126; // more accurate, necessary?

	static {
		if (USE_NEW_SLING_DETECTION) {
			lowAngleChange   = new double[]{ -0.0230, -7.871e-4, 0.0540 };
			lowAngleVelocity = new double[]{  0.0473, -0.1756,   2.8654 };
			highAngleBegin[BlueBird.id]   = Math.toRadians(77.226);
			highAngleBegin[RedBird.id]    = Math.toRadians(74.476);
			highAngleBegin[YellowBird.id] = Math.toRadians(75.032);
			highAngleBegin[BlackBird.id]  = Math.toRadians(72.777);
			highAngleBegin[WhiteBird.id]  = Math.toRadians(69.365);
			highAngleChange[BlueBird.id]   = new double[]{ -6.2164, 17.7277, -12.5889 };
			highAngleChange[RedBird.id]    = new double[]{ -6.8544, 19.1149, -13.2502 };
			highAngleChange[YellowBird.id] = new double[]{ -7.1737, 20.0922, -13.9949 };
			highAngleChange[BlackBird.id]  = new double[]{ -10.4124, 28.4201, -19.2827 };
			highAngleChange[WhiteBird.id]  = new double[]{ -11.8720, 31.2441, -20.4036 };
			highAngleVelocity[BlueBird.id]   = new double[]{ 39.1345, -119.2619, 92.3808 };
			highAngleVelocity[RedBird.id]    = new double[]{ 42.1667, -124.9211, 93.8631 };
			highAngleVelocity[YellowBird.id] = new double[]{ 43.7502, -130.3646, 98.4179 };
			highAngleVelocity[BlackBird.id]  = new double[]{ 65.4336, -186.8649, 134.5157 };
			highAngleVelocity[WhiteBird.id]  = new double[]{ 73.2264, -199.8274, 137.3380 };
		} else {
			lowAngleChange   = new double[]{ -0.0204, -0.0045, 0.0549 };
			lowAngleVelocity = new double[]{  0.0406, -0.1640, 2.8615 };
			highAngleBegin[BlueBird.id]   = Math.toRadians(77.2834);
			highAngleBegin[RedBird.id]    = Math.toRadians(74.5361);
			highAngleBegin[YellowBird.id] = Math.toRadians(75.0575);
			highAngleBegin[BlackBird.id]  = Math.toRadians(72.8029);
			highAngleBegin[WhiteBird.id]  = Math.toRadians(69.4597);
			highAngleChange[BlueBird.id]   = new double[]{ -6.7165, 19.1056, -13.5384 };
			highAngleChange[RedBird.id]    = new double[]{ -6.6978, 18.7235, -13.0074 };
			highAngleChange[YellowBird.id] = new double[]{ -6.9560, 19.5175, -13.6161 };
			highAngleChange[BlackBird.id]  = new double[]{ -11.4143, 31.0365, -20.9903 };
			highAngleChange[WhiteBird.id]  = new double[]{ -13.6359, 35.7343, -23.2604 };
			highAngleVelocity[BlueBird.id]   = new double[]{ 42.8185, -129.3400, 99.2806 };
			highAngleVelocity[RedBird.id]    = new double[]{ 36.6116, -110.3012, 84.2551 };
			highAngleVelocity[YellowBird.id] = new double[]{ 41.4429, -124.2663, 94.3941 };
			highAngleVelocity[BlackBird.id]  = new double[]{ 76.0678, -214.6207, 152.6222 };
			highAngleVelocity[WhiteBird.id]  = new double[]{ 78.4918, -213.2718, 145.9202 };
		}
	}
}
