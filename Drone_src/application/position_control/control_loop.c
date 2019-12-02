/*
 * control_loop.c
 *
 *  Created on: Nov 17, 2019
 *      Author: jakov
 */

#include "madgwickFilter.h"
#include "control_loop.h"

#define PROPELLER_ROTATION_DIRECTION (-1)

_STATIC_ _INLINE_ int16_t M0_Diff(Vector_t err, _FLOAT_ factor){
	return (int16_t)((-err.v[0] + err.v[1] - PROPELLER_ROTATION_DIRECTION*err.v[2])*factor);
}

_STATIC_ _INLINE_ int16_t M1_Diff(Vector_t err, _FLOAT_ factor){
	return (int16_t)((err.v[0] + err.v[1] + PROPELLER_ROTATION_DIRECTION*err.v[2])*factor);
}

_STATIC_ _INLINE_ int16_t M2_Diff(Vector_t err, _FLOAT_ factor){
	return (int16_t)((err.v[0] - err.v[1] - PROPELLER_ROTATION_DIRECTION*err.v[2])*factor);
}

_STATIC_ _INLINE_ int16_t M3_Diff(Vector_t err, _FLOAT_ factor){
	return (int16_t)((-err.v[0] - err.v[1] + PROPELLER_ROTATION_DIRECTION*err.v[2])*factor);
}

__attribute__((optimize("O2")))
_STATIC_ _INLINE_ uint16_t ClipMotorSpeedValue(uint16_t motorspeed, int16_t diff, uint16_t throttle){
	uint16_t oneAndHalfThrottle = (throttle + (throttle >> 1));
	if((motorspeed + diff) < 0){
		return 0;
	}else if((motorspeed + diff) > oneAndHalfThrottle){
		return oneAndHalfThrottle;
	}else{
		return (motorspeed + diff);
	}
}

bool ControlLoop_run(Quaternion_t *errorQuaternion,
		     ControlParams_t *control,
			 Vector_t *omega,
			 uint16_t throttle, /* a value from 0 - 1000 */
		     Motorcontrolvalues_t *motors){

	int16_t diff;
	Vector_t err;
	_FLOAT_ theta_half;
	_FLOAT_ factor;
	_FLOAT_ q0;


	if(throttle > 37){ /* Of 1000 */
		q0 = errorQuaternion->q[0];
		theta_half = acos(q0);
		/*
		 * the factor is for scaling linerarly with the error angle:
		 * theta_half = acos(q0)
		 * acos(q0)/sin(theta_half) = acos(q0)/(sqrt(1 - q0**2))
		 */
		factor = theta_half * invSqrt(1 - q0*q0);

		Vect_write_three_values(&err,
					errorQuaternion->q[1]*factor,
					errorQuaternion->q[2]*factor,
					errorQuaternion->q[3]*factor);

		Motors_set_all_data_speed(motors, throttle);

		diff = M0_Diff(err, control->P) +
				M0_Diff(*omega, control->D);
		motors->motorspeeds[0] = ClipMotorSpeedValue(motors->motorspeeds[0], diff, throttle);

		diff = M1_Diff(err, control->P) +
				M1_Diff(*omega, control->D);
		motors->motorspeeds[1] = ClipMotorSpeedValue(motors->motorspeeds[1], diff, throttle);

		diff = M2_Diff(err, control->P) +
				M2_Diff(*omega, control->D);
		motors->motorspeeds[2] = ClipMotorSpeedValue(motors->motorspeeds[2], diff, throttle);

		diff = M3_Diff(err, control->P) +
				M3_Diff(*omega, control->D);
		motors->motorspeeds[3] = ClipMotorSpeedValue(motors->motorspeeds[3], diff, throttle);
		return true;
	}else{
		Motors_set_all_data_speed(motors, 0);
		return false;
	}
}
