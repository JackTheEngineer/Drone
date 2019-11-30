/*
 * control_loop.c
 *
 *  Created on: Nov 17, 2019
 *      Author: jakov
 */

#include "madgwickFilter.h"
#include "control_loop.h"

#define MOTOR_CONSTELLATION (-1) // Was (-1) in order to turn back to the position the quad is in

_STATIC_ _INLINE_ int16_t M0_Diff(Vector_t err, _FLOAT_ factor){
	return (int16_t)((-err.v[0] + err.v[1] - MOTOR_CONSTELLATION*err.v[2])*factor);
}

_STATIC_ _INLINE_ int16_t M1_Diff(Vector_t err, _FLOAT_ factor){
	return (int16_t)((err.v[0] + err.v[1] + MOTOR_CONSTELLATION*err.v[2])*factor);
}

_STATIC_ _INLINE_ int16_t M2_Diff(Vector_t err, _FLOAT_ factor){
	return (int16_t)((err.v[0] - err.v[1] - MOTOR_CONSTELLATION*err.v[2])*factor);
}

_STATIC_ _INLINE_ int16_t M3_Diff(Vector_t err, _FLOAT_ factor){
	return (int16_t)((-err.v[0] - err.v[1] + MOTOR_CONSTELLATION*err.v[2])*factor);
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

void ControlLoop_run(Quaternion_t *q,
		     ControlParams_t *control,
		     RC_Data_t *remote_control,
			 Vector_i32_t *omega,
		     Motorcontrolvalues_t *motors){

	int16_t diff;
	uint16_t throttle = remote_control->throttle/4;
	Vector_t err;
	Vector_t derr_dt;

	if(throttle > 37){ /* Of 1000 */
		Vect_write_three_values(&err,
					q->q[1],
					q->q[2],
					q->q[3]);
		Vect_transform_i32_to_float_with_mult(omega, &derr_dt, 0.1);

		Motors_set_all_data_speed(motors, throttle);

		diff = M0_Diff(err, control->P) +
				M0_Diff(derr_dt, control->D);
		motors->motorspeeds[0] = ClipMotorSpeedValue(motors->motorspeeds[0], diff, throttle);

		diff = M1_Diff(err, control->P) +
				M1_Diff(derr_dt, control->D);
		motors->motorspeeds[1] = ClipMotorSpeedValue(motors->motorspeeds[1], diff, throttle);

		diff = M2_Diff(err, control->P) +
				M2_Diff(derr_dt, control->D);
		motors->motorspeeds[2] = ClipMotorSpeedValue(motors->motorspeeds[2], diff, throttle);

		diff = M3_Diff(err, control->P) +
				M3_Diff(derr_dt, control->D);
		motors->motorspeeds[3] = ClipMotorSpeedValue(motors->motorspeeds[3], diff, throttle);

	}else{
		Motors_set_all_data_speed(motors, 0);
		/* reset the Rotation of the drone */
		q->q[0] = 1.0f;
		q->q[1] = 0.0f;
		q->q[2] = 0.0f;
		q->q[3] = 0.0f;
	}

}
