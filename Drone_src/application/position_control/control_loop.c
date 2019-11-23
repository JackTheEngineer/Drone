/*
 * control_loop.c
 *
 *  Created on: Nov 17, 2019
 *      Author: jakov
 */


#include "madgwickFilter.h"
#include "control_loop.h"
#include <math.h>

void ControlLoop_run(Quaternion_t const *q, ControlParams_t const *control,
		RC_Data_t const *remote_control, Motorcontrolvalues_t *motors){

	int16_t diff;
	Vector_t err;
	Vect_write_three_values(&err,
				q->q[1],
				q->q[2],
				q->q[3]);
#define MOTOR_CONSTELLATION (-1)

	Motors_set_all_data_speed(motors, remote_control->throttle/4);

	diff = (int16_t)((-err.v[0] + err.v[1] - MOTOR_CONSTELLATION*err.v[2])*control->P.v[0]);
	motors->motorspeeds[0] = ((motors->motorspeeds[0] + diff) < 0) ? 0 : (motors->motorspeeds[0] + diff);

	diff = (int16_t)((err.v[0] + err.v[1] + MOTOR_CONSTELLATION*err.v[2])*control->P.v[0]);
	motors->motorspeeds[1] = ((motors->motorspeeds[1] + diff) < 0) ? 0 : (motors->motorspeeds[1] + diff);

	diff = (int16_t)((err.v[0] - err.v[1] - MOTOR_CONSTELLATION*err.v[2])*control->P.v[0]);
	motors->motorspeeds[2] = ((motors->motorspeeds[2] + diff) < 0) ? 0 : (motors->motorspeeds[2] + diff);

	diff = (int16_t)((-err.v[0] - err.v[1] + MOTOR_CONSTELLATION*err.v[2])*control->P.v[0]);
	motors->motorspeeds[3] = ((motors->motorspeeds[3] + diff) < 0) ? 0 : (motors->motorspeeds[3] + diff);

	if(remote_control->throttle < 150){
		Motors_set_all_data_speed(motors, 0);
	}
}
