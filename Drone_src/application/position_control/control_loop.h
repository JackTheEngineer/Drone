/*
 * control_loop.h
 *
 *  Created on: Nov 17, 2019
 *      Author: jakov
 */

#ifndef DRONE_SRC_APPLICATION_POSITION_CONTROL_CONTROL_LOOP_H_
#define DRONE_SRC_APPLICATION_POSITION_CONTROL_CONTROL_LOOP_H_

#include "base.h"
#include "motors.h"
#include "rc_control.h"

typedef struct ControlParams{
	Vector_t P;
	Vector_t I;
	Vector_t D;
	Vector_t sum_err;
	Vector_t last_err;
}ControlParams_t;

void ControlLoop_run(Quaternion_t const *q, ControlParams_t const *control,
		RC_Data_t const *remote_control, Motorcontrolvalues_t *motors);

#endif /* DRONE_SRC_APPLICATION_POSITION_CONTROL_CONTROL_LOOP_H_ */
