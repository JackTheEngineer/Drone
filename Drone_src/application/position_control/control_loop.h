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

typedef struct ControlParams{
	Vector_t P;
	Vector_t I;
	Vector_t D;
	Vector_t sum_err;
	Vector_t last_err;
}ControlParams_t;

void ControlLoop_run(Quaternion_t *q, ControlParams_t *control, Motorcontrolvalues_t *motors);

#endif /* DRONE_SRC_APPLICATION_POSITION_CONTROL_CONTROL_LOOP_H_ */
