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
#include "vector_operations.h"
#include "quaternions.h"
#include "control_params.h"


void ControlLoop_run(Quaternion_t *q,
					ControlParams_t *control,
					RC_Data_t *remote_control,
					Motorcontrolvalues_t *motors);

#endif /* DRONE_SRC_APPLICATION_POSITION_CONTROL_CONTROL_LOOP_H_ */
