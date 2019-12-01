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
#include "vector_operations.h"
#include "quaternions.h"
#include "control_params.h"


bool ControlLoop_run(Quaternion_t *errorQuaternion,
					ControlParams_t *control,
					Vector_t *omega,
					 uint16_t throttle, /* a value from 0 - 1000 */
					Motorcontrolvalues_t *motors);

#endif /* DRONE_SRC_APPLICATION_POSITION_CONTROL_CONTROL_LOOP_H_ */
