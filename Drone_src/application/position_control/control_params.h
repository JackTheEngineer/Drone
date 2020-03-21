/*
 * control_params.h
 *
 *  Created on: Nov 24, 2019
 *      Author: jakov
 */

#ifndef DRONE_SRC_APPLICATION_POSITION_CONTROL_CONTROL_PARAMS_H_
#define DRONE_SRC_APPLICATION_POSITION_CONTROL_CONTROL_PARAMS_H_

#include "vector_operations.h"

typedef struct ControlParams{
	_FLOAT_ P;
	_FLOAT_ D;
	_FLOAT_ I;
	Vector_t integralErr;
}ControlParams_t;


#endif /* DRONE_SRC_APPLICATION_POSITION_CONTROL_CONTROL_PARAMS_H_ */
