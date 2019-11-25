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
	Vector_t P;
	Vector_t I;
	Vector_t D;
	Vector_t sum_err;
	Vector_t last_err;
}ControlParams_t;


#endif /* DRONE_SRC_APPLICATION_POSITION_CONTROL_CONTROL_PARAMS_H_ */
