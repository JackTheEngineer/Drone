/*
 * control_loop.c
 *
 *  Created on: Nov 17, 2019
 *      Author: jakov
 */


#include "madgwickFilter.h"
#include "control_loop.h"
#include <math.h>

void ControlLoop_run(Quaternion_t *q, ControlParams_t *control, Motorcontrolvalues_t *motors){
	_FLOAT_ theta_2 = acos(q->q[0]);
	Vector_t err;
	Vect_write_three_values(&err, q->q[1], q->q[2], q->q[3]);
	/* X value */
	motors->motorspeeds[0] += (int16_t)(-err.v[0] + err.v[1] - err.v[2])*control->P.v[0]*theta_2;
	motors->motorspeeds[1] += (int16_t)(err.v[0] + err.v[1] + err.v[2])*control->P.v[0]*theta_2;
	motors->motorspeeds[2] += (int16_t)(err.v[0] - err.v[1] - err.v[2])*control->P.v[0]*theta_2;
	motors->motorspeeds[3] += (int16_t)(-err.v[0] - err.v[1] + err.v[2])*control->P.v[0]*theta_2;
}
