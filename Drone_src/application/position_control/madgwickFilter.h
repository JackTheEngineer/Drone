/*
 * madgwickFilter.h
 *
 *  Created on: Nov 15, 2019
 *      Author: jakov
 */

#ifndef DRONE_SRC_APPLICATION_STATE_ESTIMATION_MADGWICKFILTER_H_
#define DRONE_SRC_APPLICATION_STATE_ESTIMATION_MADGWICKFILTER_H_

#include "vector_operations.h"
#include "quaternions.h"

void MadgwickAHRSupdate(Vector_t *g, Vector_t *, Vector_t *m, Quaternion_t *q);
void MadgwickAHRSupdateIMU(Vector_t *g, Vector_t *a, Quaternion_t *q);


#endif /* DRONE_SRC_APPLICATION_STATE_ESTIMATION_MADGWICKFILTER_H_ */
