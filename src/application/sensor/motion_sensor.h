/*
 * motion_sensor.h
 *
 *  Created on: Apr 13, 2016
 *      Author: jakov
 */

#ifndef SRC_SENSOR_MOTION_SENSOR_H_
#define SRC_SENSOR_MOTION_SENSOR_H_

#include "base.h"
#include "vector_operations.h"

typedef struct Sensordata{
	Vector_i32_t acceleration;
	Vector_i32_t angle_speed;
	Vector_i32_t magnetic_field;
}Sensordata_t;

void Motion_sensor_get_data(Sensordata_t *sensordata);
void Motion_sensor_set_data_zero(Sensordata_t *sensordata);

#endif /* SRC_SENSOR_MOTION_SENSOR_H_ */
