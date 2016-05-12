/*
 * motion_sensor.h
 *
 *  Created on: Apr 13, 2016
 *      Author: jakov
 */

#ifndef SRC_SENSOR_MOTION_SENSOR_H_
#define SRC_SENSOR_MOTION_SENSOR_H_

#include "base.h"

typedef struct Accelerometer{
	int32_t x;
	int32_t y;
	int32_t z;
}Accelerometer_t;

typedef struct Gyroscope{
	int32_t x;
	int32_t y;
	int32_t z;
}Gyroscope_t;

typedef struct Compass{
	int32_t x;
	int32_t y;
	int32_t z;
}Compass_t;

typedef struct Sensordata{
	Accelerometer_t accel_data;
	Gyroscope_t gyro_data;
	Compass_t compass_data;
}Sensordata_t;

void motion_senor_get_data(Sensordata_t *sensordata);
void motion_sensor_set_data_zero(Sensordata_t *sensordata);


#endif /* SRC_SENSOR_MOTION_SENSOR_H_ */
