/*
 * fake_motion_sensor.h
 *
 *  Created on: Aug 1, 2016
 *      Author: chocolate
 */

#ifndef TEST_FAKES_FAKE_MOTION_SENSOR_H_
#define TEST_FAKES_FAKE_MOTION_SENSOR_H_

#include "motion_sensor.h"

#define GYRO_MAX_SPEED 250 // +- 250 °/sec
#define MAX_ACCELERATION 2 // +- 2   g

#define SENSOR_ADC_RES 16 /* Sensor ADC resolution in bits */

#define OFFSET_X_FROM_CENTER 0.03 // -> 3cm
#define OFFSET_Y_FROM_CENTER 0.00

#endif /* TEST_FAKES_FAKE_MOTION_SENSOR_H_ */
