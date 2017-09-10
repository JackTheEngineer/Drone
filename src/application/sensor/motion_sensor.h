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

/*
 * CODES
 */
#define I2C_IF_DIS (1<<4) /* reset/disable the I2C interface */
#define H_RESET (1<<7)

/*
 * ADDRESSES
 */
#define ACCEL_XOUT_H 59
#define ACCEL_XOUT_L 60
#define ACCEL_YOUT_H 61
#define ACCEL_YOUT_L 62
#define ACCEL_ZOUT_H 63
#define ACCEL_ZOUT_L 64
#define TEMP_OUT_H 65
#define TEMP_OUT_L 66
#define GYRO_XOUT_H 67
#define GYRO_XOUT_L 68
#define GYRO_YOUT_H 69
#define GYRO_YOUT_L 70
#define GYRO_ZOUT_H 71
#define GYRO_ZOUT_L 72

#define USER_CTRL 106 /* User control register address */
#define PWR_MGMT_1 107
#define PWR_MGMT_2 108
#define WHO_AM_I 117

#define MPU9250_ADDRESS 0x68

#define MPU9250_USIC XMC_USIC2_CH0

typedef struct Settings{
	uint32_t afs_sel;
	uint32_t fs_sel;
}Sensor_settings_t;


typedef struct Sensordata{
	Vector_i32_t acceleration;
	Vector_i32_t angle_speed;
	Vector_i32_t magnetic_field;

	Sensor_settings_t settings;
}Sensordata_t;

void Motion_sensor_init(Sensordata_t *sensordata);
void Motion_sensor_get_data(Sensordata_t *sensordata);
void Motion_sensor_set_data_zero(Sensordata_t *sensordata);

#endif /* SRC_SENSOR_MOTION_SENSOR_H_ */
