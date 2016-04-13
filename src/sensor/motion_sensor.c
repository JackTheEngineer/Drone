/*
 * motion_sensor.c
 *
 *  Created on: Apr 13, 2016
 *      Author: jakov
 */

#include "motion_sensor.h"

void motion_sensor_set_data_zero(Sensordata_t *sensordata){
	sensordata->accel_data.x = 0;
	sensordata->accel_data.y = 0;
	sensordata->accel_data.z = 0;
	sensordata->gyro_data.x = 0;
	sensordata->gyro_data.y = 0;
	sensordata->gyro_data.z = 0;
	sensordata->compass_data.x = 0;
	sensordata->compass_data.y = 0;
	sensordata->compass_data.z = 0;
}

void motion_senor_get_data(Sensordata_t *sensordata){

}
