/*
 * motion_sensor.c
 *
 *  Created on: Apr 13, 2016
 *      Author: jakov
 */

#include "motion_sensor.h"
#include "msensor_iface.h"

void Motion_sensor_init(Sensordata_t *sensordata){
	Motion_sensor_set_data_zero(sensordata);
	MSensor_Iface_Init();
}

void Motion_sensor_set_data_zero(Sensordata_t *sensordata){
	Vect_i32_set_all_values_to(&sensordata->acceleration, 0);
	Vect_i32_set_all_values_to(&sensordata->angle_speed, 0);
	Vect_i32_set_all_values_to(&sensordata->magnetic_field, 0);
}

void Motion_sensor_get_data(Sensordata_t *sensordata){
	if(sensordata == NULL){return;}
	uint8_t vals[6];
	MSensor_Iface_readBytes(ACCEL_XOUT_H, &vals[0], 6);
	Vect_i32_write(&sensordata->acceleration, 1, (int16_t)((((int16_t)vals[0]) << 8)|(vals[1])));
	Vect_i32_write(&sensordata->acceleration, 2, (int16_t)((((int16_t)vals[2]) << 8)|(vals[3])));
	Vect_i32_write(&sensordata->acceleration, 3, (int16_t)((((int16_t)vals[4]) << 8)|(vals[5])));
	MSensor_Iface_readBytes(GYRO_XOUT_H, &vals[0], 6);
	Vect_i32_write(&sensordata->angle_speed, 1, (int16_t)((((int16_t)vals[0]) << 8)|(vals[1])));
	Vect_i32_write(&sensordata->angle_speed, 2, (int16_t)((((int16_t)vals[2]) << 8)|(vals[3])));
	Vect_i32_write(&sensordata->angle_speed, 3, (int16_t)((((int16_t)vals[4]) << 8)|(vals[5])));
}


       
