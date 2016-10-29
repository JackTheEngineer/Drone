/*
 * motion_sensor.c
 *
 *  Created on: Apr 13, 2016
 *      Author: jakov
 */

#include "motion_sensor.h"

void Motion_sensor_set_data_zero(Sensordata_t *sensordata){
	Vect_i32_set_all_values_to(&sensordata->acceleration, 0);
	Vect_i32_set_all_values_to(&sensordata->angle_speed, 0);
	Vect_i32_set_all_values_to(&sensordata->magnetic_field, 0);
}


void Motion_sensor_get_data(Sensordata_t *sensordata){
	
}
