/*
 * fake_motion_sensor.c
 *
 *  Created on: Aug 1, 2016
 *      Author: chocolate
 */

#include "fake_motion_sensor.h"
#include "drone_physics.h"


void Motion_sensor_set_data_zero(Sensordata_t *sensordata){
	Vect_i32_set_all_values_to(&sensordata->acceleration, 0);
	Vect_i32_set_all_values_to(&sensordata->angle_speed, 0);
	Vect_i32_set_all_values_to(&sensordata->magnetic_field, 0);
	sensordata->settings.afs_sel = 0;
	sensordata->settings.fs_sel = 0;
}

void Motion_sensor_get_data(Sensordata_t *sensordata){
	Physical_Drone_t *drone = Drone_get_dronedata();
	uint8_t afs_sel = sensordata->settings.afs_sel;

	Vect_transform_float_to_i32_with_limits(&drone->acceleration, &sensordata->acceleration, SENSOR_ADC_RES, (1<<(afs_sel+1)));
}
