/*
 * fake_motion_sensor.c
 *
 *  Created on: Aug 1, 2016
 *      Author: chocolate
 */

#include "fake_motion_sensor.h"
#include "simulation_connection.h"

void Motion_sensor_get_data(Sensordata_t *sensordata){
	Simulation_write_sensordata(sensordata);
}
