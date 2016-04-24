/*
 * drone_simulation.c
 *
 *  Created on: Apr 23, 2016
 *      Author: jakov
 */
#include "drone_simulation.h"

void drone_set_drone_data_zero(Drone_Data_t *dronedata){
	dronedata->position.x = 0;
	dronedata->position.y = 0;
	dronedata->position.z = 0;
	dronedata->angular_position.x = 0;
	dronedata->angular_position.y = 0;
	dronedata->angular_position.z = 0;
	dronedata->angular_speed.x = 0;
	dronedata->angular_speed.y = 0;
	dronedata->angular_speed.z = 0;
	dronedata->speed.x = 0;
	dronedata->speed.y = 0;
	dronedata->speed.z = 0;
}
/* timestep in seconds */
void drone_calculate_next_values(Drone_Data_t *dronedata, Rotor_Speeds_t *rotorspeeds, double timestep){
}
