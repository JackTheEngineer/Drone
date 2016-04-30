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
/* Differential function that calculates the Position,
 *  speed, angular position and angular speed
 *
 * timestep in seconds
 */

_STATIC_ void calculate_g_acceleration(Drone_Data_t *dronedata, double timestep){
	double new_z;
	double new_v;

	new_z = dronedata->position.z + dronedata->speed.z*timestep;
	new_v = dronedata->speed.z + GRAVITY_CONST*timestep;

	dronedata->position.z = new_z;
	dronedata->speed.z = new_v;
}

void drone_calculate_next_values(Drone_Data_t *dronedata, Rotor_Speeds_t *rotorspeeds, double timestep){
	calculate_g_acceleration(dronedata, timestep);

}
