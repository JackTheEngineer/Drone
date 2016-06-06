/*
 * drone_simulation.c
 *
 *  Created on: Apr 23, 2016
 *      Author: jakov
 */
#include "drone_simulation.h"
#include "propeller.h"

_STATIC_ double sum_of_vertical_force_components(Rotor_Speeds_t *rotorspeeds);

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

void drone_calculate_next_values(Drone_Data_t *dronedata, Rotor_Speeds_t *rotorspeeds, double timestep){
    double new_z;
    double new_v;
    
    double z_force = sum_of_vertical_force_components(rotorspeeds);

    new_z = dronedata->position.z + dronedata->speed.z*timestep;
    new_v = dronedata->speed.z + z_force*timestep;
        
    dronedata->position.z = new_z;
    dronedata->speed.z = new_v;
}

void drone_set_position(double x, double y, double z, Drone_Data_t* dronedata){
    dronedata->position.x = x;
    dronedata->position.y = y;
    dronedata->position.z = z;
}

_STATIC_ double sum_of_vertical_force_components(Rotor_Speeds_t *rotorspeeds){
    double vertical_force; 
    vertical_force = propeller_force_of_rpm(rotorspeeds->motor_1); 
    vertical_force += propeller_force_of_rpm(rotorspeeds->motor_2); 
    vertical_force += propeller_force_of_rpm(rotorspeeds->motor_3); 
    vertical_force += propeller_force_of_rpm(rotorspeeds->motor_4); 
    vertical_force += GRAVITY_CONST;
    return vertical_force;
}
