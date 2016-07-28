/*
 * drone_simulation.c
 *
 *  Created on: Apr 23, 2016
 *      Author: jakov
 */
#include "drone_simulation.h"
#include "propeller.h"

void Drone_set_drone_data_zero(Physical_Drone_Data_t *dronedata){
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

void Drone_calculate_next_values(Physical_Drone_Data_t *dronedata, Rotor_Moments_t *rotormoments, double timestep){
    /*
    Step by step:
    calculate sum of moments( -> force of propeller, and the increased speed of rotation both) ( copter_system)
    sum of forces on the copter (forces on the mass center), and external forces like wind
    calculate acceleration, angular acceleration, angular speed, angular position, speed, angular position,
    calculate faked motion sensor values
    Ldot = sumofmoments
    */

	Vector_t sum_of_moments_container;
	Vector_t *sum_of_moments = &(sum_of_moments_container);
	Vector_t sum_of_forces_container;
	Vector_t *sum_of_forces = &(sum_of_forces_container);

	Vector_t motor_moments[4]; 		// forces of propellers (cross multiply) radiuses [0 - 3], and moments of propellers [4 - 7]
	Vector_t propeller_moments[4]; 	// Moments that come from inertia of the propellers
	Vector_t forces[4]; 			// propeller forces [0 - 3]


}

void Drone_set_position(double x, double y, double z, Physical_Drone_Data_t *dronedata){
    Vector_t *position = &(dronedata->position);
	Vect_write(position, 1, x);
	Vect_write(position, 2, y);
	Vect_write(position, 3, z);
}

void Drone_set_speed(double x, double y, double z, Physical_Drone_Data_t *dronedata){
	Vector_t *speed = &(dronedata->speed);
	Vect_write(speed, 1, x);
	Vect_write(speed, 2, y);
	Vect_write(speed, 3, z);
}

void Drone_set_angular_position(double pitch, double roll, double yaw, Physical_Drone_Data_t *dronedata){
	Vector_t *angle = &(dronedata->angular_position);
	Vect_write(angle, 1, pitch);
	Vect_write(angle, 2, roll);
	Vect_write(angle, 3, yaw);
}
