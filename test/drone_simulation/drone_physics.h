/*
 * drone_simulation.h
 *
 *  Created on: Apr 23, 2016
 *      Author: jakov
 */

#ifndef TEST_SUPPORT_DRONE_SIMULATION_H_
#define TEST_SUPPORT_DRONE_SIMULATION_H_

#include "base.h"
#include "physical_definitions.h"
#include "drone_constants.h"
#include "vector_operations.h"

/* Used as Absolute position from a point Zero */
typedef struct Drone_Data{
	Vector_t position;
	Vector_t angular_position;
	Vector_t speed;
	Vector_t angular_speed;
	Vector_t rotorspeeds[NMBR_OF_MOTORS];
}Physical_Drone_t;

typedef struct Rotor_Moments{
	double moments[NMBR_OF_MOTORS];
}Rotor_Moments_t;

void Drone_set_position(double x, double y, double z, Physical_Drone_t* dronedata);
void Drone_calculate_next_values(Physical_Drone_t *dronedata, Rotor_Moments_t *rotormoments, double timestep);
void Drone_set_drone_data_zero(Physical_Drone_t *dronedata);
void Drone_set_speed(double x, double y, double z, Physical_Drone_t *dronedata);
void Drone_set_angular_position(double pitch, double roll, double yaw, Physical_Drone_t *dronedata);

#endif /* TEST_SUPPORT_DRONE_SIMULATION_H_ */
