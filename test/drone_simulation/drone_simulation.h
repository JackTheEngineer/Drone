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
#include "vector_operations.h"


#define MOTOR_KV 1100
#define AKKU_VOLTAGE 11

#define PROPELLER_MASS 0.015 // kg
#define PROPELLER_LENGTH 0.25 // m
#define PROPELLER_L = 1/12 * PROPELLER_MASS * SQR(PROPELLER_LENGTH)

#define GRAVITY_CONST -9.81

/* Used as Absolute position from a point Zero */
typedef struct Drone_Data{
	Vector_t position;
	Vector_t angular_position;
	Vector_t speed;
	Vector_t angular_speed;
	Vector_t rotorspeeds[4];
}Physical_Drone_Data_t;

typedef struct Rotor_Speeds{
	double moments[4];
}Rotor_Moments_t;

void Drone_set_position(double x, double y, double z, Physical_Drone_Data_t* dronedata);
void Drone_calculate_next_values(Physical_Drone_Data_t *dronedata, Rotor_Moments_t *rotormoments, double timestep);
void Drone_set_drone_data_zero(Physical_Drone_Data_t *dronedata);

#endif /* TEST_SUPPORT_DRONE_SIMULATION_H_ */
