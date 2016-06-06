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
}Drone_Data_t;

typedef struct Rotor_Speeds{
	double motor_1;
	double motor_2;
	double motor_3;
	double motor_4;
}Rotor_Speeds_t;

void drone_set_position(double x, double y, double z, Drone_Data_t* dronedata);
void drone_calculate_next_values(Drone_Data_t *dronedata, Rotor_Speeds_t *rotorspeeds, double timestep);
void drone_set_drone_data_zero(Drone_Data_t *dronedata);

#endif /* TEST_SUPPORT_DRONE_SIMULATION_H_ */
