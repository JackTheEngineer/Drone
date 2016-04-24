/*
 * drone_simulation.h
 *
 *  Created on: Apr 23, 2016
 *      Author: jakov
 */

#ifndef TEST_SUPPORT_DRONE_SIMULATION_H_
#define TEST_SUPPORT_DRONE_SIMULATION_H_

#include "base.h"

/* Simulation with four inner masses,
 * four outer masses
 * and four propellers
 */

// Definition of one mass
#define INNER_MASS 0.2 // in kg, there are four of them
#define INNER_MASS_RADIUS 0.1 // meter

#define OUTER_MASS  0.110
#define MOTOR_KV 1100
#define AKKU_VOLTAGE 11

#define PROPELLER_MASS 0.015 // kg
#define PROPELLER_LENGTH 0.25 // m
#define PROPELLER_L = 1/12 * PROPELLER_MASS * PROPELLER_LENGTH * PROPELLER_LENGTH

#define GRAVITY_CONST -9.81

typedef struct Vector{
	double x;
	double y;
	double z;
}Vector_t;

/* Used as Absoluta position from a point Zero */
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

void drone_calculate_next_values(Drone_Data_t *dronedata, Rotor_Speeds_t *rotorspeeds, double timestep);

#endif /* TEST_SUPPORT_DRONE_SIMULATION_H_ */
