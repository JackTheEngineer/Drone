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

typedef struct Vector{
	double x;
	double y;
	double z;
}Vector_t;

typedef struct Angles{
	double theta;
	double phi;
}Angles_t;

/* Used as Absoluta position from a point Zero */
typedef struct Drone_Data{
	Vector_t position;
	Angles_t angular_position;
	Vector_t speed;
	Vector_t angular_speed;
	Vector_t acceleration;
	Vector_t angular_acceleration;
}Drone_Data_t;

void drone_calculate_next_position(Drone_Data_t *dronedata);

#endif /* TEST_SUPPORT_DRONE_SIMULATION_H_ */
