/*
 * drone_constants.h
 *
 *  Created on: Aug 1, 2016
 *      Author: chocolate
 */

#ifndef TEST_DRONE_SIMULATION_DRONE_CONSTANTS_H_
#define TEST_DRONE_SIMULATION_DRONE_CONSTANTS_H_

#include "base.h"

#define MOTOR_KV 1100
#define AKKU_VOLTAGE 11

#define NMBR_OF_MOTORS 4

#define PROPELLER_MASS 0.015 // kg
#define PROPELLER_LENGTH 0.25 // m
#define PROPELLER_L = 1/12 * PROPELLER_MASS * SQR(PROPELLER_LENGTH)

#define GRAVITY_CONST -9.81

#endif /* TEST_DRONE_SIMULATION_DRONE_CONSTANTS_H_ */
