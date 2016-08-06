/*
n * fake_motors.h
 *
 *  Created on: Jul 31, 2016
 *      Author: chocolate
 */

#ifndef TEST_FAKES_FAKE_MOTORS_H_
#define TEST_FAKES_FAKE_MOTORS_H_

#include "motors.h"
#include "vector_operations.h"

#define VALUES_TO_CURRENTS 0.002
#define CURRENT_TO_SPEED 10.0
#define SPEED_TO_THRUST  10.0

typedef struct Motor{
	double current;
	Vector_t torque;
	Vector_t speed;
	Vector_t thrust;
	Vector_t position;
}Motor_t;

void fake_Motor_calculate_currents_from_controlvalues(Motor_t motors[NMBR_OF_MOTORS], Motorcontrolvalues_t *motor_values);
void fake_Motor_calculate_speeds_from_currents(Motor_t motors[NMBR_OF_MOTORS]);
void fake_Motor_calculate_thrust_from_speed(Motor_t motors[NMBR_OF_MOTORS]);

#endif /* TEST_FAKES_FAKE_MOTORS_H_ */
