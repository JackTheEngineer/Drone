/*
n * fake_motors.h
 *
 *  Created on: Jul 31, 2016
 *      Author: chocolate
 */

#ifndef TEST_FAKES_FAKE_MOTORS_H_
#define TEST_FAKES_FAKE_MOTORS_H_

#include "motors.h"
#include "motordata_def.h"
#include "dronedata_def.h"

void fake_Motor_calculate_currents_from_controlvalues(Motor_t motors[NMBR_OF_MOTORS], Motorcontrolvalues_t *motor_values);
void fake_Motor_calculate_speeds_from_currents(Motor_t motors[NMBR_OF_MOTORS]);
void fake_Motor_calculate_thrust_from_speed(Motor_t motors[NMBR_OF_MOTORS]);
Physical_Drone_t *fake_Motors_get_drone_pointer(void);

#endif /* TEST_FAKES_FAKE_MOTORS_H_ */
