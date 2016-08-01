/*
 * fake_motors.h
 *
 *  Created on: Jul 31, 2016
 *      Author: chocolate
 */

#ifndef TEST_FAKES_FAKE_MOTORS_H_
#define TEST_FAKES_FAKE_MOTORS_H_

#include "motors.h"

#define VALUES_TO_CURRENTS 0.002

typedef struct motorcurrents{
	double currents[NMBR_OF_MOTORS];
}Motorcurrents_t;

void fake_Motors_calculate_motorcurrent_of_control_value(Motorcontrolvalues_t *motor_values, Motorcurrents_t *currents);

#endif /* TEST_FAKES_FAKE_MOTORS_H_ */
