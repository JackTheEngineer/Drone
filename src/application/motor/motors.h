/*
 * motor.h
 *
 *  Created on: Apr 18, 2016
 *      Author: jakov
 */

#ifndef MOTORS_H_
#define MOTORS_H_

#include "base.h"
#include "drone_constants.h"

typedef struct motors{
	uint16_t motorspeeds[NMBR_OF_MOTORS];
}Motorcontrolvalues_t;

void Motors_set_all_data_speed(Motorcontrolvalues_t *motorspeeds, uint16_t value);
void Motors_act_on_pwm(Motorcontrolvalues_t *motorspeeds);

#endif /* MOTORS_H_ */
