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
	uint32_t motorspeeds[NMBR_OF_MOTORS];
}Motorcontrolvalues_t;

void Motors_Init(void);
void Motors_Set_Speed(Motorcontrolvalues_t *motorspeeds);

#endif /* MOTORS_H_ */
