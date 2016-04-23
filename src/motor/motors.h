/*
 * motor.h
 *
 *  Created on: Apr 18, 2016
 *      Author: jakov
 */

#ifndef MOTOR_H_
#define MOTOR_H_

#include "base.h"

typedef struct motors{
	uint32_t motor1_speed;
	uint32_t motor2_speed;
	uint32_t motor3_speed;
	uint32_t motor4_speed;
}Motors_t;

void Motors_Init(void);
void Motors_Set_Speed(Motors_t *motorspeeds);

#endif /* MOTOR_H_ */
