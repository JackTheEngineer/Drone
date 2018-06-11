/*
 * pwm.h
 *
 *  Created on: Apr 18, 2016
 *      Author: jakov
 */

#ifndef PWM_H_
#define PWM_H_

#include "base.h"

#define NUM_OF_MOTORS 4

void PWM_Init(void);
void PWM_Motor_Set_Rate(uint16_t Speed, uint8_t motor_index);

#endif /* PWM_H_ */
