/*
 * pwm.h
 *
 *  Created on: Apr 18, 2016
 *      Author: jakov
 */

#ifndef PWM_H_
#define PWM_H_

#include "base.h"

void PWM_Init(void);
void PWM_Motor1_Set_Rate(uint16_t Speed);
void PWM_Motor2_Set_Rate(uint16_t Speed);
void PWM_Motor3_Set_Rate(uint16_t Speed);
void PWM_Motor4_Set_Rate(uint16_t Speed);

#endif /* PWM_H_ */
