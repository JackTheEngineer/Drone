/*
 * pwm.h
 *
 *  Created on: Apr 18, 2016
 *      Author: jakov
 */

#ifndef PWM_H_
#define PWM_H_

#include "base.h"
#include "xmc_ccu4.h"
#include "xmc_gpio.h"

#define MODULE_PTR      	CCU40
#define MODULE_NUMBER   	(0U)

#define SLICE0_PTR       	CCU40_CC40
#define SLICE0_NUMBER    	(0U)
#define SLICE0_OUTPUT		P0_0

void PWM_Init(void);
void PWM_Motor1_Set_Rate(uint32_t Speed);
void PWM_Motor2_Set_Rate(uint32_t Speed);
void PWM_Motor3_Set_Rate(uint32_t Speed);
void PWM_Motor4_Set_Rate(uint32_t Speed);

#endif /* PWM_H_ */
