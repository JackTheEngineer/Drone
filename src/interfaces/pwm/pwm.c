/*
 * pwm.c
 *
 *  Created on: Apr 18, 2016
 *      Author: jakov
 */

#include "pwm.h"

void PWM_Motor1_Set_Rate(uint32_t Speed){

}
void PWM_Motor2_Set_Rate(uint32_t Speed){

}
void PWM_Motor3_Set_Rate(uint32_t Speed){

}
void PWM_Motor4_Set_Rate(uint32_t Speed){

}

void PWM_Init(void){
	XMC_CCU4_Init(MODULE_PTR,  XMC_CCU4_SLICE_MCMS_ACTION_TRANSFER_PR_CR);
	XMC_CCU4_StartPrescaler(MODULE_PTR);
	XMC_CCU4_SetModuleClock(MODULE_PTR, XMC_CCU4_CLOCK_SCU);
	XMC_CCU4_SLICE_CompareInit(SLICE0_PTR, &SLICE0_config);
	XMC_CCU4_SLICE_SetTimerCompareMatch(SLICE0_PTR, 21000);
	XMC_CCU4_SLICE_SetTimerPeriodMatch(SLICE0_PTR, 62499U);
}


