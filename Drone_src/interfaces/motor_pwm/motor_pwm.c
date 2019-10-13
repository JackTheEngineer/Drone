/*
 * pwm.c
 *
 *  Created on: Apr 18, 2016
 *      Author: Jakov
 */

#include "motor_pwm.h"
#include "pwm_ccu4.h"
#include "pwm_ccu4_conf.h"

#define CPU_FREQU 144000000UL



PWM_CCU4_t *pwms[NUM_OF_MOTORS] = {
		&PWM_CCU4_0,
		&PWM_CCU4_1,
		&PWM_CCU4_2,
		&PWM_CCU4_3,
};

void PWM_Init(void){
	for(uint8_t i=0; i < NUM_OF_MOTORS; i++){
		PWM_CCU4_Init(pwms[i]);
		PWM_CCU4_SetFreqAndDutyCycle(pwms[i], 50, 0);
		PWM_CCU4_Start(pwms[i]);
	}
}

/* Requires speed value from 0 - 1000 */
void PWM_Motor_Set_Rate(uint16_t Speed, uint8_t motor_index){
	uint32_t duty_cycle;
	if(motor_index >= NUM_OF_MOTORS){
		return;
	}
	if(Speed > 1000){
		Speed = 1000;
	}
	duty_cycle = 498 + Speed/2;
	PWM_CCU4_SetDutyCycle(pwms[motor_index], duty_cycle);
}
