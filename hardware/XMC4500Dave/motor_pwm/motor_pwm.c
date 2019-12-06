/*
 * pwm.c
 *
 *  Created on: Apr 18, 2016
 *      Author: Jakov
 */

#include "motor_pwm.h"
#include "hardware.h"

PWM_CCU8_t *pwms[NUM_OF_MOTORS] = {
		&PWM_CCU8_0,
		&PWM_CCU8_3,
		&PWM_CCU8_1,
		&PWM_CCU8_2,
};

void PWM_Init(void){
	for(uint8_t i=0; i < NUM_OF_MOTORS; i++){
		PWM_CCU8_Init(pwms[i]);
		PWM_CCU8_SetFreqSymmetric(pwms[i], 125);
		PWM_CCU8_Start(pwms[i]);
	}
}

/* Requires speed value from 0 - 1000 */
void PWM_Motor_Set_Rate(uint16_t Speed, uint8_t motor_index){
	uint32_t duty_cycle;
	if(motor_index >= NUM_OF_MOTORS){
		return;
	}

	if(Speed >= 32768){
		Speed = 0;
	}
	if(Speed > 1000){
		Speed = 1000;
	}
	duty_cycle = 1250 + (((uint32_t)Speed)*1250)/1000;

	/* PWM CCU Set Dutycycle accepts dutycycle from
	 * 0 - 10000. with 125 Hz, 8m period,
	 * 1ms pulse means 12,5% dutycycle
	 * 2ms pulse means 25,5% dutycycle
	 */
	PWM_CCU8_SetDutyCycleSymmetric(pwms[motor_index], XMC_CCU8_SLICE_COMPARE_CHANNEL_1, duty_cycle);
}
