/*
 * pwm.c
 *
 *  Created on: Apr 18, 2016
 *      Author: Jakov
 */

#include "motor_pwm.h"
#include "hardware.h"

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

	if(Speed >= 32768){
		Speed = 0;
	}

	if(Speed > 1000){
		Speed = 1000;
	}

	duty_cycle = 470 + Speed/2;
	/*
	 PWM accepts duty cycle from 0 to 10000.
	 Assuming the pwm period is configured for 20ms
	 duty cycle value of 1000 means 2 ms ON Time,
	 duty cycle value of 500 means 1 ms ON Time,
	 */
	PWM_CCU4_SetDutyCycle(pwms[motor_index], duty_cycle);
}
