/*
 * motor.c
 *
 *  Created on: Apr 18, 2016
 *      Author: jakov
 */

#include "motors.h"
#include "motor_pwm.h"

void Motors_set_all_data_speed(Motorcontrolvalues_t *motorspeeds, uint16_t value){
	for(uint8_t i=0; i < NUM_OF_MOTORS; i++){
		motorspeeds->motorspeeds[i] = value;
	}
}

void Motors_act_on_pwm(Motorcontrolvalues_t *motorspeeds){
	for(uint8_t i=0; i < NUM_OF_MOTORS; i++){
		PWM_Motor_Set_Rate(motorspeeds->motorspeeds[i], i);
	}
}




