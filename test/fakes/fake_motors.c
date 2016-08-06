/*
 * fake_motors.c
 *
 *  Created on: Jul 31, 2016
 *      Author: chocolate
 */

#include "fake_motors.h"
#include "simulation.h"

void Motors_Init(void){

}

void Motors_Set_Speed(Motorcontrolvalues_t *motor_values){
	Simulation_recieve(motor_values);
}

void fake_Motor_calculate_currents_from_controlvalues(Motor_t motors[NMBR_OF_MOTORS], Motorcontrolvalues_t *motor_values){
	uint32_t i;

	for(i = 0; i<NMBR_OF_MOTORS; i++){
		motors[i].current = motor_values->motorspeeds[i];
	}
}

void fake_Motor_calculate_speeds_from_currents(Motor_t motors[NMBR_OF_MOTORS]){
	uint32_t i;

	for(i=0;i<NMBR_OF_MOTORS; i++){
		Vect_write(&(motors[i].speed), 3, motors[i].current * CURRENT_TO_SPEED);
	}
}

void fake_Motor_calculate_thrust_from_speed(Motor_t motors[NMBR_OF_MOTORS]){
    uint8_t i;
    double speed;

    for(i=0; i<NMBR_OF_MOTORS; i++){
        speed = Vect_read(&(motors[i].speed), 3);
        Vect_write(&(motors[i].thrust), 3, (SPEED_TO_THRUST*SQR(speed))); 
    }
}
