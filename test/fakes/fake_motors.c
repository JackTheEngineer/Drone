/*
 * fake_motors.c
 *
 *  Created on: Jul 31, 2016
 *      Author: chocolate
 */

#include "fake_motors.h"
#include "simulation_connection.h"


void Motors_Set_Speed(Motorcontrolvalues_t *motor_values){
	POINTER_TO_CONTAINER(Motorcurrents_t, currents);

	fake_Motors_calculate_motorcurrent_of_control_value(motor_values, currents);
	Simulation_recieve_motorcurrents(currents);
}


void fake_Motors_calculate_motorcurrent_of_control_value(Motorcontrolvalues_t *motor_values, Motorcurrents_t *currents){
	uint32_t i;

	for(i=0; i<NMBR_OF_MOTORS;i++){
		currents->currents[i] = (double)motor_values->motorspeeds[i] * VALUES_TO_CURRENTS;
	}
}
