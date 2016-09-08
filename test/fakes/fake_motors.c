/*
 * fake_motors.c
 *
 *  Created on: Jul 31, 2016
 *      Author: chocolate
 */

#include "fake_motors.h"
#include "simulation.h"
#include "drone_masses.h"

#define VALUES_TO_CURRENTS 0.002
#define CURRENT_TO_SPEED 10.0
#define SPEED_TO_THRUST 0.01

_STATIC_ void fake_Motors_initailize_positions(Motor_t motors[NMBR_OF_MOTORS]);

POINTER_TO_CONTAINER(Physical_Drone_t, dronedata);

Physical_Drone_t *fake_Motors_get_drone_pointer(void){
	return dronedata;
}

void Motors_Init(void){
    /* Here the dronedata gets injected */
    fake_Motors_initailize_positions(dronedata->motors);
}

void Motors_Set_Speed(Motorcontrolvalues_t *motor_values){
    /* Here the dronedata gets injected */
    fake_Motor_calculate_currents_from_controlvalues(dronedata->motors, motor_values);
    fake_Motor_calculate_speeds_from_currents(dronedata->motors);
    fake_Motor_calculate_thrust_from_speed(dronedata->motors);
    Simulation_recieve(dronedata);
}

_STATIC_ void fake_Motors_initailize_positions(Motor_t motors[NMBR_OF_MOTORS]){
    Vect_write_three_values(&(motors[0].position), OUTER_MASS_RADIUS, 0 , 0);
    Vect_write_three_values(&(motors[1].position), 0, OUTER_MASS_RADIUS, 0);
    Vect_write_three_values(&(motors[2].position),  -OUTER_MASS_RADIUS, 0 , 0);
    Vect_write_three_values(&(motors[3].position), 0, -OUTER_MASS_RADIUS, 0);
}

void fake_Motor_calculate_currents_from_controlvalues(Motor_t motors[NMBR_OF_MOTORS], Motorcontrolvalues_t *motor_values){
	uint32_t i;

	for(i = 0; i<NMBR_OF_MOTORS; i++){
		motors[i].current = motor_values->motorspeeds[i] * VALUES_TO_CURRENTS;
	}
}

void fake_Motor_calculate_speeds_from_currents(Motor_t motors[NMBR_OF_MOTORS]){
        Vect_write(&(motors[0].speed), 3, motors[0].current * CURRENT_TO_SPEED);
        Vect_write(&(motors[1].speed), 3, -motors[1].current * CURRENT_TO_SPEED);
        Vect_write(&(motors[2].speed), 3, motors[2].current * CURRENT_TO_SPEED);
        Vect_write(&(motors[3].speed), 3, -motors[3].current * CURRENT_TO_SPEED);
}

void fake_Motor_calculate_thrust_from_speed(Motor_t motors[NMBR_OF_MOTORS]){
    uint8_t i;
    double speed;

    for(i=0; i<NMBR_OF_MOTORS; i++){
        speed = Vect_read(&(motors[i].speed), 3);
        Vect_write(&(motors[i].thrust), 3, (SPEED_TO_THRUST*SQR(speed))); 
    }
}
