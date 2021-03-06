/*
 * fake_motors.c
 *
 *  Created on: Jul 31, 2016
 *      Author: chocolate
 */

#include "fake_motors.h"
#include "drone_masses.h"
#include "timestep_definition.h"
#include "drone_physics.h"

#define VALUES_TO_CURRENTS 0.002
#define CURRENT_TO_SPEED 10.0
#define SPEED_TO_THRUST 0.01

_STATIC_ void fake_Motors_initailize_positions(Motor_t motors[NMBR_OF_MOTORS]);

Physical_Drone_t *fake_Motors_get_drone_pointer(void){
	return Drone_get_dronedata();
}

void Motors_Init(void){
	/*
	 * Here the rather unaestethic fetching of the dronedata comes.
	 * it's because the real interface to the motors has to stay 
	 * independend of the dronedata. 
	 *
	 * The same counts for the motion sensor
	 */
       
	Physical_Drone_t* dronedata = Drone_get_dronedata();
	fake_Motors_initailize_positions(dronedata->motors);
	Drone_calculate_inverse_mass_matrix(&(dronedata->J_Inverse));
}

void Motors_Set_Speed(Motorcontrolvalues_t *motor_values){
	/*
	 * Here the rather unaestethic fetching of the dronedata comes.
	 * it's because the real interface to the motors has to stay 
	 * independend of the dronedata. 
	 *
	 * The same counts for the motion sensor
	 */

	Physical_Drone_t *dronedata = Drone_get_dronedata();
	fake_Motor_calculate_currents_from_controlvalues(dronedata->motors, motor_values);
	fake_Motor_calculate_speeds_from_currents(dronedata->motors);
	fake_Motor_calculate_thrust_from_speed(dronedata->motors);
	Drone_calculate_next_values(dronedata, TIMESTEP);
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
        Vect_write(&(motors[0].speed), 2, motors[0].current * CURRENT_TO_SPEED);
        Vect_write(&(motors[1].speed), 2, -motors[1].current * CURRENT_TO_SPEED);
        Vect_write(&(motors[2].speed), 2, motors[2].current * CURRENT_TO_SPEED);
        Vect_write(&(motors[3].speed), 2, -motors[3].current * CURRENT_TO_SPEED);
}

void fake_Motor_calculate_thrust_from_speed(Motor_t motors[NMBR_OF_MOTORS]){
    uint8_t i;
    double speed;

    for(i=0; i<NMBR_OF_MOTORS; i++){
        speed = Vect_read(&(motors[i].speed), 2);
        Vect_write(&(motors[i].thrust), 2, (SPEED_TO_THRUST*SQR(speed)));
    }
}
