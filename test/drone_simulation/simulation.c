/*
 * simulation_connection.c
 *
 *  Created on: Jul 28, 2016
 *      Author: chocolate
 */

#include "fake_motors.h"
#include "motion_sensor.h"
#include "drone_physics.h"
#include "simulation.h"

// Simulation Time Step im s
#define TIMESTEP (double)0.0001 // 100 us
#define SIMULATIONTIME (double)2.0
#define ITERATIONS (int)(SIMULATIONTIME/TIMESTEP)

POINTER_TO_CONTAINER(Physical_Drone_t, drone);
Motor_t motors[4];

_STATIC_ void Simulation_run_value_calculations_multiple_times(Motor_t motors[NMBR_OF_MOTORS]);


void Simulation_recieve(Motorcontrolvalues_t *motor_values){
	/* Here the drone simulation and the motor value information gets injected,
	 * thus makes this function not purely dependent on it's parameters.
	 */
	fake_Motor_calculate_currents_from_controlvalues(motors, motor_values);
	fake_Motor_calculate_speeds_from_currents(motors);
	Simulation_run_value_calculations_multiple_times(motors);
}

void Simulation_write_sensordata(Sensordata_t *sensordata){

}

_STATIC_ void Simulation_run_value_calculations_multiple_times(Motor_t motors[NMBR_OF_MOTORS]){

}

