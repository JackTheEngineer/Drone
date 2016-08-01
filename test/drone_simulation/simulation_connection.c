/*
 * simulation_connection.c
 *
 *  Created on: Jul 28, 2016
 *      Author: chocolate
 */

#include "motors.h"
#include "motion_sensor.h"
#include "simulation_connection.h"

#include "drone_physics.h"

// Simulation Time Step im s
#define TIMESTEP (double)0.0001 // 100 us
#define SIMULATIONTIME (double)2.0
#define ITERATIONS (int)(SIMULATIONTIME/TIMESTEP)

_STATIC_ void Simulation_run_value_calculations_multiple_times(Motorcurrents_t *motorcurrents);

POINTER_TO_CONTAINER(Physical_Drone_t, drone);

void Simulation_recieve_motorcurrents(Motorcurrents_t *motorcurrents){
    // Motorcurrent values are from 0 - 1000
	Simulation_run_value_calculations_multiple_times(motorcurrents);
}

void Simulation_write_sensordata(Sensordata_t *sensordata){

}

_STATIC_ void Simulation_run_value_calculations_multiple_times(Motorcurrents_t *motorcurrents){

}

