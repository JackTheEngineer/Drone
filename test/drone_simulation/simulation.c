/*
 * simulation_connection.c
 *
 *  Created on: Jul 28, 2016
 *      Author: chocolate
 */

#include "motion_sensor.h"
#include "drone_physics.h"
#include "simulation.h"

Physical_Drone_t *drone;

#define TIMESTEP (double)0.0001 // 100 us
#define SIMULATIONTIME (double)2.0
#define ITERATIONS (uint32_t)(SIMULATIONTIME/TIMESTEP)

void Simulation_init(Physical_Drone_t *dronedata_from_external){
    drone = dronedata_from_external;
    Drone_set_drone_data_zero(drone);
    Drone_set_position(0, 0, 100.0, drone); // Set drone up in the sky
}

void Simulation_recieve(Physical_Drone_t *drone){
        Drone_calculate_next_values(drone, TIMESTEP);
}

void Simulation_write_sensordata(Sensordata_t *sensordata){
    
}





