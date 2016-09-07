/*
 * simulation_connection.c
 *
 *  Created on: Jul 28, 2016
 *      Author: chocolate
 */

#include "motion_sensor.h"
#include "drone_physics.h"
#include "simulation.h"
#include "timestep_definition.h"

void Simulation_recieve(Physical_Drone_t *drone){
        Drone_calculate_next_values(drone, TIMESTEP);
}

void Simulation_write_sensordata(Sensordata_t *sensordata){
    
}





