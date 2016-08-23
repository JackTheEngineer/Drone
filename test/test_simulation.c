/*
 * test_simulation_connection.c
 *
 *  Created on: Jul 30, 2016
 *      Author: chocolate
 */

#include "drone_simulation/simulation.h"
#include "test_helper.h"
#include "motors.h"
#include "drone_physics.h"

TEST_GROUP(simulation_connection);
POINTER_TO_CONTAINER(Physical_Drone_t, drone);

TEST_SETUP(simulation_connection){
    Simulation_init(drone);
}

TEST_TEAR_DOWN(simulation_connection){
}

TEST(simulation_connection, simulation_init_should_compile){
    
}


