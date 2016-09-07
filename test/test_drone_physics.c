/*
 * test_drone_simulation.c
 *
 *  Created on: Apr 23, 2016
 *      Author: jakov
 */
#include "test_helper.h"
#include "vector_tester.h"
#include "vector_operations.h"
#include "drone_physics.h"
#include "propeller.h"

_STATIC_ void Test_Drone_data_being_zero(Physical_Drone_t *dronedata);
_STATIC_ void Test_rotorspeeds_zero(Vector_t rotorspeeds[NMBR_OF_MOTORS]);

TEST_GROUP(drone_simulation);
POINTER_TO_CONTAINER(Physical_Drone_t, drone);

TEST_SETUP(drone_simulation){
	Drone_set_drone_data_zero(drone);
}

TEST_TEAR_DOWN(drone_simulation){
}

TEST(drone_simulation, set_zero_should_work){
	Vect_set_all_values_to(&(drone->angular_position), 5.0);
	Vect_set_all_values_to(&(drone->angular_speed), 5.0);
	Vect_set_all_values_to(&(drone->position), 5.0);
	Vect_set_all_values_to(&(drone->speed), 5.0);

	Drone_set_drone_data_zero(drone);

	Test_Drone_data_being_zero(drone);
}

_STATIC_ void Test_Drone_data_being_zero(Physical_Drone_t *drone){
	POINTER_TO_CONTAINER(Vector_t, zero_vector);
	Vect_set_all_values_to(zero_vector, 0.0);

	Test_vectors_equal(zero_vector, &(drone->angular_position));
	Test_vectors_equal(zero_vector, &(drone->angular_speed));
	Test_vectors_equal(zero_vector, &(drone->position));
	Test_vectors_equal(zero_vector, &(drone->speed));
}

