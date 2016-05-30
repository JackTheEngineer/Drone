/*
 * test_drone_simulation.c
 *
 *  Created on: Apr 23, 2016
 *      Author: jakov
 */
#include "test_helper.h"
#include "drone_simulation.h"

TEST_GROUP(drone_simulation);
Drone_Data_t dronedata_container;
Drone_Data_t *dronedata = &dronedata_container;
Rotor_Speeds_t rotorspeeds_container;
Rotor_Speeds_t *rotorspeeds = &rotorspeeds_container;

TEST_SETUP(drone_simulation){
}

TEST_TEAR_DOWN(drone_simulation){
}

TEST(drone_simulation, set_zero_should_work){
	dronedata->position_e.x = 12.0;
	dronedata->position_e.y = 12.0;
	dronedata->position_e.z = 12.0;
	dronedata->angular_position.x = 12.0;
	dronedata->angular_position.y = 12.0;
	dronedata->angular_position.z = 12.0;
	dronedata->angular_speed.x = 12.0;
	dronedata->angular_speed.y = 12.0;
	dronedata->angular_speed.z = 12.0;
	dronedata->speed.x = 12.0;
	dronedata->speed.y = 12.0;
	dronedata->speed.z = 12.0;

	drone_set_drone_data_zero(dronedata);

	TEST_ASSERT_EQUAL_DOUBLE(0, dronedata->position_e.x);
	TEST_ASSERT_EQUAL_DOUBLE(0, dronedata->position_e.y);
	TEST_ASSERT_EQUAL_DOUBLE(0, dronedata->position_e.z);
	TEST_ASSERT_EQUAL_DOUBLE(0, dronedata->angular_position.x);
	TEST_ASSERT_EQUAL_DOUBLE(0, dronedata->angular_position.y);
	TEST_ASSERT_EQUAL_DOUBLE(0, dronedata->angular_position.z);
	TEST_ASSERT_EQUAL_DOUBLE(0, dronedata->angular_speed.x);
	TEST_ASSERT_EQUAL_DOUBLE(0, dronedata->angular_speed.y);
	TEST_ASSERT_EQUAL_DOUBLE(0, dronedata->angular_speed.z);
	TEST_ASSERT_EQUAL_DOUBLE(0, dronedata->speed.x);
	TEST_ASSERT_EQUAL_DOUBLE(0, dronedata->speed.y);
	TEST_ASSERT_EQUAL_DOUBLE(0, dronedata->speed.z);
}

TEST(drone_simulation, with_no_forces_g_iteration_is_similar_to_formula){
	double time = 2.0;
	uint32_t number_of_calls = (int)(time/TIMESTEP);
	uint32_t i;

	drone_set_drone_data_zero(dronedata);
	dronedata->position_e.z = 100.0;

	for(i = 0; i < number_of_calls; i++){
		// The First test implementation was with a timestep of 0.0001 s
		// This means that the timestep declaration could be a reason of failure
		drone_calculate_next_values(dronedata, rotorspeeds, TIMESTEP);
	}
	double formula_value = (0.5)*GRAVITY_CONST*SQR(time) + 100.0;
	printf("formula_value: %f \n", formula_value);
	printf("iterated value: %f \n", dronedata->position_e.z);
	TEST_ASSERT_DOUBLE_WITHIN(0.01, formula_value , dronedata->position_e.z);
}

TEST(drone_simulation, with_motorforces_equal_to_g_should_stand_still){

}
