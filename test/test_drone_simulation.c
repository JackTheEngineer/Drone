/*
 * test_drone_simulation.c
 *
 *  Created on: Apr 23, 2016
 *      Author: jakov
 */
#include "test_helper.h"
#include "drone_simulation.h"
#include "propeller.h"

// Simulation Time Step im s
#define TIMESTEP (double)0.0001 // 100 us
#define SIMULATIONTIME (double)2.0
#define ITERATIONS (int)(SIMULATIONTIME/TIMESTEP)


TEST_GROUP(drone_simulation);
Physical_Drone_Data_t dronedata_container;
Physical_Drone_Data_t *dronedata = &dronedata_container;
Rotor_Moments_t rotorspeeds_container;
Rotor_Moments_t *rotorspeeds = &rotorspeeds_container;

_STATIC_ void set_all_motor_rpms_to(double motor_rpm, Rotor_Moments_t* rotorspeeds);

TEST_SETUP(drone_simulation){
}

TEST_TEAR_DOWN(drone_simulation){
}

TEST(drone_simulation, set_zero_should_work){
	dronedata->position.x = 12.0;
	dronedata->position.y = 12.0;
	dronedata->position.z = 12.0;
	dronedata->angular_position.x = 12.0;
	dronedata->angular_position.y = 12.0;
	dronedata->angular_position.z = 12.0;
	dronedata->angular_speed.x = 12.0;
	dronedata->angular_speed.y = 12.0;
	dronedata->angular_speed.z = 12.0;
	dronedata->speed.x = 12.0;
	dronedata->speed.y = 12.0;
	dronedata->speed.z = 12.0;

	Drone_set_drone_data_zero(dronedata);

	TEST_ASSERT_EQUAL_DOUBLE(0, dronedata->position.x);
	TEST_ASSERT_EQUAL_DOUBLE(0, dronedata->position.y);
	TEST_ASSERT_EQUAL_DOUBLE(0, dronedata->position.z);
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
	uint32_t i;

	Drone_set_drone_data_zero(dronedata);
    Drone_set_position(0,0,100, dronedata);

	for(i = 0; i < ITERATIONS; i++){
		// The First test implementation was with a timestep of 0.0001 s
		// This means that the timestep declaration could be a reason of failure
		Drone_calculate_next_values(dronedata, rotorspeeds, TIMESTEP);
	}
	double formula_value = (0.5)*GRAVITY_CONST*SQR(SIMULATIONTIME) + 100.0;
	printf("formula_value: %f \n", formula_value);
	printf("iterated value: %f \n", dronedata->position.z);
	TEST_ASSERT_DOUBLE_WITHIN(0.01, formula_value , dronedata->position.z);
}

TEST(drone_simulation, with_motorforces_equal_to_g_should_stand_still){
    uint32_t i; 

    Drone_set_drone_data_zero(dronedata);
    Drone_set_position(0, 0, 100, dronedata);

    set_all_motor_rpms_to(
        propeller_rpm_of_force(-GRAVITY_CONST/4), 
        rotorspeeds);
    
    for(i =0; i < ITERATIONS; i++){
        Drone_calculate_next_values(dronedata, rotorspeeds, TIMESTEP);
        TEST_ASSERT_DOUBLE_WITHIN(0.01 ,100.0, dronedata->position.z);
    }
}

TEST(drone_simulation, with_symmetric_motorforces_equal_to_g_should_stand_still){
    uint32_t i; 

    Drone_set_drone_data_zero(dronedata);
    Drone_set_position(0, 0, 100, dronedata);
    
    rotorspeeds->moments[0] = propeller_rpm_of_force(-GRAVITY_CONST*2/5);
    rotorspeeds->moments[1] = propeller_rpm_of_force(-GRAVITY_CONST*2/5);
    rotorspeeds->moments[2] = propeller_rpm_of_force(-GRAVITY_CONST*1/10);
    rotorspeeds->moments[3] = propeller_rpm_of_force(-GRAVITY_CONST*1/10);

    for(i = 0; i < ITERATIONS; i++){
        Drone_calculate_next_values(dronedata, rotorspeeds, TIMESTEP);
        TEST_ASSERT_DOUBLE_WITHIN(0.01, 100.0, dronedata->position.z);
    }
}

_STATIC_ void set_all_motor_rpms_to(double motor_rpm, Rotor_Moments_t* rotorspeeds){
	uint8_t i;
	for(i=0; i<=3; i++){
		rotorspeeds->moments[i] =  motor_rpm;
	}
}
