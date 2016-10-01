/*
 * test_fake_sensor.c
 *
 *  Created on: Apr 13, 2016
 *      Author: jakov
 */
#include "test_helper.h"
#include "motion_sensor.h"

TEST_GROUP(fake_motion_sensor);
POINTER_TO_CONTAINER(Sensordata_t, sensordata);

TEST_SETUP(fake_motion_sensor){
	Motion_sensor_set_data_zero(sensordata);
}

TEST_TEAR_DOWN(fake_motion_sensor){
}

TEST(fake_motion_sensor, set_zero_should_set_all_values_to_zero){
	
}
