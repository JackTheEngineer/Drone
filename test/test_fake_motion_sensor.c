/*
 * test_fake_sensor.c
 *
 *  Created on: Apr 13, 2016
 *      Author: jakov
 */
#include "test_helper.h"
#include "motion_sensor.h"
#include <stdio.h>

TEST_GROUP(fake_motion_sensor);
Sensordata_t sensordata_container;
Sensordata_t* sensordata = &sensordata_container;

TEST_SETUP(fake_motion_sensor){
	motion_sensor_set_data_zero(sensordata);
}

TEST_TEAR_DOWN(fake_motion_sensor){
}

TEST(fake_motion_sensor, set_zero_should_set_all_values_to_zero){
}

TEST(fake_motion_sensor, should_write_some_values){

}
