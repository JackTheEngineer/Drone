/*
 * test_motion_sensor.c
 *
 *  Created on: Apr 13, 2016
 *      Author: jakov
 */
#include "test_helper.h"
#include "motion_sensor.h"

TEST_GROUP(fake_motion_sensor);
Sensordata_t sensordata_container;
Sensordata_t* sensordata = &sensordata_container;

TEST_SETUP(fake_motion_sensor){
}

TEST_TEAR_DOWN(fake_motion_sensor){
}

TEST(fake_motion_sensor, set_zero_should_set_all_values_to_zero){
	sensordata->accel_data.x = 101;
	sensordata->accel_data.y = 101;
	sensordata->accel_data.z = 110;
	sensordata->gyro_data.x = 10;
	sensordata->gyro_data.y = 1101;
	sensordata->gyro_data.z = 101;
	sensordata->compass_data.x = 101;
	sensordata->compass_data.y = 101;
	sensordata->compass_data.z = 1101;

	motion_sensor_set_data_zero(sensordata);

	TEST_ASSERT_EQUAL_INT32(0, sensordata->accel_data.x);
	TEST_ASSERT_EQUAL_INT32(0, sensordata->accel_data.y);
	TEST_ASSERT_EQUAL_INT32(0, sensordata->accel_data.z);
	TEST_ASSERT_EQUAL_INT32(0, sensordata->gyro_data.x);
	TEST_ASSERT_EQUAL_INT32(0, sensordata->gyro_data.z);
	TEST_ASSERT_EQUAL_INT32(0, sensordata->gyro_data.x);
	TEST_ASSERT_EQUAL_INT32(0, sensordata->compass_data.x);
	TEST_ASSERT_EQUAL_INT32(0, sensordata->compass_data.y);
	TEST_ASSERT_EQUAL_INT32(0, sensordata->compass_data.z);
}

TEST(fake_motion_sensor, should_write_some_values){
	TEST_IGNORE();
}


