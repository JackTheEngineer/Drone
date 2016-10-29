/*
 * test_fake_sensor.c
 *
 *  Created on: Apr 13, 2016
 *      Author: jakov
 */
#include "test_helper.h"
#include "motion_sensor.h"
#include "dronedata_def.h"
#include "drone_physics.h"
#include "vector_tester.h"
#include "fake_motion_sensor.h"

TEST_GROUP(fake_motion_sensor);
POINTER_TO_CONTAINER(Sensordata_t, sensordata);

TEST_SETUP(fake_motion_sensor){
	Physical_Drone_t *drone = Drone_get_dronedata();
	Motion_sensor_set_data_zero(sensordata);
	Drone_set_drone_data_zero(drone);
}

TEST_TEAR_DOWN(fake_motion_sensor){
}

TEST(fake_motion_sensor, with_afs_sel_zero_limit_should_be_2){
	POINTER_TO_CONTAINER(Vector_i32_t, compare_vector);
	POINTER_TO_CONTAINER(Sensordata_t, sensor);
	Physical_Drone_t *drone = Drone_get_dronedata();
	sensor->settings.afs_sel = 0;
	
	Vect_write_three_values(&drone->acceleration, 2.1, 2.1, 2.1);
	Vect_i32_set_all_values_to(compare_vector, (1<<(SENSOR_ADC_RES-1)));
	Motion_sensor_get_data(sensor);
		
	Test_vectors_i32_equal(compare_vector, &sensor->acceleration);
}

TEST(fake_motion_sensor, with_afs_sel_1_limit_should_be_4){
	POINTER_TO_CONTAINER(Vector_i32_t, compare_vector);
	POINTER_TO_CONTAINER(Sensordata_t, sensor);
	Physical_Drone_t *drone = Drone_get_dronedata();
	sensor->settings.afs_sel = 1;
	
	Vect_write_three_values(&drone->acceleration, 4.1, 4.1, 4.1);
	Vect_i32_set_all_values_to(compare_vector, (1<<(SENSOR_ADC_RES-1)));
	Motion_sensor_get_data(sensor);
		
	Test_vectors_i32_equal(compare_vector, &sensor->acceleration);
}

