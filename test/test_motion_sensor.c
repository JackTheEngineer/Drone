/*
 * test_motion_sensor.c
 *
 *  Created on: Apr 13, 2016
 *      Author: jakov
 */
#include "test_helper.h"
#include "motion_sensor.h"
#include "vector_tester.h"

TEST_GROUP(motion_sensor);
Sensordata_t sensordata_container;
Sensordata_t* sensordata = &sensordata_container;

_STATIC_ void Test_Sensordata_zero(Sensordata_t *sensordata);

TEST_SETUP(motion_sensor){
}

TEST_TEAR_DOWN(motion_sensor){
}

TEST(motion_sensor, set_zero_should_set_all_values_to_zero){

	Vect_set_all_values_to(&(sensordata->acceleration), 5);
	Vect_set_all_values_to(&(sensordata->angle_speed), 5);
	Vect_set_all_values_to(&(sensordata->magnetic_field), 5);

	Motion_sensor_set_data_zero(sensordata);

	Test_Sensordata_zero(sensordata);
}



_STATIC_ void Test_Sensordata_zero(Sensordata_t *sensordata){
	Vector_t zero_vector_container;
	Vector_t *zero_vector = &(zero_vector_container);

	Vect_set_all_values_to(zero_vector, 0.0);

	Test_vectors_equal(zero_vector, &(sensordata->acceleration));
	Test_vectors_equal(zero_vector, &(sensordata->angle_speed));
	Test_vectors_equal(zero_vector, &(sensordata->magnetic_field));

}

