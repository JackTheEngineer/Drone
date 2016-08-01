/*
 * test_fake_motors.h
 *
 *  Created on: Jul 30, 2016
 *      Author: chocolate
 */


#include "test_helper.h"
#include "fake_motors.h"

TEST_GROUP(fake_motors);
_STATIC_ void Set_all_values_zero(Motorcontrolvalues_t *motorvalues, Motorcurrents_t *currents);
_STATIC_ void Set_motorvalues_to_x(Motorcontrolvalues_t *motorvalues, double x);
_STATIC_ void Set_all_values_in_double_list(double *list, uint32_t length, double value_to_set);
_STATIC_ void calucalte_multipliers(Motorcontrolvalues_t *motorvalues, Motorcurrents_t *currents, double *multipliers);
_STATIC_ void Test_multipliers_equal_and_nonzero(double *multipliers1, double *multipliers2 );
_STATIC_ void Set_all_values_in_ui32_list(uint32_t *list, uint32_t length, double value_to_set);
TEST_SETUP(fake_motors){
}

TEST_TEAR_DOWN(fake_motors){
}

TEST(fake_motors, Calculate_currents_from_control_values_should_be_linear_functions){
	POINTER_TO_CONTAINER(Motorcontrolvalues_t, motorvalues);
	POINTER_TO_CONTAINER(Motorcurrents_t, currents);
	double multipliers1[NMBR_OF_MOTORS];
	double multipliers2[NMBR_OF_MOTORS];

	Set_all_values_zero(motorvalues, currents);
	Set_motorvalues_to_x(motorvalues, 100);
	fake_Motors_calculate_motorcurrent_of_control_value(motorvalues, currents);
	calucalte_multipliers(motorvalues, currents, multipliers1);

	Set_all_values_zero(motorvalues, currents);
	Set_motorvalues_to_x(motorvalues, 200);
	fake_Motors_calculate_motorcurrent_of_control_value(motorvalues, currents);
	calucalte_multipliers(motorvalues, currents, multipliers2);

	Test_multipliers_equal_and_nonzero(multipliers1, multipliers2);
}

_STATIC_ void Set_all_values_zero(Motorcontrolvalues_t *motorvalues, Motorcurrents_t *currents){
	Set_all_values_in_ui32_list(motorvalues->motorspeeds, NMBR_OF_MOTORS, 0);
	Set_all_values_in_double_list(currents->currents, NMBR_OF_MOTORS, 0);
}

_STATIC_ void Set_motorvalues_to_x(Motorcontrolvalues_t *motorvalues, double x){
	Set_all_values_in_ui32_list(motorvalues->motorspeeds, NMBR_OF_MOTORS, x);
}

_STATIC_ void Set_all_values_in_double_list(double *list, uint32_t length, double value_to_set){
	uint32_t i;

	for(i=0; i<length; i++){
		list[i] = value_to_set;
	}
}

_STATIC_ void Set_all_values_in_ui32_list(uint32_t *list, uint32_t length, double value_to_set){
	uint32_t i;

	for(i=0;i<length;i++){
		list[i] = value_to_set;
	}
}

_STATIC_ void calucalte_multipliers(Motorcontrolvalues_t *motorvalues, Motorcurrents_t *currents, double *multipliers){
	uint32_t i;

	for(i=0;i<NMBR_OF_MOTORS;i++){
		multipliers[i] = currents->currents[i]/(double)motorvalues->motorspeeds[i];
	}
}

_STATIC_ void Test_multipliers_equal_and_nonzero(double *multipliers1, double *multipliers2 ){
	uint32_t i;

	for(i=0; i<NMBR_OF_MOTORS; i++){
		TEST_ASSERT_EQUAL_DOUBLE(multipliers1[i], multipliers2[i]);
		TEST_ASSERT_TRUE(multipliers1[i] != 0.0);
	}
}
