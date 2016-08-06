/*
 * test_fake_motors.h
 *
 *  Created on: Jul 30, 2016
 *      Author: chocolate
 */


#include "test_helper.h"
#include "fake_motors.h"

typedef double *(*Motorelement_selector_f)(Motor_t *motor);

_STATIC_ void Set_all_values_zero(Motorcontrolvalues_t *motorvalues, Motor_t motors[NMBR_OF_MOTORS]);
_STATIC_ void Set_motorvalues_to_x(Motorcontrolvalues_t *motorvalues, double x);
_STATIC_ void calucalte_multipliers(Motorcontrolvalues_t *motorvalues, Motor_t motors[NMBR_OF_MOTORS], double *multipliers);
_STATIC_ void calculate_quadratic_multipliers(double *multipliers, Motor_t motors[NMBR_OF_MOTORS]);

_STATIC_ void Test_multipliers_equal_and_nonzero(double *multipliers1, double *multipliers2 );
_STATIC_ void Set_all_values_in_ui32_list(uint32_t *list, uint32_t length, double value_to_set);
_STATIC_ void Test_selected_values_in_Motors_list_equal_value(Motor_t motors[NMBR_OF_MOTORS], Motorelement_selector_f selector, double value);
_STATIC_ void Set_all_selected_values_in_Motors_list(Motor_t motors[NMBR_OF_MOTORS], Motorelement_selector_f selector, double value);
_STATIC_ void Test_selected_values_in_Motors_list_nonzero(Motor_t motors[NMBR_OF_MOTORS], Motorelement_selector_f selector);

_STATIC_ double *select_speed_z_axis(Motor_t *motor);
_STATIC_ double *select_current(Motor_t *motor);
_STATIC_ double *select_thrusts_z_axis(Motor_t *motor);


TEST_GROUP(fake_motors);
Motor_t motors[NMBR_OF_MOTORS];

TEST_SETUP(fake_motors){
}

TEST_TEAR_DOWN(fake_motors){
}

TEST(fake_motors, Calculate_currents_from_control_values_should_be_linear_functions){
	uint8_t i;
	POINTER_TO_CONTAINER(Motorcontrolvalues_t, motorvalues);
	double multipliers[2][NMBR_OF_MOTORS];

	for(i=0; i<2; i++){
		Set_all_values_zero(motorvalues, motors);
		Set_motorvalues_to_x(motorvalues, (i+1)*100); 
		fake_Motor_calculate_currents_from_controlvalues(motors, motorvalues);
		calucalte_multipliers(motorvalues, motors, multipliers[i]);
	}

	Test_multipliers_equal_and_nonzero(multipliers[0], multipliers[1]);
}

TEST(fake_motors, caluclate_speeds_from_currents_is_zero_with_input_zero){
	Set_all_selected_values_in_Motors_list(motors, select_current, 0);
	Set_all_selected_values_in_Motors_list(motors, select_speed_z_axis, 1.0);
	fake_Motor_calculate_speeds_from_currents(motors);
	Test_selected_values_in_Motors_list_equal_value(motors,select_speed_z_axis, 0.0);
}

TEST(fake_motors, calculate_speeds_from_currents_should_give_a_nonzero_value_with_nonzero_current){
	Set_all_selected_values_in_Motors_list(motors, select_current, 10);
        Set_all_selected_values_in_Motors_list(motors, select_speed_z_axis, 0.0);
	fake_Motor_calculate_speeds_from_currents(motors);
	Test_selected_values_in_Motors_list_nonzero(motors, select_speed_z_axis);
}

TEST(fake_motors, caluclate_thursts_from_speeds_should_be_quadratic){
    uint8_t i;
    double multipliers[3][NMBR_OF_MOTORS];
    
    for(i=0; i<3; i++){
        Set_all_selected_values_in_Motors_list(motors, select_speed_z_axis, ((i+1)*10));
        Set_all_selected_values_in_Motors_list(motors, select_thrusts_z_axis, 0);
        fake_Motor_calculate_thrust_from_speed(motors);
        calculate_quadratic_multipliers(multipliers[i], motors);
    }

    for(i=0; i<2; i++){
        Test_multipliers_equal_and_nonzero(multipliers[i], multipliers[i+1]);
    }
}

_STATIC_ void Set_all_values_zero(Motorcontrolvalues_t *motorvalues, Motor_t motors[NMBR_OF_MOTORS]){
	Set_all_values_in_ui32_list(motorvalues->motorspeeds, NMBR_OF_MOTORS, 0);
        Set_all_selected_values_in_Motors_list(motors, select_current, 0);
}

_STATIC_ void Set_motorvalues_to_x(Motorcontrolvalues_t *motorvalues, double x){
	Set_all_values_in_ui32_list(motorvalues->motorspeeds, NMBR_OF_MOTORS, x);
}

_STATIC_ void Set_all_values_in_ui32_list(uint32_t *list, uint32_t length, double value_to_set){
	uint32_t i;

	for(i=0;i<length;i++){
		list[i] = value_to_set;
	}
}

_STATIC_ void calucalte_multipliers(Motorcontrolvalues_t *motorvalues, Motor_t motors[NMBR_OF_MOTORS], double *multipliers){
	uint32_t i;

	for(i=0;i<NMBR_OF_MOTORS;i++){
		multipliers[i] = motors[i].current/(double)motorvalues->motorspeeds[i];
	}
}

_STATIC_ void calculate_quadratic_multipliers(double *multipliers, Motor_t motors[NMBR_OF_MOTORS]){
    uint32_t i;
    double speed;

    for(i=0; i<NMBR_OF_MOTORS; i++){
        speed = Vect_read(&(motors[i].speed), 3);
        multipliers[i] = Vect_read(&(motors[i].thrust), 3)/SQR(speed);
    }
}

_STATIC_ void Test_multipliers_equal_and_nonzero(double *multipliers1, double *multipliers2 ){
	uint32_t i;

	for(i=0; i<NMBR_OF_MOTORS; i++){
		TEST_ASSERT_EQUAL_DOUBLE(multipliers1[i], multipliers2[i]);
		TEST_ASSERT_TRUE(multipliers1[i] != 0.0);
	}
}

_STATIC_ void Test_selected_values_in_Motors_list_equal_value(Motor_t motors[NMBR_OF_MOTORS], Motorelement_selector_f selector, double value){
	uint32_t i;

	for(i=0;i<NMBR_OF_MOTORS;i++){
		TEST_ASSERT_EQUAL_DOUBLE(value, *(selector(&(motors[i]))));
	}
}

_STATIC_ void Test_selected_values_in_Motors_list_nonzero(Motor_t motors[NMBR_OF_MOTORS], Motorelement_selector_f selector){
	uint32_t i;

	for(i=0;i<NMBR_OF_MOTORS;i++){
		TEST_ASSERT_FALSE(*(selector(&(motors[i]))) == 0);
	}
}

_STATIC_ double *select_speed_z_axis(Motor_t *motor){
	return Vect_pointer_to_index(&(motor->speed), 3);
}

_STATIC_ double *select_current(Motor_t *motor){
	return &(motor->current);
}

_STATIC_ double *select_thrusts_z_axis(Motor_t *motor){
    return Vect_pointer_to_index(&(motor->thrust),3);
}

_STATIC_ void Set_all_selected_values_in_Motors_list(Motor_t motors[NMBR_OF_MOTORS], Motorelement_selector_f selector, double value){
    uint32_t i;
    
    for(i=0; i<NMBR_OF_MOTORS; i++){
        *(selector(&(motors[i]))) = value;
    }
}

