/*
 * test_math_helpers.c
 *
 *  Created on: May 14, 2016
 *      Author: jakov
 */

#include "test_helper.h"
#include "math_helpers.h"


TEST_GROUP(math_helpers);
Vector_t vect;

TEST_SETUP(math_helpers){
}

TEST_TEAR_DOWN(math_helpers){
}

TEST(math_helpers, kron_delta_should_return_zero_when_indizes_are_not_equal){
	TEST_ASSERT_EQUAL(0, kron_delta(1,0));
}

TEST(math_helpers, kron_delta_should_return_one_when_indizes_are_equal){
	TEST_ASSERT_EQUAL(1, kron_delta(10,10));
}

TEST(math_helpers, vect_val_from_ind_returns_x_value_on_ind_1){
	vect.x = 55;
	TEST_ASSERT_EQUAL(55, vect_val_of_ind(&vect, 1));
}

TEST(math_helpers, vect_val_from_ind_returns_y_value_on_ind_2){
	vect.y = 77;
	TEST_ASSERT_EQUAL(77, vect_val_of_ind(&vect, 2));
}

TEST(math_helpers, vect_val_from_ind_returns_z_value_on_ind_3){
	vect.z = 99;
	TEST_ASSERT_EQUAL(99, vect_val_of_ind(&vect, 3));
}

TEST(math_helpers, vect_val_from_ind_returns_zero_if_index_zero){
	TEST_ASSERT_EQUAL(0, vect_val_of_ind(&vect, 0));
}

TEST(math_helpers, vect_val_from_ind_returns_zero_if_index_bigger3){
	TEST_ASSERT_EQUAL(0, vect_val_of_ind(&vect, 4));
}
