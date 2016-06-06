/*
 * test_propeller.c
 *
 *  Created on: May 19, 2016
 *      Author: jakov
 */

#include "test_helper.h"
#include "propeller.h"

TEST_GROUP(propeller);

TEST_SETUP(propeller){
}

TEST_TEAR_DOWN(propeller){
}

TEST(propeller, propeller_get_force_from_rpm_and_get_rpm_from_force_should_reverse_functions){
    TEST_ASSERT_EQUAL_DOUBLE(345.5, propeller_rpm_of_force(propeller_force_of_rpm(345.5)));
}

TEST(propeller, propeller_get_force_from_rpm_should_be_zero_with_rpm_zero){
    TEST_ASSERT_EQUAL_DOUBLE(0.0, propeller_force_of_rpm(0));
}

TEST(propeller, propeller_get_rpm_of_force_should_be_zero_with_force_zero){
	TEST_ASSERT_EQUAL_DOUBLE(0.0, propeller_rpm_of_force(0));
}
