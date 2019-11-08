/*
 * test_drone_simulation.c
 *
 *  Created on: Apr 23, 2016
 *      Author: jakov
 */
#include "test_helper.h"
#include "vector_tester.h"
#include "vector_operations.h"
#include "drone_physics.h"
#include "propeller.h"
#include "matrix_tester.h"
#include "matrix_operations.h"

_STATIC_ void Test_Drone_data_being_zero(Physical_Drone_t *dronedata);
_STATIC_ void Test_rotorspeeds_zero(Vector_t rotorspeeds[NMBR_OF_MOTORS]);
_STATIC_ void _assign_angles_values(Vector_t *angles);

TEST_GROUP(drone_simulation);
POINTER_TO_CONTAINER(Physical_Drone_t, drone);

TEST_SETUP(drone_simulation){
	Drone_set_drone_data_zero(drone);
}

TEST_TEAR_DOWN(drone_simulation){}


IGNORE_TEST(drone_simulation, set_zero_should_work){
	Vect_set_all_values_to(&(drone->angular_position), 5.0);
	Vect_set_all_values_to(&(drone->angular_speed), 5.0);
	Vect_set_all_values_to(&(drone->position), 5.0);
	Vect_set_all_values_to(&(drone->speed), 5.0);

	Drone_set_drone_data_zero(drone);

	Test_Drone_data_being_zero(drone);
}

_STATIC_ void Test_Drone_data_being_zero(Physical_Drone_t *drone){
	POINTER_TO_CONTAINER(Vector_t, zero_vector);
	Vect_set_all_values_to(zero_vector, 0.0);

	Test_vectors_equal(zero_vector, &(drone->angular_position));
	Test_vectors_equal(zero_vector, &(drone->angular_speed));
	Test_vectors_equal(zero_vector, &(drone->position));
	Test_vectors_equal(zero_vector, &(drone->speed));
}

IGNORE_TEST(drone_simulation, Rotation_matrix_inverse_and_transpose_should_be_the_same){
	POINTER_TO_CONTAINER(Vector_t, angles);
	POINTER_TO_CONTAINER(Matrix_t, R_zyx);
	POINTER_TO_CONTAINER(Matrix_t, inverse);
	POINTER_TO_CONTAINER(Matrix_t, transpose);

	_assign_angles_values(angles);

	Generate_Rotation_Matrix(R_zyx, angles);
	Mat_inverse(R_zyx, inverse);
	Mat_transpose(R_zyx, transpose);

	Test_Mat_equal(inverse, transpose);
}

IGNORE_TEST(drone_simulation, Rotation_from_earth_frame_to_drone_frame_and_back_again_should_give_the_same_vector){
	POINTER_TO_CONTAINER(Vector_t, angles);
	POINTER_TO_CONTAINER(Vector_t, rotated_vector);
	POINTER_TO_CONTAINER(Vector_t, compare_vector);

	Vect_write(rotated_vector, 0, 1.557);
	Vect_write(rotated_vector, 1, 5.2347);
	Vect_write(rotated_vector, 2, 4.3442);
	Vect_copy_from_to(rotated_vector, compare_vector);

	_assign_angles_values(angles);

	Rotate_from_drone_frame_to_earth_frame(rotated_vector, angles);
	Rotate_from_earth_frame_to_drone_frame(rotated_vector, angles);
	
	Test_vectors_equal(rotated_vector, compare_vector);
}


_STATIC_ void _assign_angles_values(Vector_t *angles){
	Vect_write(angles, 0, 1.234);
	Vect_write(angles, 1, 0.9434);
	Vect_write(angles, 2, 0.8884);
}
