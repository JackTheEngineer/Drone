/*
 * test_matrix_operations.c
 *
 *  Created on: May 13, 2016
 *      Author: jakov
 */

#include "test_helper.h"
#include "matrix_operations.h"
#include "matrix_tester.h"

Matrix_t matrix_container;
Matrix_t *matrix = &matrix_container;
Vector_t vect_container;
Vector_t *vect = &vect_container; 

TEST_GROUP(matrix_operations);

_STATIC_ void Mat_write_one_two_three_matrix(Matrix_t *matrix);

TEST_SETUP(matrix_operations){
	Vect_set_all_values_to(vect, 0.0);
	Mat_set_all_values_to(matrix, 0.0);
}

TEST_TEAR_DOWN(matrix_operations){
}

TEST(matrix_operations, Mat_write_should_use_plus_one_index){
	Mat_write(matrix, 0, 0,(_FLOAT_) 5.9);
	TEST_ASSERT_DOUBLE_WITHIN(0.000001, (_FLOAT_)5.9 , matrix->M[0][0]);
}

TEST(matrix_operations, Mat_write_should_use_plus_one_index_everywhere){
	uint8_t i;
	uint8_t j;

	for(i = 0; i < 3; i++){
		for(j = 0; j < 3; j++){
			Mat_write(matrix, i, j, (_FLOAT_)i+j);
			TEST_ASSERT_DOUBLE_WITHIN(0.000001,(_FLOAT_)i+j, matrix->M[i][j]);
		}
	}
}

TEST(matrix_operations, Mat_read_shoud_read_with_inceased_index){
	uint8_t i;
	uint8_t j;

	for(i = 0; i < 3; i++){
		for(j = 0; j < 3; j++){
			matrix->M[i][j] = j+i;
			TEST_ASSERT_DOUBLE_WITHIN(0.000001,j+i, Mat_read(matrix, i, j));
		}
	}
}

TEST(matrix_operations, Mat_read_should_return_zero_if_index_bigger_3){
	TEST_ASSERT_DOUBLE_WITHIN(0.000001,0.0, Mat_read(matrix, 4,4));
}

TEST(matrix_operations, Mat_Set_matrix_to_certain_value_should_work){
	uint8_t i;
	uint8_t j;
	for(i = 0; i < 3; i++){
		for(j = 0; j < 3; j++){
			matrix->M[i][j] = 1;
		}
	}
	Mat_set_all_values_to(matrix, 0.0);
	for(i = 0; i < 3; i++){
		for(j = 0; j < 3; j++){
			TEST_ASSERT_DOUBLE_WITHIN(0.000001,0.0, matrix->M[i][j]);
		}
	}
}

TEST(matrix_operations, set_diag_to_value_should_work){
	uint8_t i;
	uint8_t j;
	for(i = 0; i < 3; i++){
		for(j = 0; j < 3; j++){
			matrix->M[i][j] = 1;
		}
	}
	Mat_set_diag_to(matrix, 0.0);
	for(i = 0; i < 3; i++){
		TEST_ASSERT_DOUBLE_WITHIN(0.000001,0.0, matrix->M[i][i]);
	}
}

TEST(matrix_operations, multiply_with_const_should_work){
	Mat_set_all_values_to(matrix, 1);
	Mat_times_const(matrix, 5);
	uint8_t i;
	uint8_t j;
	for(i = 0; i < 3; i++){
		for(j = 0; j < 3; j++){
			TEST_ASSERT_DOUBLE_WITHIN(0.000001,5, Mat_read(matrix, i,j));
		}
	}
}

TEST(matrix_operations, Add_to_matrix_should_do_as_said){
	Mat_write(matrix, 0, 0, (_FLOAT_)5.2);
	Mat_add_to(matrix, 0, 0, (_FLOAT_)1.2);
	TEST_ASSERT_DOUBLE_WITHIN(0.000001, (_FLOAT_)6.4, Mat_read(matrix, 0, 0));
}

TEST(matrix_operations, Add_to_matrix_should_do_as_said2){
	Mat_write(matrix, 1, 1, (_FLOAT_) 5.2);
	Mat_add_to(matrix, 1, 1, (_FLOAT_) 1.2);
	TEST_ASSERT_DOUBLE_WITHIN(0.000001, (_FLOAT_) 6.4, Mat_read(matrix, 1, 1));
}

TEST(matrix_operations, Mat_times_vector_should_use_mathematical_rule){
	POINTER_TO_CONTAINER(Vector_t, resultvect);

	Mat_set_all_values_to(matrix, 0.0);
	Mat_write_one_two_three_matrix(matrix);

	Vect_write_three_values(vect, 1, 3, 5);

	Mat_times_vect(matrix, vect, resultvect);
	
	TEST_ASSERT_DOUBLE_WITHIN(0.000001, 22, Vect_read(resultvect, 0));
	TEST_ASSERT_DOUBLE_WITHIN(0.000001, 49, Vect_read(resultvect, 1));
	TEST_ASSERT_DOUBLE_WITHIN(0.000001, 76, Vect_read(resultvect, 2));
}


IGNORE_TEST(matrix_operations, Inverse_of_diagonal_matrix_should_be_one_over_diag_value){
  POINTER_TO_CONTAINER(Matrix_t, inverse_matrix);
  POINTER_TO_CONTAINER(Matrix_t, compare_matrix);
	Mat_set_all_values_to(inverse_matrix, 0.0);

	Mat_set_all_values_to(matrix, 0.0);
	Mat_set_diag_to(matrix, 5.0);
	//print_matrix(matrix," Matrix ");

	Mat_set_all_values_to(compare_matrix, 0.0);
	Mat_set_diag_to(compare_matrix, 1.0/5.0);
	//print_matrix(compare_matrix, " compare matrix ");

	Mat_inverse(matrix, inverse_matrix);

	//print_matrix(inverse_matrix, "inverse matrix:");
	Test_Mat_equal(inverse_matrix, compare_matrix);
}

IGNORE_TEST(matrix_operations, Matrix_inverse_should_give_correct_results_with_zeroes_on_diagonal){
  POINTER_TO_CONTAINER(Matrix_t, inverse_matrix);
  POINTER_TO_CONTAINER(Matrix_t, compare_matrix);

	/*writing a matrix like:
	 * 	( 0 , 2 , 0 )
	 * 	( 0 , 0 , 2 )
	 * 	( 2 , 0 , 0 )
	 */
	Mat_set_all_values_to(matrix, 0.0);
	Mat_write(matrix, 0,1, 2.0);
	Mat_write(matrix, 1,2, 2.0);
	Mat_write(matrix, 2,0, 2.0);

	/*the inverse matrix to the other one is like this:
	 *  ( 0 , 0 ,1/2)
	 *  (1/2, 0 , 0 )
	 *  ( 0 ,1/2, 0 )
	 */
	Mat_set_all_values_to(compare_matrix, 0.0);
	Mat_write(compare_matrix, 0,2, 0.5);
	Mat_write(compare_matrix, 1,0, 0.5);
	Mat_write(compare_matrix, 2,1, 0.5);

	Mat_inverse(matrix, inverse_matrix);

	Test_Mat_equal(compare_matrix, inverse_matrix);
}

TEST(matrix_operations, Matrix_copy_should_work){
	Matrix_t copy_to_matrix_container;
	Matrix_t *copy_to_matrix = &copy_to_matrix_container;

	Mat_set_all_values_to(copy_to_matrix, 0.0);
	Mat_write_one_two_three_matrix(matrix);
	Mat_copy(matrix, copy_to_matrix);

	Test_Mat_equal(matrix, copy_to_matrix);
}

TEST(matrix_operations, Mat_times_mat_on_example_matrices_should_work){
	POINTER_TO_CONTAINER(Matrix_t, compare_matrix);
	Mat_set_all_values_to(compare_matrix, 0.0);
	Mat_set_all_values_to(matrix, 0.0);

	Mat_write_one_two_three_matrix(matrix);

	Mat_write(compare_matrix, 0,0, 30.0);
	Mat_write(compare_matrix, 0,1, 36.0);
	Mat_write(compare_matrix, 0,2, 42.0);
	Mat_write(compare_matrix, 1,0, 66.0);
	Mat_write(compare_matrix, 1,1, 81.0);
	Mat_write(compare_matrix, 1,2, 96.0);
	Mat_write(compare_matrix, 2,0, 102.0);
	Mat_write(compare_matrix, 2,1, 126.0);
	Mat_write(compare_matrix, 2,2, 150.0);

	/** result is also being saved into 'matrix' */
	Mat_times_mat(matrix, matrix, matrix);

	Test_Mat_equal(matrix, compare_matrix);
}

TEST(matrix_operations, Mat_times_mat_should_work_in_correct_order){
	POINTER_TO_CONTAINER(Matrix_t, M);
	POINTER_TO_CONTAINER(Matrix_t, compare_matrix);
	Mat_set_all_values_to(compare_matrix, 0.0);
	Mat_set_all_values_to(M, 0.0);
	Mat_set_all_values_to(matrix, 0.0);
	
	Mat_write(matrix, 0,0, 1.0);
	Mat_write(matrix, 0,1, 0.0);
	Mat_write(matrix, 0,2, 0.0);
	Mat_write(matrix, 1,0, 1.0);
	Mat_write(matrix, 1,1, 0.0);
	Mat_write(matrix, 1,2, 1.0);
	Mat_write(matrix, 2,0, 0.0);
	Mat_write(matrix, 2,1, 1.0);
	Mat_write(matrix, 2,2, 0.0);

	Mat_write(M, 0,0, 0.0);
	Mat_write(M, 0,1, 1.0);
	Mat_write(M, 0,2, 1.0);
	Mat_write(M, 1,0, 1.0);
	Mat_write(M, 1,1, 0.0);
	Mat_write(M, 1,2, 0.0);
	Mat_write(M, 2,0, 1.0);
	Mat_write(M, 2,1, 0.0);
	Mat_write(M, 2,2, 1.0);

	Mat_write(compare_matrix, 0,0, 0.0);
	Mat_write(compare_matrix, 0,1, 1.0);
	Mat_write(compare_matrix, 0,2, 1.0);
	Mat_write(compare_matrix, 1,0, 1.0);
	Mat_write(compare_matrix, 1,1, 1.0);
	Mat_write(compare_matrix, 1,2, 2.0);
	Mat_write(compare_matrix, 2,0, 1.0);
	Mat_write(compare_matrix, 2,1, 0.0);
	Mat_write(compare_matrix, 2,2, 0.0);

	Mat_times_mat(matrix, M, matrix);
	Test_Mat_equal(matrix,compare_matrix);
}

TEST(matrix_operations, Transpose_should_work){
	POINTER_TO_CONTAINER(Matrix_t, compare_matrix)
	Mat_write_one_two_three_matrix(matrix);

	Mat_write(compare_matrix, 0,0, 1.0);
	Mat_write(compare_matrix, 0,1, 4.0);
	Mat_write(compare_matrix, 0,2, 7.0);
	Mat_write(compare_matrix, 1,0, 2.0);
	Mat_write(compare_matrix, 1,1, 5.0);
	Mat_write(compare_matrix, 1,2, 8.0);
	Mat_write(compare_matrix, 2,0, 3.0);
	Mat_write(compare_matrix, 2,1, 6.0);
	Mat_write(compare_matrix, 2,2, 9.0);

	Mat_transpose(matrix, matrix);

	Test_Mat_equal(matrix, compare_matrix);
}

_STATIC_ void Mat_write_one_two_three_matrix(Matrix_t *matrix){
	uint8_t i;
	uint8_t j;
	uint8_t k=1;

	for(i = 0; i < 3; i++){
		for(j = 0; j < 3; j++){
			Mat_write(matrix, i, j, k);
			k++;
		}
	}
}
