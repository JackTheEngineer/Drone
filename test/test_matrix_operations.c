/*
 * test_matrix_operations.c
 *
 *  Created on: May 13, 2016
 *      Author: jakov
 */

#include "test_helper.h"
#include "matrix_operations.h"



three_by_three_t matrix_container;
three_by_three_t *matrix = &matrix_container;
Vector_t vect_container;
Vector_t *vect = &vect_container; 

TEST_GROUP(matrix_operations);

_STATIC_ void Mat_write_arbitrary_values_into(three_by_three_t *matrix);
_STATIC_ void Test_Mat_equal(three_by_three_t *matrix1, three_by_three_t *matrix2);
_STATIC_ void print_matrix(three_by_three_t *M, const char *name);

TEST_SETUP(matrix_operations){
	Vect_set_all_values_to(vect, 0.0);
}

TEST_TEAR_DOWN(matrix_operations){
}

TEST(matrix_operations, Mat_write_should_use_plus_one_index){
	Mat_write(matrix, 1, 1, 5.9);
	TEST_ASSERT_EQUAL_DOUBLE(5.9, matrix->M[0][0]);
}

TEST(matrix_operations, Mat_write_should_use_plus_one_index_everywhere){
	uint8_t i;
	uint8_t j;

	for(i = 1; i <= 3; i++){
		for(j = 1; j <= 3; j++){
			Mat_write(matrix, i, j, i+j);
			TEST_ASSERT_EQUAL_DOUBLE(i+j, matrix->M[i-1][j-1]);
		}
	}
}

TEST(matrix_operations, Mat_write_should_ignore_index_bigger_3){
	TEST_IGNORE_MESSAGE(" i dunno how to check that this has been ignored");
}

TEST(matrix_operations, Mat_read_shoud_read_with_inceased_index){
	uint8_t i;
	uint8_t j;

	for(i = 1; i <= 3; i++){
		for(j = 1; j <= 3; j++){
			matrix->M[i-1][j-1] = j+i;
			TEST_ASSERT_EQUAL_DOUBLE(j+i, Mat_read(matrix, i, j));
		}
	}
}

TEST(matrix_operations, Mat_read_should_return_zero_if_index_bigger_3){
	TEST_ASSERT_EQUAL_DOUBLE(0.0, Mat_read(matrix, 4,4));
}

TEST(matrix_operations, Mat_Set_matrix_to_certain_value_should_work){
	uint8_t i;
	uint8_t j;
	for(i = 1; i <= 3; i++){
		for(j = 1; j <= 3; j++){
			matrix->M[i-1][j-1] = 1;
		}
	}
	Mat_set_all_values_to(matrix, 0.0);
	for(i = 1; i <= 3; i++){
		for(j = 1; j <= 3; j++){
			TEST_ASSERT_EQUAL_DOUBLE(0.0, matrix->M[i-1][j-1]);
		}
	}
}

TEST(matrix_operations, set_diag_to_value_should_work){
	uint8_t i;
	uint8_t j;
	for(i = 1; i <= 3; i++){
		for(j = 1; j <= 3; j++){
			matrix->M[i-1][j-1] = 1;
		}
	}
	Mat_set_diag_to(matrix, 0.0);
	for(i = 1; i <= 3; i++){
		TEST_ASSERT_EQUAL_DOUBLE(0.0, matrix->M[i-1][i-1]);
	}
}

TEST(matrix_operations, multiply_with_const_should_work){
	Mat_set_all_values_to(matrix, 1);
	Mat_mult_with_const(matrix, 5);
	uint8_t i;
	uint8_t j;
	for(i = 1; i <= 3; i++){
		for(j = 1; j <= 3; j++){
			TEST_ASSERT_EQUAL_DOUBLE(5, Mat_read(matrix, i,j));
		}
	}
}

TEST(matrix_operations, Add_to_matrix_should_do_as_said){
	matrix->M[0][0] = 5.2;
	Mat_add_to(matrix, 1,1, 1.2);
	TEST_ASSERT_EQUAL_DOUBLE(6.4, matrix->M[0][0]);
}

TEST(matrix_operations, Add_to_matrix_should_do_as_said2){
	matrix->M[1][1] = 5.2;
	Mat_add_to(matrix, 2, 2, 1.2);
	TEST_ASSERT_EQUAL_DOUBLE(6.4, matrix->M[1][1]);
}

TEST(matrix_operations, Mat_times_vector_should_use_mathematical_rule){
	Vector_t resultvect_container;
	Vector_t *resultvect = &resultvect_container;

	Mat_set_all_values_to(matrix, 0.0);
	Mat_write_arbitrary_values_into(matrix);

	Vect_write_three_values(vect, 1, 3, 5);

	Mat_times_vect(matrix, vect, resultvect);

	TEST_ASSERT_EQUAL_DOUBLE(22, Vect_read(resultvect, 1));
	TEST_ASSERT_EQUAL_DOUBLE(49, Vect_read(resultvect, 2));
	TEST_ASSERT_EQUAL_DOUBLE(76, Vect_read(resultvect, 3));
}

TEST(matrix_operations, Inverse_of_diagonal_matrix_should_be_one_over_diag_value){
	three_by_three_t inverse_matrix_container;
	three_by_three_t *inverse_matrix = &(inverse_matrix_container);
	three_by_three_t compare_matrix_matrix_container;
	three_by_three_t *compare_matrix = &(compare_matrix_matrix_container);

	Mat_set_all_values_to(inverse_matrix, 0.0);

	Mat_set_all_values_to(matrix, 0.0);
	Mat_set_diag_to(matrix, 5.0);
	print_matrix(matrix," Matrix ");

	Mat_set_all_values_to(compare_matrix, 0.0);
	Mat_set_diag_to(compare_matrix, 1.0/5.0);
	print_matrix(compare_matrix, " compare matrix ");

	Mat_inverse(matrix, inverse_matrix);

	print_matrix(inverse_matrix, "inverse matrix:");
	Test_Mat_equal(inverse_matrix, compare_matrix);
}

TEST(matrix_operations, Matrix_inverse_should_give_correct_results_with_zeroes_on_diagonal){
	three_by_three_t inverse_matrix_container;
	three_by_three_t *inverse_matrix = &(inverse_matrix_container);
	three_by_three_t compare_matrix_matrix_container;
	three_by_three_t *compare_matrix = &(compare_matrix_matrix_container);

	/*writing a matrix like:
	 * 	( 0 , 2 , 0 )
	 * 	( 0 , 0 , 2 )
	 * 	( 2 , 0 , 0 )
	 */
	Mat_set_all_values_to(matrix, 0.0);
	Mat_write(matrix, 1,2, 2.0);
	Mat_write(matrix, 2,3, 2.0);
	Mat_write(matrix, 3,1, 2.0);

	/*the inverse matrix to the other one is like this:
	 *  ( 0 , 0 ,1/2)
	 *  (1/2, 0 , 0 )
	 *  ( 0 ,1/2, 0 )
	 */
	Mat_set_all_values_to(compare_matrix, 0.0);
	Mat_write(compare_matrix, 1,3, 0.5);
	Mat_write(compare_matrix, 2,1, 0.5);
	Mat_write(compare_matrix, 3,2, 0.5);

	Mat_inverse(matrix, inverse_matrix);

	Test_Mat_equal(compare_matrix, inverse_matrix);
}

TEST(matrix_operations, Matrix_copy_should_work){
	three_by_three_t copy_to_matrix_container;
	three_by_three_t *copy_to_matrix = &copy_to_matrix_container;

	Mat_set_all_values_to(copy_to_matrix, 0.0);
	Mat_write_arbitrary_values_into(matrix);
	Mat_copy(matrix, copy_to_matrix);

	Test_Mat_equal(matrix, copy_to_matrix);
}

_STATIC_ void Mat_write_arbitrary_values_into(three_by_three_t *matrix){
	uint8_t i;
	uint8_t j;
	uint8_t k=1;

	for(i=1; i<=3; i++){
		for(j=1; j<=3; j++){
			Mat_write(matrix, i, j, k);
			k++;
		}
	}
}

_STATIC_ void Test_Mat_equal(three_by_three_t *matrix1, three_by_three_t *matrix2){
	uint8_t i;
	uint8_t j;
	double mat1_val;
	double mat2_val;
	char error_message[50];

	for(i=1; i<=3; i++){
		for(j=1; j<= 3; j++){
			mat1_val = Mat_read(matrix1, i, j);
			mat2_val = Mat_read(matrix2, i, j);
			sprintf(error_message, "at index (%d, %d), values: %f != %f",  i, j,mat1_val, mat2_val );
			TEST_ASSERT_EQUAL_DOUBLE_MESSAGE(
					mat1_val,
					mat2_val,
					error_message);
		}
	}
}

_STATIC_ void print_matrix(three_by_three_t *M, const char *name){
	uint8_t i;
	uint8_t j;

	printf(name);
	printf("\n{ \n");
	for(i=1; i<=3; i++){
		for(j=1; j<=3; j++){
			printf("%f \t", Mat_read(M, i, j));
		}
		printf("\n");
	}
	printf("} \n");
}
