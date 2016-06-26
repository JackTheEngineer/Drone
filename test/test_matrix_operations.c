/*
 * test_matrix_operations.c
 *
 *  Created on: May 13, 2016
 *      Author: jakov
 */

#include "test_helper.h"
#include "matrix_operations.h"



three_by_three_t moi;
three_by_three_t *matrix = &moi;
Vector_t vect_container;
Vector_t *vect = &vect_container; 

TEST_GROUP(matrix_operations);


TEST_SETUP(matrix_operations){
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
    Vect_set_all_values_to(vect, 0.0);
    
    Mat_times_vect(matrix, vect, resultvect);
    
    

}
