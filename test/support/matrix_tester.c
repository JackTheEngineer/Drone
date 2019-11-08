/*
 * matrix_tester.c
 *
 *  Created on: Sep 8, 2016
 *      Author: chocolate
 */

#include "matrix_tester.h"
#include "matrix_operations.h"
#include "test_helper.h"

void Test_Mat_equal(Matrix_t *matrix1, Matrix_t *matrix2){
	uint8_t i;
	uint8_t j;
	_FLOAT_ mat1_val;
	_FLOAT_ mat2_val;
	char error_message[50];

	for(i=0; i < 3; i++){
		for(j=0; j < 3; j++){
			mat1_val = Mat_read(matrix1, i, j);
			mat2_val = Mat_read(matrix2, i, j);
			sprintf(error_message, "at index (%d, %d), values: %f != %f",  i, j, mat1_val, mat2_val );
			TEST_ASSERT_DOUBLE_WITHIN_MESSAGE(
							  0.000001,
							  mat1_val,
							  mat2_val,
							  error_message);
		}
	}
}

void print_matrix(Matrix_t *M, const char *name){
	uint8_t i;
	uint8_t j;

	printf("%s", name);
	printf("\n{ \n");
	for(i=0; i<3; i++){
		for(j=0; j<3; j++){
			printf("%f \t", Mat_read(M, i, j));
		}
		printf("\n");
	}
	printf("} \n");
}
