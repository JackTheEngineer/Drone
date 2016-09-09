/*
 * matrix_tester.h
 *
 *  Created on: Sep 8, 2016
 *      Author: chocolate
 */

#ifndef TEST_SUPPORT_MATRIX_TESTER_H_
#define TEST_SUPPORT_MATRIX_TESTER_H_

#include "physical_definitions.h"

void Test_Mat_equal(Matrix_t *matrix1, Matrix_t *matrix2);
void print_matrix(Matrix_t *M, const char *name);

#endif /* TEST_SUPPORT_MATRIX_TESTER_H_ */
