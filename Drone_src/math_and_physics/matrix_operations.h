/*
 * physical_helpers.h
 *
 *  Created on: May 13, 2016
 *      Author: jakov
 */

#ifndef MATRIX_OPERATIONS_H_
#define MATRIX_OPERATIONS_H_

#include "physical_definitions.h"

void Mat_write(Matrix_t *M, uint8_t i, uint8_t j, _FLOAT_ value);
_FLOAT_ Mat_read(Matrix_t *M, uint8_t i, uint8_t j);
void Mat_set_all_values_to(Matrix_t *M, _FLOAT_ value);
void Mat_set_diag_to(Matrix_t *M, _FLOAT_ value);
void Mat_times_const(Matrix_t *M, _FLOAT_ constant);
void Mat_add_to(Matrix_t *M, uint8_t i, uint8_t j, _FLOAT_ value);
void Mat_times_vect(Matrix_t *M, Vector_t *vect, Vector_t *resultvect);
void Mat_times_mat(Matrix_t *M1, Matrix_t *M2, Matrix_t *Result_M);
void Mat_inverse(Matrix_t *M, Matrix_t *Inverse);
void Mat_copy(Matrix_t *M_from, Matrix_t *M_to);
void Mat_multiply_line_a_by_x(Matrix_t *M, uint8_t a, _FLOAT_ x);
void Mat_transpose(Matrix_t *M, Matrix_t *result_M);

#endif /* MATRIX_OPERATIONS_H_ */
