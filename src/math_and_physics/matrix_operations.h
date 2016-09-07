/*
 * physical_helpers.h
 *
 *  Created on: May 13, 2016
 *      Author: jakov
 */

#ifndef MATRIX_OPERATIONS_H_
#define MATRIX_OPERATIONS_H_

#include "physical_definitions.h"

void Mat_write(three_by_three_t *M, uint8_t i, uint8_t j, double value);
double Mat_read(three_by_three_t *M, uint8_t i, uint8_t j);
void Mat_set_all_values_to(three_by_three_t *M, double value);
void Mat_set_diag_to(three_by_three_t *M, double value);
void Mat_times_const(three_by_three_t *M, double constant);
void Mat_add_to(three_by_three_t *M, uint8_t i, uint8_t j, double value);
void Mat_times_vect(three_by_three_t *M, Vector_t *vect, Vector_t *resultvect);
void Mat_times_mat(three_by_three_t *M1, three_by_three_t *M2, three_by_three_t *Result_M);
void Mat_inverse(three_by_three_t *M, three_by_three_t *Inverse);
void Mat_copy(three_by_three_t *M_from, three_by_three_t *M_to);
void Mat_multiply_line_a_by_x(three_by_three_t *M, uint8_t a, double x);


#endif /* MATRIX_OPERATIONS_H_ */
