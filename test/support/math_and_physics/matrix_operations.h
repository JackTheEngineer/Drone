/*
 * physical_helpers.h
 *
 *  Created on: May 13, 2016
 *      Author: jakov
 */

#ifndef TEST_SUPPORT_DRONE_SIMULATION_PHYSICAL_HELPERS_H_
#define TEST_SUPPORT_DRONE_SIMULATION_PHYSICAL_HELPERS_H_

#include "physical_definitions.h"

void Mat_set_M_zero(three_by_three_t *M);
void Mat_write(three_by_three_t *M, uint8_t i, uint8_t j, double value);
double Mat_read(three_by_three_t *M, uint8_t i, uint8_t j);
void Mat_set_all_values_to(three_by_three_t *M, double value);
void Mat_set_diag_to(three_by_three_t *M, double value);
void Mat_mult_with_const(three_by_three_t *M, double constant);
void Mat_add_to(three_by_three_t *M, uint8_t i, uint8_t j, double value);

#endif /* TEST_SUPPORT_DRONE_SIMULATION_PHYSICAL_HELPERS_H_ */
